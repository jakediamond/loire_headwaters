# 
# Purpose: To summarize DO data for Loire headwaters
# Author: Jake Diamond
# Date: October 3, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(tidytext)
library(readxl)
library(tidyverse)

# Load data and calculate percent saturation
df_DO <- readRDS("Headwaters/Data/DO_time_series_wide") %>%
  mutate(DOsat = if_else(DO_temp == 0,
                        0,
                        14.652 - 0.41022 * DO_temp + 0.007991 * 
                          DO_temp^2 - 0.000077774 * DO_temp^3),
         DO_per = DO * 100/ DOsat)

# Get rid of bad data when sensors were buried or when out of water
flood_sites1 <- c("Loise Feurs", "Mare Pont du diable", "Mare aval",
                 "Vizézy amont Bullieux", "Toranche aval", "Mare Azieux")
flood_sites2 <- c("Loise aval Poncins", "Loise Essertine en Donzy")
df_DO <- df_DO %>%
  filter(!(Site %in% flood_sites1 & between(datetime, 
                                            ymd_hm("2019-08-06 16:00"),
                                            ymd_hm("2019-08-29 16:45"))),
         !(Site %in% flood_sites2 & between(datetime, 
                                            ymd_hm("2019-08-06 16:00"),
                                            ymd_hm("2019-08-30 10:30"))),
         !(Site == "Coise aval Montrond" & between(datetime, 
                                                   ymd_hm("2019-08-06 16:00"),
                                                   ymd_hm("2019-09-11 11:30"))),
         !(Site == "Loise amont Doise Salt" & between(datetime, 
                                                   ymd_hm("2019-07-16 00:30"),
                                                   ymd_hm("2019-07-20 20:00"))),
         !(Site == "Loise aval Doise Salt" & between(datetime, 
                                                      ymd_hm("2019-08-06 19:00"),
                                                      ymd_hm("2019-08-30 10:15"))),
         !(Site == "Loise Essertine en Donzy" & between(datetime, 
                                                   ymd_hm("2019-07-20 22:15"),
                                                   ymd_hm("2019-07-23 15:30"))),
         !(Site == "Vizézy amont Bullieux" & between(datetime, 
                                                   ymd_hm("2019-07-08 19:00"),
                                                   ymd_hm("2019-07-16 14:15"))),
         !(Site == "Vizézy amont Bullieux" & between(datetime, 
                                                   ymd_hm("2019-07-24 23:00"),
                                                   ymd_hm("2019-08-29 15:30"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-12 19:45"),
                                               ymd_hm("2019-07-21 17:30"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-23 22:45"),
                                               ymd_hm("2019-07-29 01:00"))),
         !(Site == "Toranche Pontet" & between(datetime, 
                                               ymd_hm("2019-07-31 03:45"),
                                               ymd_hm("2019-08-06 17:30"))),
         !(Site == "Doise" & between(datetime, 
                                     ymd_hm("2019-07-14 15:00"),
                                     ymd_hm("2019-07-20 17:15"))),
         !(Site == "Doise" & between(datetime, 
                                     ymd_hm("2019-08-03 20:15"),
                                     ymd_hm("2019-08-06 12:30")))
         
         )

# Use threshold of daily temperature and %sat
df_DO_sum <- df_DO %>%
  mutate(date = date(datetime)) %>%
  group_by(Site,date) %>%
  summarize(do_mean = mean(DO_per, na.rm = TRUE),
            do_amp = max(DO_per, na.rm = T) - min(DO_per, na.rm = T),
            wat_t = max(DO_temp, na.rm = T) - min(DO_temp, na.rm = T)) %>%
  mutate(oow = if_else((do_mean > 90 & wat_t > 5) |
                         (do_mean > 90 & do_amp < 2) |
                         (wat_t > 8), "yes", "no"))


df_DO <- df_DO %>%
  mutate(date = date(datetime)) %>%
  left_join(df_DO_sum) %>%
  filter(oow == "no")

# data_send <- df_DO %>%
#   filter(Site %in% c("Toranche Pontet", "Toranche aval", "Toranche St Cyr les Vignes"),
#          year(datetime) == 2019) %>%
#   select(-contains("shed"), -contains("lux"), -Location)
# write_excel_csv2(data_send, path = "Headwaters/Data/toranche_oxygen.csv")
# 

# add a column for west/east
side <- tibble(watershed = c("Coise", "Loise", "Toranche", "Lignon", "Mare"),
               side = c("east", "east", "east", "west", "west"))

# Read in watershed info
ws_meta <- read_excel("Headwaters/Data/Field measurements/sensor_metadata.xlsx",
                      sheet = 7)

# Calculate some daily metrics
df_DO_d <- df_DO %>%
  mutate(year = year(date)) %>%
  # filter(year(date) == 2020) %>%
  # dplyr::filter(DO > 1) %>%
  filter(between(month(date), 5, 9)) %>%
  group_by(year, Subwatershed, Watershed, Site, date) %>%
  summarize(mean_DO = mean(DO, na.rm = TRUE),
            max_DO = max(DO, na.rm = TRUE),
            min_DO = min(DO, na.rm = TRUE),
            amp_DO = max_DO - min_DO,
            mean_per = mean(DO_per, na.rm = TRUE),
            max_per = max(DO_per, na.rm = TRUE),
            min_per = min(DO_per, na.rm = TRUE),
            amp_per = max_per - min_per,
            mean_T = mean(DO_temp, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(cols = -c(1:5), names_to = "msmt", values_to = "val") %>%
  left_join(select(ws_meta, site = name_site, watershed, area,
                   Site = site_sensor, site_chem, site_code)) %>%
  arrange(Watershed, area) %>%
  mutate(site_code = fct_inorder(site_code),
         Site = fct_inorder(Site))

df_sum <- df_DO_d %>%
  # filter(amp < 4) %>%
  group_by(Site, Subwatershed, Watershed, msmt) %>%
  summarize(value = median(val, na.rm = T))

# Save to disc
saveRDS(df_sum, "Headwaters/Data/DO_summary")

# # site areas
# areas <- select(ws, Site = site_sensor, surf_km2)
# st_geometry(areas) <- NULL

# Combine those
df_sum <- df_sum %>%
  # left_join(areas) %>%
  pivot_longer(cols = -c(1:3), names_to = "msmt", values_to = "val")

# New names for facets
fac_labs <- tibble(msmt = unique(df_sum$msmt),
                   fac = c("DO moy. (mg/L)",
                           "DO max. (mg/L)",
                           "DO min. (mg/L)",
                           "DO amp. (mg/L)",
                           "DO moy. (%)",
                           "DO max. (%)",
                           "DO min. (%)",
                           "DO amp. (%)",
                           "T moy. (deg C)"))

# Combine those
df_sum_p <- left_join(side, select(ws_meta, site = name_site, watershed, area,
                                   site_sensor, site_chem, site_code)) %>%
  right_join(rename(df_sum, site_sensor = Site)) %>%
  # mutate(site_code = str_c(str_sub(site_chem, 1, 3),
  #                          str_pad(round(area), 3, pad = "0"))) %>%
  mutate(side = fct_rev(side)) %>%
  arrange(watershed, area) %>%
  mutate(site_code = fct_inorder(site_code),
         site = fct_inorder(site))
  
  
  
  #left_join(df_sum, fac_labs)

# Reorder for plotting
# df_sum_p <- df_sum_p %>%
#   ungroup() %>%
#   mutate(fac = as.factor(fac),
#          site = reorder_within(Site, val, fac))
# 
# # Combine all
# chem <- left_join(side, select(ws_meta, site = name_site, watershed, area,
#                                site_sensor, site_chem)) %>%
#   right_join(rename(chem, site_chem = site)) %>%
#   left_join(types) %>%
#   mutate(site_code = str_c(str_sub(site_chem, 1, 3),
#                            str_pad(round(area), 3, pad = "0"))) %>%
#   mutate(side = fct_rev(side)) %>%
#   arrange(watershed, area) %>%
#   mutate(site_code = fct_inorder(site_code))

# Quick look
p_do <- df_DO_d %>%
  left_join(fac_labs) %>%
  filter(msmt %in% c("amp_per", "max_per", "min_per", "mean_T")) %>%
  ggplot(aes(x = Site, 
             y = val, color = Watershed)) +
  stat_summary(fun = mean, geom = "point", alpha = 0.7) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", show.legend = FALSE) +
  facet_grid(~fac, scales = "free", switch = "y") +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  coord_flip() +
  scale_x_reordered() +
  xlab("") + ylab("") +
  theme_bw() +
  theme(panel.grid = element_blank(),
    # axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

p_do


library(gtable)
gt = ggplot_gtable(ggplot_build(p_n))
# gtable_show_layout(gt)
gt$widths[7] = 0.5*gt$widths[7]


ggsave(plot = p_do,
       filename = "Headwaters/Figures/DO_summary.png",
       dpi = 600,
       width = 18.4,
       height = 12,
       units = "cm")
