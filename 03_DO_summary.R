# 
# Purpose: To summarize DO data for Loire headwaters
# Author: Jake Diamond
# Date: October 3, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
setwd("C:/Users/diamo/Dropbox/Projects/Loire_headwaters")

# Load libraries
library(lubridate)
library(tidytext)
library(readxl)
library(tidyverse)

# Load data and calculate percent saturation
df_DO <- readRDS("Headwaters/Data/headwaters_data_clean")  
unique(df_DO$site_code)
# Only look at data from the beginning of 2020 sampling to first rain event
df_DO_spring <- df_DO %>%
  mutate(date = date(datetime)) %>%
  filter(between(date, ymd("2020-03-01"), ymd("2020-04-18")))

# Take a quick look
ggplot(data = filter(df_DO_spring, site_code =="tor063"),
       aes(x = datetime,
           y = DO_per,
           color = site_code)) +
  geom_line() +
  facet_wrap(~Subwatershed) + 
  theme_bw()

# Need to remove a few points, still not completely clean
df <- df_DO_spring %>%
  filter(site_code == "fon003" & datetime == "2020-04-13 15:00:00") %>%
  mutate_if(is.numeric, ~NA_real_) %>%
  bind_rows(df_DO_spring %>%
              filter(!(site_code == "fon003" & datetime == "2020-04-13 15:00:00"))) %>%
  filter(!(site_code == "car004" & date >= ymd("2020-03-28"))) %>%
  mutate(area = as.numeric(str_sub(site_code, 4,6))) %>%
  arrange(Subwatershed, area, datetime)

# Take a quick look
ggplot(data = df,
       aes(x = datetime,
           y = DO_per,
           color = area,
           group = site_code)) +
  geom_line() +
  scale_color_viridis_c() +
  facet_wrap(~Subwatershed) + 
  theme_bw() +
  labs(y = "DO sat. (%)",
       x = "",
       title = "3 March – 18 April")
ggsave(filename = "Headwaters/Figures/spring_timeseries.png",
       dpi = 600,
       width = 36,
       height = 24,
       units = "cm")

# summarize by day
df_sum <- df %>%
  group_by(Subwatershed, site_code, Site, area, date) %>%
  summarize(across(where(is.numeric), 
                   list(mean = mean, max = max, min = min)))

# Take a quick look at lux vs DO
ggplot(data = df_sum,
       aes(x = lux_max,
           y = DO_per_max,
           color = area,
           group = site_code)) +
  geom_point() +
  scale_color_viridis_c() +
  # facet_wrap(~Subwatershed) + 
  theme_bw()

# Take a quick look at the summary data for DO
df_sum %>%
  select(starts_with("DO")) %>%
  mutate(DO_amp = DO_max - DO_min,
         DO_per_amp = DO_per_max - DO_per_min,
         DO_temp_amp = DO_temp_max - DO_temp_min) %>%
  select(-ends_with("mean")) %>%
  pivot_longer(cols = starts_with("DO")) %>%
  drop_na() %>%
  ungroup() %>%
  # group_by(Subwatershed, site_code, Site, area)
  ggplot(aes(x = site_code,
             y = value,
             color = Subwatershed)) +
  stat_summary() +
  theme_bw() +
  # scale_color_brewer(type = "qual") +
  facet_wrap(~name, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "",
       title = "Mean daily summary for March–April")
ggsave(filename = "Headwaters/Figures/mean_daily_summary_spring.png",
       dpi = 600,
       width = 36,
       height = 24,
       units = "cm")

# Only look at data in summer
df_DO_summer <- df_DO %>%
  mutate(date = date(datetime)) %>%
  filter(between(date, ymd("2020-06-09"), ymd("2020-06-23"))) %>%
  mutate(area = as.numeric(str_sub(site_code, 4,6))) %>%
  arrange(Subwatershed, area, datetime)

# Take a quick look
ggplot(data = filter(df_DO_summer, site_code == "tor063"),
       aes(x = datetime,
           y = DO_per,
           color = area,
           group = site_code)) +
  geom_line() +
  scale_color_viridis_c() +
  facet_wrap(~Subwatershed) + 
  theme_bw() +
  labs(y = "DO sat. (%)",
       x = "",
       title = "9 June – 1 July")
ggsave(filename = "Headwaters/Figures/summer_timeseries.png",
       dpi = 600,
       width = 36,
       height = 24,
       units = "cm")

# Look at week before leaf on and one week after in early season
df_DO_leaf <- df_DO %>%
  mutate(date = date(datetime)) %>%
  filter((between(date, ymd("2020-04-12"), ymd("2020-04-18")) |
            between(date, ymd("2020-05-04"), ymd("2020-05-12"))) 
         )%>%
  mutate(period = if_else(between(date, ymd("2020-04-12"), ymd("2020-04-18")),
                          "leaf_off",
                          "leaf_on")) %>%
  group_by(Subwatershed) %>%
  mutate(rank = dense_rank(site_code)) %>%
  ungroup()

# Need to remove a few points, still not completely clean
df_DO_leaf <- df_DO_leaf %>%
  filter(site_code == "fon003" & datetime == "2020-04-13 15:00:00") %>%
  mutate_if(is.numeric, ~NA_real_) %>%
  bind_rows(df_DO_leaf %>%
              filter(!(site_code == "fon003" & datetime == "2020-04-13 15:00:00"))) %>%
  mutate(area = as.numeric(str_sub(site_code, 4,6))) %>%
  arrange(Subwatershed, area, datetime) %>%
  drop_na(DO)

# Take a quick look
ggplot(data = df_DO_leaf,
       aes(x = datetime,
           y = DO_per,
           linetype = as.factor(rank),
           color = area)
       ) +
  geom_line(size = 1.5, alpha = 0.7) +
  scale_color_viridis_c()+
  scale_linetype_discrete("site order") +
  facet_grid(Subwatershed~period, scales = "free") + 
  theme_bw() +
  labs(y = "DO sat. (%)",
       x = "",
       title = "Spring DO, before and after leaf off")

ggsave(filename = "Headwaters/Figures/spring_timeseries_leaf_onoff_comparison.png",
       dpi = 600,
       width = 36,
       height = 24,
       units = "cm")

# summarize by day
df_sum_leaf <- df_DO_leaf %>%
  group_by(Subwatershed, site_code, Site, area, date, rank, period) %>%
  summarize(across(where(is.numeric), 
                   list(mean = mean, max = max, min = min)))

# Take a quick look at the summary data for DO
df_sum_leaf %>%
  select(period, starts_with("DO")) %>%
  mutate(DO_amp = DO_max - DO_min,
         DO_per_amp = DO_per_max - DO_per_min,
         DO_temp_amp = DO_temp_max - DO_temp_min) %>%
  select(-ends_with("mean")) %>%
  pivot_longer(cols = starts_with("DO")) %>%
  ungroup() %>%
  group_by(Subwatershed, site_code, Site, area, rank, name, period) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = period, values_from = value) %>%
  # drop_na() %>%
  ungroup() %>%
  mutate(delta = leaf_off - leaf_on) %>%
  left_join(rename(df_k, Site = site)) %>%
  # group_by(Subwatershed, site_code, Site, area)
  ggplot(aes(x = width_m,#reorder_within(site_code, delta, name),
             y = delta,
             color = area)) +
  geom_point() +
  theme_bw() +
  # scale_x_reordered() +
  # coord_flip() +
  scale_color_viridis_c() +
  # scale_color_brewer(type = "qual") +
  facet_wrap(~name, scales = "free") +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "difference in mean value",
       x = "",
       title = "Difference between leaf off and leaf on")
ggsave(filename = "Headwaters/Figures/mean_daily_summary_spring_leaf_compare.png",
       dpi = 600,
       width = 24,
       height = 24,
       units = "cm")



































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
