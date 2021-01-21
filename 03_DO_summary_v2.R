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
library(fuzzyjoin)
library(streamMetabolizer)
library(tidyverse)

# Load DO data
df_DO <- readRDS("Headwaters/Data/headwaters_data_clean") %>%
  mutate(date = date(datetime),
         month = month(date))

# Load geometry data
df_k <- read_xlsx("Headwaters/Data/hydraulic_data.xlsx") %>%
  select(Site = site, width = width_m, depth = depth_m, slope = slope_tnet)

# Load fuzzy join dates data which specifies the different periods for each site
df_fj <- read_xlsx("Headwaters/Data/DO_dates.xlsx") %>%
  select(-notes) %>%
  pivot_longer(cols = -site_code,
               names_to = c("period", "position"),
               names_sep = "_") %>%
  pivot_wider(names_from = position,
              values_from = value) %>%
  drop_na()

# Join the period data to the DO data (next time probably should subset to just date
# then join that back to original data...much faster)
df <- df_DO %>%
  fuzzy_left_join(df_fj,
                  by = c("site_code" = "site_code",
                         "date" = "start",
                         "date" = "end"),
                  match_fun = list(`==`, `>=`, `<=`))

# Fix duplicate name
df <- df %>% select(-site_code.y) %>%
  rename(site_code = site_code.x)

# Need to remove a few points, still not completely clean
df <- df %>%
  filter(site_code == "fon003" & datetime == "2020-04-13 15:00:00") %>%
  mutate_if(is.numeric, ~NA_real_) %>%
  bind_rows(df %>%
              filter(!(site_code == "fon003" & datetime == "2020-04-13 15:00:00"))) %>%
  mutate(area = as.numeric(str_sub(site_code, 4,6))) %>%
  arrange(Subwatershed, area, datetime)

# Save data
saveRDS(df, "clean_data_with_periods")
df <-readRDS("clean_data_with_periods")

# Also add the geometry data
df <- left_join(df, df_k)

# Get a meta_data for ease of use later
df_meta <- select(df, Site, site_code, Subwatershed, area, slope, width, depth) %>%
  distinct()

# Read in watershed info
ws_meta <- read_excel("Headwaters/Data/Loire_headwaters_data_share.xlsx",
                      sheet = "watershed_meta") %>%
  rename(Site = site)

# Overall metadata
meta <- left_join(df_meta, ws_meta)

# Organize by solar day
df_solar <- df %>%
  left_join(select(meta, Site, longitude, watershed)) %>%
  mutate(solartime = calc_solar_time(datetime, longitude),
         newtime = solartime - hours(3)) %>% #start at 4 in the morning
  mutate(date = date(newtime))

# summarize by day
df_sum <- df_solar %>%
  group_by_at(vars(-c(solartime, newtime, datetime, DO:q_mmd))) %>%
  summarize(across(where(is.numeric), 
                   list(mean = mean, max = max, min = min)))

df_sum %>%
  mutate(amp = DO_per_max - DO_per_min) %>%
  filter(oow == "no") %>%
  # filter(period %in% c("leafoff", "leafon", "summer")) %>%
  filter(between(month, 6, 8)) %>%
  select(DO_per_max, DO_per_min) %>%
  pivot_longer(cols = c(DO_per_max, DO_per_min)) %>%
  left_join(df_sum %>% select(DO_temp_min) %>% filter(between(month, 6, 8)) %>%
              ungroup() %>% group_by(site_code) %>% 
              summarize(temp = mean(DO_temp_min, na.rm = TRUE))) %>%
  ggplot(aes(x = area,
           y = value,
           shape = name,
           fill = temp)) +
  stat_summary() +
  theme_bw() +
  scale_fill_viridis_c() + 
  # facet_wrap(~period, scales = "free_y") +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 4),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                limits = c(1, 1000)) +
  annotation_logticks(sides = "b") +
  geom_hline(yintercept = 100, linetype = "dashed") +
  scale_shape_manual(name = "daily summer DO",
                    values = c(21, 24),
                    labels = c("max.", "min.")) +
  labs(x = "width (m)",
       y = expression("DO saturation (%)")) + 
  theme(legend.position = c(0.25, 0.85),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill=alpha(0.01)),
        legend.key = element_rect(fill=alpha(0.01)))
ggsave(filename = "Headwaters/Figures/DO_summaries/dailyDO_summer_max_min_by_width.png",
       dpi = 600,
       width = 9.2,
       height = 9.2,
       units = "cm")

# Compare leaf off and leaf on periods
df_DO_sum <- df_sum %>%
  select(starts_with("DO")) %>%
  mutate(DO_amp = DO_max - DO_min,
         DO_per_amp = DO_per_max - DO_per_min,
         DO_temp_amp = DO_temp_max - DO_temp_min) %>%
  select(-ends_with("mean")) %>%
  pivot_longer(cols = starts_with("DO")) %>%
  drop_na() %>%
  ungroup() %>%
  group_by(site_code, name, period) %>%
  summarize(median = median(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = period, values_from = c(median, sd)) %>%
  ungroup() %>%
  mutate(del = median_leafon - median_leafoff,
         del_sd = sqrt(sd_leafoff^2 + sd_leafon^2),
         per = ((median_leafon - median_leafoff)/ median_leafoff) * 100,
         per_sd = sqrt((sd_leafon/median_leafon)^2 + (sd_leafoff/median_leafoff)^2)
         ) %>%
  left_join(df_meta)

# Plot this
df_DO_sum %>%
  drop_na(del) %>%
  ggplot(data = .,
       aes(x = area,
           y = del,
           color = width)) +
  geom_point() +
  geom_errorbar(aes(ymin = del - del_sd,
                    ymax = del + del_sd),
                width = 0.01) +
  theme_bw() +
  scale_color_viridis_c(name = "width (m)") +
  scale_x_log10() +
  # scale_color_brewer(type = "qual") +
  facet_wrap(~name, scales = "free") +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "(leaf_on - leaf_off) (units depend)",
       x = "area (km2)",
       title = "absolute difference between leaf off and leaf on")
ggsave(filename = "Headwaters/Figures/abs_change_leaf_compare.png",
       dpi = 600,
       width = 36,
       height = 24,
       units = "cm")

# Plot summertime differences
df_DO_sum %>%
  drop_na(median_summer) %>%
  mutate(delsum = median_summer - median_leafon,
         delsum_sd = sqrt(sd_summer^2 + sd_leafon^2),
         persum = ((median_summer - median_leafon)/ median_leafon) * 100,
         persum_sd = sqrt((sd_leafon/median_leafon)^2 + (sd_summer/median_summer)^2)
  ) %>%
  ggplot(data = .,
         aes(x = area,
             y = median_summer,
             color = width)) +
  geom_point() +
  geom_errorbar(aes(ymin = median_summer - sd_summer,
                    ymax = median_summer + sd_summer),
                width = 0.01) +
  theme_bw() +
  scale_color_viridis_c(name = "width (m)") +
  scale_x_log10() +
  # scale_color_brewer(type = "qual") +
  facet_wrap(~name, scales = "free") +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "mean value",
       x = "area (km2)",
       title = "Summertime values")
ggsave(filename = "Headwaters/Figures/summer_time_values.png",
       dpi = 600,
       width = 36,
       height = 24,
       units = "cm")

# Timing of min and max by day
df_time <- df %>%
  select(site_code, date, datetime, DO_per, period) %>%
  group_by_at(vars(-c(DO_per, datetime))) %>%
  filter((DO_per == max(DO_per) |
            DO_per == min(DO_per))) %>%
  mutate(hour = hour(datetime),
         type = if_else(DO_per == max(DO_per),
                        "max",
                        "min"))

x = df_time %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(site_code, month, year, type) %>%
  summarize(hour = mean(hour)) %>%
  left_join(df_meta) %>%
  filter(year == 2020,
         area <190) %>%
  ggplot(.,
         aes(x = month,
             y = hour,
             color = area,
             group = site_code)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_color_viridis_c() +
  facet_grid(type~Subwatershed, scales = "free")
x












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
