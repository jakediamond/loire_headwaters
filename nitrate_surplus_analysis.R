# -------------------------------------
# Author: 
# Purpose: 
# Date:
# -------------------------------------
library(tidyverse)
library(sf)
# Read in Loire watershed files
ws <- st_read("Headwaters/Data/GIS/watershed_sites_Loire_new2020.shp")
ws <- st_set_crs(ws, 2154)

# Read in watershed info
ws_meta <- read_excel("Headwaters/Data/Field measurements/sensor_metadata.xlsx",
                      sheet = "landscape")

# Join that info to the ws file
ws <- ws %>%
  arrange(name_site) %>%
  bind_cols(arrange(drop_na(ws_meta, name_site), name_site) %>%
              select(-name_site))

# Read in data for N surplus
nsur <- read_csv("Headwaters/Data/watershed_sites_Loire_new2020_surplus.csv")

# Clean data
colnames(nsur) <- word(colnames(nsur), 1)
nsur_l <- pivot_longer(nsur, -Annee, names_to = "YL93", values_to = "nsur") %>%
  mutate(YL93 = as.numeric(YL93)) %>%
  rename(year = Annee)

# Combine with ws info
df_ws <- st_set_geometry(ws, NULL)
df <- select(df_ws, site, site_code, watershed, subwatershed,
             YL93, area_km2 = surf_km2) %>%
  right_join(nsur_l) %>%
  mutate(nsur = nsur / (area_km2 * 100))

# Quick time series
(ggplot(df,
       aes(x = year,
           y = nsur,
           color = area_km2,
           group = site_code)) +
  # geom_point() +
  geom_line() +
  scale_color_viridis_c() +
  theme_bw() +
  scale_y_log10() +
  facet_wrap(~subwatershed) +
  ylab("N surplus (kg/ha/yr)")) %>%
  ggsave(filename = "Headwaters/Figures/nsurplus_timeseries.png",
         device = "png",
         dpi = 600,
         width = 184,
         height = 120,
         units = "mm")

  
# Compare to nitrate values
# Add chemistry data to points
chem <- read_excel("Headwaters/Data/Loire_headwaters_data_share.xlsx",
                   sheet = "water_chemistry") %>%
  select(-`sample bottle`) %>%
  group_by(site) %>%
  mutate_if(is.character, parse_number) %>%  #convert LDL to the DL
  ungroup() %>%
  pivot_longer(-c(datetime, site)) %>%
  left_join(read_excel("Headwaters/Data/water_chemistry/detection_limits.xlsx")) %>%
  mutate(value = if_else(value < dl, value * 0.5, value)) %>% #take half of detection limit
  pivot_wider(id_cols = c("site", "datetime"), names_from = name)

# Get data into as P or as N, already done for february 2020 samples!!
chem <- chem %>%
  filter(month(datetime) != 2) %>%
  mutate(NH4 = 14.01 * NH4 / 18.04,
         NO2 = 14.01 * NO2 / 46.01,
         NO3 = 14.01 * NO3 / 62,
         PO4 = 30.97 * PO4 / 94.97) %>%
  bind_rows(filter(chem, month(datetime) == 2))

chem_sum <- filter(chem, site != "Charpassonne spring",
                    (month(datetime) %in% c(7,8))) %>%
  ungroup() %>%
  select(-datetime) %>%
  group_by(site) %>%
  summarize_all(mean, na.rm = TRUE)

# Plot nitrate vs most recent N surplus 
(select(chem_wint, site, NO3) %>%
  right_join(df %>%
               filter(year > 2005) %>%
               group_by(site, watershed, subwatershed, area_km2) %>%
               summarize(sur = mean(nsur, na.rm = T)) %>%
               ungroup()
             ) %>%
  ggplot(aes(x = sur, y = NO3,
             color = watershed,
             shape = watershed,
             size = area_km2)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2", type = "qual") +
  # scale_x_log10() +
  theme_bw() +
  labs(title = "Mean summer nitrate vs. mean N surplus",
       y = "mean summer nitrate (mg/L)",
       x = "mean annual N surplus since 2005 (kg/ha)")) %>%
  ggsave(filename = "Headwaters/Figures/nitrate_vs_nsurplus_summer.png",
         device = "png",
         dpi = 600,
         width = 184,
         height = 120,
         units = "mm")



df_r <- select(chem_wint, site, NO3) %>%
  mutate(rank = row_number(NO3)) %>%
  right_join(df %>%
               filter(year > 2005) %>%
               group_by(site, watershed, subwatershed, area_km2) %>%
               summarize(sur = mean(nsur, na.rm = T)) %>%
               ungroup() %>%
               mutate(rank_sur = row_number(sur))) %>%
  mutate(ws = if_else(subwatershed == "Charpassonne",
                      "Charpassonne",
                      watershed))
(ggplot(data= df_r,
       aes(x = rank_sur,
           y = rank,
           color = watershed,
           shape = watershed,
           size = area_km2,
           group = watershed)) +
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", type = "qual") +
  stat_smooth(method = "lm", se = FALSE) +
  ylab("Ranking of summer nitrate") +
  xlab("Ranking of nitrogen surplus") +
  ggtitle("Ranking of summer nitrate vs nitrogen surplus")) %>%
  ggsave(filename = "Headwaters/Figures/nitrate_ranking_surplus_comparison_summer.png",
         device = "png",
         dpi = 600,
         width = 18.4,
         height = 18.4,
         units = "cm")



buff <- readRDS("Headwaters/Data/buffer_land_use")
# Clean up dataframe
meta <- tibble(reGROUP = c(11, 12, 21, 22, 31, 32, 33, 40, 50),
               landuse = c("territoire artificialisé dense",
                           "territoire artificialisé discontinu",
                           "agricole intensif",
                           "agricole faible impact",
                           "forest",
                           "pelouses, landes, friches",
                           "espaces ourverts",
                           "zones humides",
                           "surfaces en eaux"))
y= left_join(buff, meta) %>%
  left_join(select(chem_sum, site, DOC = SO4) %>%
              mutate(rank = row_number(DOC),
                     site = tolower(site))) %>%
  left_join(mutate(df_ws, site = tolower(site))) %>%
  filter(reGROUP %in% c(21, 22, 31),
         !is.na(watershed)) %>%
  ggplot(aes(x = area_lu_frac,
             y = DOC,
             color = watershed,
             shape = watershed)) +
  theme_bw() +
  geom_point() + 
  scale_color_brewer(palette = "Dark2", type = "qual") +
  facet_grid(buff_dists~landuse) +
  ylab("Winter DOC (mg/L)") +
  xlab("land use fraction") +
  ggtitle("Winter DOC vs land use fractions in varying buffer sizes")
y


ggsave(plot = y,
       filename = "Headwaters/Figures/winter_DOC_buffer_landuse.png",
       device = "png",
       dpi = 600,
       width = 18.4,
       height = 18.4,
       units = "cm")

select(chem_wint, site, NO3) %>%
  left_join(df_ws) %>%
  group_by(watershed) %>%
  mutate(rank = row_number(NO3)) %>%
  filter(watershed %in% c("Loise", "Coise", "Toranche")) %>%
  filter(!(subwatershed %in% c("Carrat", "Coizet", "Doise", "Potensinet", "Rieu", "Violay"))) %>%
  ggplot(aes(x = surf_km2,
             y = NO3,
             color = subwatershed,
             group = interaction(watershed, subwatershed))) +
  geom_point() +
  geom_line() +
  # stat_smooth(se = FALSE) +
  # geom_label(aes(label = site)) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  facet_wrap(~watershed, scales = "free", nrow = 3) +
  theme_bw()
