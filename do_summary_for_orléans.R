# 
# Purpose: To plot some DO stuff for Orléans presentation
# Author: Jake Diamond
# Date: October 1, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")

# Load libraries
library(lubridate)
library(readxl)
library(patchwork)
library(transformr)
library(gganimate)
library(tmap)
library(sf)
library(tidyverse)

# Load data
df_q <- read_excel("Headwaters/Data/Discharge/coise.xlsx") %>%
  filter(date >= ymd("2020-07-15")) %>%
  filter(date <= ymd("2020-08-25"))
df <- readRDS("Headwaters/Data/DO_time_series_wide") %>%
  filter(Site == "Coise le Nézel",
         datetime > ymd("2020-07-15")) %>%
  filter(datetime < ymd("2020-08-25")) %>%
  mutate(DOsat = ifelse(DO_temp == 0,
                        0,
                        14.652 - 0.41022 * DO_temp + 0.007991 * 
                          DO_temp^2 - 0.000077774 * DO_temp^3),
         DO_per = DO * 100/ DOsat)


do <- ggplot(data = df,
       aes(x = datetime, 
           y = DO))+
  geom_line() +
  theme_bw() + ylab("OD (mg/L)") +
  theme(axis.title.x = element_blank())

t <- ggplot(data = df,
       aes(x = datetime, 
           y = DO_temp))+
  geom_line(color = "red") +
  theme_bw() + ylab("T (degC)") +
  theme(axis.title.x = element_blank())
  

q <- ggplot(data = df_q,
       aes(x = date, 
           y = q * 1000))+
  geom_line(color = "blue") +
  theme_bw() + ylab("Q (L/s)") +
  theme(axis.title.x = element_blank())


(do / t / q) %>%
  ggsave(filename = "Headwaters/Figures/CoiseNezel_example.png",
         dpi = 600,
         height = 12,
         width = 18.4,
         units = "cm")


# Read in watershed info
ws_meta <- read_excel("Headwaters/Data/Field measurements/sensor_metadata.xlsx",
                      sheet = "lat_long_meta")


# Use threshold of daily temperature and %sat
df_filt <- readRDS("Headwaters/Data/DO_time_series_wide") %>%
  filter(datetime > ymd("2020-07-15")) %>%
  filter(datetime < ymd("2020-08-25")) %>%
  mutate(date = date(datetime)) %>%
  mutate(DOsat = ifelse(DO_temp == 0,
                        0,
                        14.652 - 0.41022 * DO_temp + 0.007991 * 
                          DO_temp^2 - 0.000077774 * DO_temp^3),
         DO_per = DO * 100/ DOsat) %>%
  group_by(Site,date) %>%
  summarize(do_mean = mean(DO_per, na.rm = TRUE),
            do_amp = max(DO_per, na.rm = T) - min(DO_per, na.rm = T),
            wat_t = max(DO_temp, na.rm = T) - min(DO_temp, na.rm = T)) %>%
  ungroup() %>%
  mutate(oow = if_else(((do_mean > 90 & wat_t > 5) |
                          (do_mean > 90 & do_amp < 2) |
                          (wat_t > 8) & !str_detect(Site, 'Lignon|Vizézy')), 
                       "yes", "no"))

# oow_sum <- df_filt %>%
#   group_by(Site) %>%
#   summarize(out = sum(oow == "yes", na.rm = TRUE),
#             total = n())
# 
# sd(oow_sum$out)
oow_map <- left_join(df_filt, ws_meta) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
oow_map <- st_transform(oow_map,
                        crs = 2154)

anim <- ggplot() +
  geom_sf(data = oow_map,#slice_sample(df_map, n = 5000),
          aes(color = oow)) +
  scale_color_manual(values = c("blue","red"),
                     breaks = c("no", "yes"),
                     labels = c("écoulement", "à sec")) +
  theme_bw() +
  # geom_sf(data = rivs, color = "blue", alpha = 0.5) +
  transition_states(date,
                    transition_length = 1,
                    state_length = 1) +
  ggtitle('{closest_state}',
          subtitle = 'Frame {frame} of {nframes}')
anim
anim_save(anim,
          filename = "loire_headwaters_drying.gif")
# tm_shape(filter(ws, Watershed == "Loise")) + tm_borders() + 
# tm_shape(filter(riv_hw, Watershed == "Loise")) +
#   tm_lines(lwd = "OSTRAHL", col = "blue",
#            legend.lwd.show = FALSE) +
#            

# 
# # Get river data
riv <- st_read("Data/GIS/tnet_out.shp")
riv <- st_set_crs(riv, 2154)
# riv <- st_transform(riv, 
                    # "+proj=longlat +init=epsg:4326")

# Read in Loire watershed files
ws <- st_read("Headwaters/Data/GIS/watershed_sites_Loire_new2020.shp")
ws <- st_set_crs(ws, 2154)
# ws <- st_transform(ws, 
                   # "+proj=longlat +init=epsg:4326")
st_crs(oow_map)
# Some data cleaning
ws <- mutate(ws,
             Site = str_remove_all(name_site, " -"),
             Site = str_remove_all(Site, "Pannissières"),
             Site = str_remove_all(Site, "moulin de Salt"),
             Site = str_replace(Site, "Donzy Salt", "Donzy"),
             Site = str_replace(Site, "Ruisseau du M", "M"),
             Site = str_replace(Site, "è", "é"),
             Site = str_replace(Site, "au Ne", "le Né"),
             Site = str_replace(Site, "aval Potensinet", "Larajasse"),
             Site = str_replace(Site, "aval le Rieu", "aval Rieu"),
             Site = str_replace(Site, "La Fontbonne amont Bessenay", "Fontbonne Bessenay"),
             Site = str_replace(Site, "La Fontbonne", "Fontbonne aval"),
             Site = str_replace(Site, "Le Carrat", "Carrat"),
             Site = str_replace(Site, "Le Coizet", "Coizet"),
             Site = str_replace(Site, "Le Violay", "Ruisseau de Violay"),
             Site = str_replace(Site, "Bruyere", "la Bruyere"),
             Site = str_replace(Site, "aval Doise", "aval Doise Salt"),
             Site = str_replace(Site, "aval Poncins", "amont Poncins"),
             Site = str_remove_all(Site, "La "),
             Site = str_replace(Site, "Aval", "aval"),
             Site = str_trim(Site)) %>%
  filter(!(Site %in% c("Lignon aval Lanzon", "L'Anzon", "Genetay", "Le Lignon amont l'Anzon",
                       "Coise aval Montrond", "Coise amont St Symphorien"))) %>%
  left_join(ws_meta) %>%
  group_by(Watershed) %>%
  filter(area_km2 == max(area_km2, na.rm = TRUE)) %>%
  ungroup()

riv_hw <- st_intersection(ws, riv)

# date dataframe
x = crossing(distinct(ws_meta, Watershed), distinct(ungroup(df_filt), date))

# Add dates
ws_d <- ws %>%
  right_join(x)

riv_hw_d <- riv_hw %>%
  right_join(x)

anim_tmap <- tm_shape(ws_d) + tm_borders() +
  tm_facets(by = "Watershed", along = "date", free.coords = TRUE) +
  tm_shape(riv_hw_d) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
  legend.lwd.show = FALSE) +
  tm_facets(by = "Watershed", along = "date", free.coords = TRUE) +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_shape(oow_map) + 
  tm_bubbles(col = "oow", 
             size = 0.4,
             palette = c("blue","red"),
             labels = c("écoulement", "à sec"),
             showNA = FALSE,
             title.col = "") +
  tm_facets(by = "Watershed", along = "date", free.coords = TRUE)
# anim_tmap
tmap_animation(anim_tmap, filename = "anim_tmap.gif",
               width = 6, height = 4,
               delay = 20)











df_sum <- readRDS("Headwaters/Data/DO_time_series_wide") %>%
  filter(datetime > ymd("2020-07-15")) %>%
  filter(datetime < ymd("2020-08-25")) %>%
  mutate(date = date(datetime)) %>%
  left_join(df_filt) %>%
  filter(oow == "no",
         DO > 2) %>%
  group_by(side, Watershed, Subwatershed, Site, area_km2, date) %>%
  summarize(
            min_DO = min(DO, na.rm = TRUE),
            max_T = max(DO_temp, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(cols = -c(1:6), names_to = "msmt", values_to = "val") %>%
  arrange(Watershed, area_km2) %>%
  mutate(Site = fct_inorder(Site))


# plot
p_do_sum <- ggplot(data = filter(df_sum, 
                            !is.na(side)),
              aes(x = Site,
                  y = val,
                  color = Watershed,
                  shape = msmt)) +
  stat_summary(fun = mean, geom = "point") + 
  scale_shape_manual(values = c(1, 16)) +
  guides(shape = FALSE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3) +
  facet_grid(msmt~side, scales = "free", switch = "y", labeller = label_parsed) +
  scale_color_brewer(name = "BV", type = "qual", palette = "Dark2") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        # strip.text = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        legend.direction = "horizontal")
p_do_sum

gt = ggplot_gtable(ggplot_build(p_do_sum))
# gtable_show_layout(gt)
gt$widths[7] = 0.5*gt$widths[7]
# grid.draw(gt)
ggsave(filename = "Headwaters/Figures/do_summary_late_summer.png",
       plot = gt,
       device = "png",
       dpi = 600,
       width = 184,
       height = 120,
       units = "mm")
  
  
  
  
  
  
  
