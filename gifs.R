# 
# Purpose: To animate Loire headwater DO time series
# Author: Jake Diamond
# Date: September 24, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(readxl)
library(ggmap)
library(scales)
library(grid)
library(sf)
library(ggrepel)
library(tidyverse)
library(tmap)








# Animation maps ----------------------------------------------------------
df_test <- filter(df_ts_sf,
                  Watershed == "Loise",
                  between(datetime, 
                          ymd_hms("2019-09-01 00:00:00"),
                          ymd_hms("2019-09-11 00:00:00"))) %>%
  group_by(Site, datetime = cut(datetime, breaks = "1 hours")) %>%
  summarize(DO_per = mean(DO_per, na.rm = TRUE)) %>%
  arrange(Site, datetime) %>%
  mutate(hour = hour(datetime),
         hour2 = ifelse(hour>12,
                        hour - 2 * (hour - 12),
                        hour))


anim_test <- tm_shape(filter(ws, Watershed == "Loise")) + tm_borders() + 
  tm_shape(filter(riv_hw, Watershed == "Loise")) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
           legend.lwd.show = FALSE) +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_shape(df_test) + 
  tm_bubbles(col = "Watershed_order", size = "DO_per",
             palette = viridisLite::viridis(12),
             perceptual = TRUE,
             size.max = 110,
             size.lim = c(30, 100),
             sizes.legend = c(0, 40, 80, 100),
             legend.col.show = FALSE) +
  tm_facets(along = "datetime", free.coords = FALSE,
            nrow = 1, ncol = 1) +
  tmap_options(limits = c(facets.plot = 241))

tmap_animation(anim_test, filename = "Figures/anim_test.gif",
               width = 6, height = 4,
               delay = 1)
anim_test




# Animate DO time series --------------------------------------------------
library(gganimate)

# Get rid of bad data when sensors were buried or when out of water
flood_sites1 <- c("Loise Feurs", "Mare Pont du diable", "Mare aval",
                  "Vizézy amont Bullieux", "Toranche aval", "Mare Azieux")
flood_sites2 <- c("Loise aval Poncins", "Loise Essertine en Donzy")
df_ts_sub <- df_ts %>%
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

# Only Loise watershed
df_ts_l <- filter(df_ts_sub,
                  Watershed == "Loise",
                  between(datetime, 
                          ymd_hms("2019-09-01 00:00:00"),
                          ymd_hms("2019-09-11 00:00:00"))) %>%
  group_by(Site, datetime = cut(datetime, breaks = "1 hours")) %>%
  ungroup() %>%
  mutate(datetime = as.POSIXct(datetime)) %>%
  group_by(Site, datetime) %>%
  summarize(DO_per = mean(DO_per, na.rm = TRUE)) %>%
  arrange(Site, datetime) %>%
  mutate(hour = hour(datetime),
         hour2 = ifelse(hour>12,
                        hour - 2 * (hour - 12),
                        hour)) %>%
  left_join(meta) %>%
  ungroup()

df_ts_l_sub <- filter(df_ts_l,
                      Location %in% c("la Jamarie",
                                      "Moulin Piquet",
                                      # "Château de Donzy",
                                      "Feurs",
                                      "aval Doise Salt"))

p_anim_loise <- ggplot(data = df_ts_l_sub, 
                       aes(x = datetime, 
                           y = DO_per,
                           group = Watershed_order,
                           color = as.factor(Watershed_order))) +
  geom_line(size = 2,
            alpha = 0.7,
            show.legend = FALSE)  +
  # geom_point(size = 3,
  #            show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 150),
                     breaks = seq(0, 150, 50)) +
  scale_color_viridis_d(name = "Watershed order") +
  # transition_reveal(datetime) + 
  theme_bw() + 
  geom_hline(yintercept = 100, linetype = "dashed") +
  scale_y_continuous(limits = c(0, 125),
                     breaks = seq(0, 125, 25)) +
  xlab("") +
  ylab("DO (% sat.)") +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18))

ggsave(plot = p_anim_loise, filename="Figures/loise_points_v2.tiff",
       dpi = 300, width = 8, height = 6, units = "in")
anim_save(animation = p_anim_loise, 
          filename="Figures/loise_points_v2.gif",
          height = 800,
          width = 1000)

p_map <- ggmap(plot_loc) + 
  geom_sf(data = filter(ws, Watershed == "Loise"), 
          alpha = 0.1, 
          color = "black", size = 1.2, inherit.aes = FALSE) +
  geom_sf(data = filter(riv_hw, Watershed == "Loise"), aes(size = brks),
          color = "blue", inherit.aes = FALSE, show.legend = FALSE) +
  xlab("") + ylab("") +
  geom_point(data = filter(df_loise_sub, counter == i),
             aes(x = Longitude,
                 y = Latitude,
                 fill = as.factor(Watershed_order),
                 size = DO_per),
             show.legend = FALSE,
             shape = 21,
             color = "white") + 
  geom_text_repel(data = filter(df_loise_sub, counter == i),
                  aes(x = Longitude,
                      y = Latitude,
                      label = Site),
                  color = "white",
                  size = 4) +
  scale_fill_viridis_d()
ggsave(p_map, filename = "Figures/Loise_map_watershed_order.tiff",
       device = "tiff",
       dpi = 300)
# Side by side with patchwork library -------------------------------------
library(patchwork)
library(animation)
# create counter
df_loise <- df_ts_l %>%
  group_by(Site) %>%
  mutate(counter = row_number()) %>%
  ungroup()

plot_loc <- get_map(location = c(pluck(df_n, 2, 1), pluck(df_n, 3, 1)),
                    maptype = "satellite",
                    zoom = 12)

brks <- tibble(OSTRAHL = c(1,2,3,4,5),
               brks = c(0.6, 0.8, 1, 1.4, 1.6))
riv_hw <- left_join(riv_hw, brks)

df_loise_sub <- filter(df_loise,
                       Location %in% c("la Jamarie",
                                       "Moulin Piquet",
                                       # "Château de Donzy",
                                       "Feurs",
                                       "aval Doise Salt"))
# Create gif
invisible(saveGIF({
  for (i in 1:240){
    set.seed(42)
    p <- ggmap(plot_loc) + 
      geom_sf(data = filter(ws, Watershed == "Loise"), 
              alpha = 0.1, 
              color = "black", size = 1.2, inherit.aes = FALSE) +
      geom_sf(data = filter(riv_hw, Watershed == "Loise"), aes(size = brks),
              color = "blue", inherit.aes = FALSE, show.legend = FALSE) +
      xlab("") + ylab("") +
      geom_point(data = filter(df_loise_sub, counter == i),
                 aes(x = Longitude,
                     y = Latitude,
                     fill = as.factor(Watershed_order),
                     size = DO_per),
                 show.legend = FALSE,
                 shape = 21,
                 color = "white") + 
      geom_text_repel(data = filter(df_loise_sub, counter == i),
                      aes(x = Longitude,
                          y = Latitude,
                          label = Site),
                      color = "white",
                      size = 4) +
      scale_fill_viridis_d()
    
    p2 <- ggplot() +
      geom_line(data = filter(df_loise_sub, counter <= i), 
                aes(x = datetime, 
                    y = DO_per,
                    group = Watershed_order,
                    color = as.factor(Watershed_order)),
                size = 2,
                alpha = 0.7,
                show.legend = FALSE) +
      geom_point(data = filter(df_loise_sub, counter == i), 
                 aes(x = datetime, 
                     y = DO_per,
                     group = Watershed_order,
                     color = as.factor(Watershed_order)),
                 size = 2,
                 alpha = 0.7,
                 show.legend = FALSE) +
      geom_hline(yintercept = 100, linetype = "dashed") +
      scale_x_datetime(limits = c(min(df_loise$datetime), 
                                  max(df_loise$datetime)),
                       breaks = "24 hours",
                       labels = date_format("%H:%M", tz = "GMT-2")) +
      scale_y_continuous(limits = c(25, 110),
                         breaks = seq(25, 110, 25)) +
      scale_color_viridis_d(name = "Watershed order") + 
      theme_bw() +
      theme(panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.y = element_text(size = 10),
            axis.text.y = element_text(size = 8),
            axis.text.x = element_text(size = 8)) +
      xlab("") +
      ylab("DO (% sat.)")
    
    # Print plots using patchwork
    print(p + p2 + plot_layout(ncol = 2))
  }
  
  
}, movie.name = "test3.gif", interval = 0.01, ani.width = 1000, ani.height = 700))
