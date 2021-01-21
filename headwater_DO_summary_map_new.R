# 
# Purpose: To headwater DO data on a map
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

# Load data ---------------------------------------------------------------
# Load DO summary data
df_sum <- readRDS("Headwaters/Data/DO_summary")
# df_met <- readRDS("Data/Headwaters_DO/headwaters_metabolism_mle_preds")
# flood_sites1 <- c("Loise Feurs", "Mare Pont du diable", "Mare aval",
#                   "Vizézy amont Bullieux", "Toranche aval", "Mare Azieux")
# flood_sites2 <- c("Loise aval Poncins", "Loise Essertine en Donzy")
# df_met<- df_met %>%
#   filter(!(Site %in% flood_sites1 & between(date, 
#                                             ymd("2019-08-06"),
#                                             ymd("2019-08-29"))),
#          !(Site %in% flood_sites2 & between(date, 
#                                             ymd("2019-08-06"),
#                                             ymd("2019-08-30"))),
#          !(Site == "Coise aval Montrond" & between(date, 
#                                                    ymd("2019-08-06"),
#                                                    ymd("2019-09-11"))),
#          !(Site == "Loise amont Doise Salt" & between(date, 
#                                                       ymd("2019-07-16"),
#                                                       ymd("2019-07-20"))),
#          !(Site == "Loise Essertine en Donzy" & between(date, 
#                                                         ymd("2019-07-20"),
#                                                         ymd("2019-07-23"))),
#          !(Site == "Vizézy amont Bullieux" & between(date, 
#                                                      ymd("2019-07-08"),
#                                                      ymd("2019-07-16"))),
#          !(Site == "Vizézy amont Bullieux" & between(date, 
#                                                      ymd("2019-07-24"),
#                                                      ymd("2019-08-29"))),
#          !(Site == "Toranche Pontet" & between(date, 
#                                                ymd("2019-07-12"),
#                                                ymd("2019-07-21"))),
#          !(Site == "Toranche Pontet" & between(date, 
#                                                ymd("2019-07-23"),
#                                                ymd("2019-07-29"))),
#          !(Site == "Toranche Pontet" & between(date, 
#                                                ymd("2019-07-31"),
#                                                ymd("2019-08-06"))),
#          !(Site == "Doise" & between(date, 
#                                      ymd("2019-07-14"),
#                                      ymd("2019-07-20"))),
#          !(Site == "Doise" & between(date, 
#                                      ymd("2019-08-03"),
#                                      ymd("2019-08-06"))),
#          between(GPP, 0, 10),
#          between(ER, -20, 0)
#          
#          
#   ) %>%
#   group_by(Site) %>%
#   summarize(mean_GPP = mean(GPP, na.rm = TRUE),
#             mean_ER = mean(ER, na.rm = TRUE))

# Load DO time series data
# df_ts <- readRDS("Data/Headwaters_DO/DO_time_series") %>%
#   mutate(DOsat = ifelse(temp == 0,
#                         0,
#                         14.652 - 0.41022 * temp + 0.007991 * 
#                           temp^2 - 0.000077774 * temp^3),
#          DO_per = DO * 100/ DOsat)

# Load metadata
meta <- read_excel("Headwaters/Data/Field measurements/sensor_metadata.xlsx",
                   sheet = "simple_meta",
                   col_types = c("numeric", "text", "text", 
                                 "text", "text",
                                 "numeric", "numeric",
                                 "text", "numeric",
                                 "text", "numeric", 
                                 "text", "numeric")) %>%
  select(-4, -5) %>%
  # rename(sensor = `Serial Number`) %>%
  filter(type == "DO") %>%
  distinct(Site, .keep_all = TRUE)

# Some data cleaning, make filename = sensor serial number, and correct datetime
df_sum <- left_join(df_sum, meta)
# df_met <- left_join(df_met, meta)

# Convert both time series and summary to sf object
# df_ts_sf <- st_as_sf(df_ts, coords = c("Longitude", "Latitude"))
df_sum_sf <- st_as_sf(df_sum, coords = c("Longitude", "Latitude"))
# df_met_sf <- st_as_sf(df_met, coords = c("Longitude", "Latitude"))
  
# # Get river data
riv <- st_read("Data/GIS/tnet_out.shp")
riv <- st_set_crs(riv, 2154)
riv <- st_transform(riv, 
                    "+proj=longlat +init=epsg:4326")

# Read in Loire watershed files
ws <- st_read("Headwaters/Data/GIS/watershed_sites_Loire_new2020.shp")
ws <- st_set_crs(ws, 2154)
ws <- st_transform(ws, 
                   "+proj=longlat +init=epsg:4326")
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
  filter(!(Site %in% c("Lignon aval Lanzon", "L'Anzon", "Genetay", "Le Lignon amont l'Anzon"))) %>%
  left_join(meta)

# Read in some discharge data
# df_q <- read_xlsx("Data/Headwaters_DO/Discharge/banque_hydro.xlsx")

# Read in landuse data
lu <- st_read("Data/GIS/loire_headwaters_landuse.shp")
lu <- st_set_crs(lu, 2154)
lu <- st_transform(lu, 
                   "+proj=longlat +init=epsg:4326")
lu <- mutate(lu,
             Site = str_remove_all(name_site, " -"),
             Site = str_remove_all(Site, "Pannissières"),
             Site = str_remove_all(Site, "moulin de Salt"),
             Site = str_replace(Site, "Donzy Salt", "Donzy"),
             Site = str_replace(Site, "aval Doise", "aval Doise Salt"),
             Site = str_replace(Site, "aval Poncins", "amont Poncins"),
             Site = str_remove_all(Site, "La "),
             Site = str_replace(Site, "Aval", "aval"),
             Site = str_trim(Site)) %>%
  left_join(meta)
lu$reGROUP <- as.factor(lu$reGROUP)

# ggmaps for satellite imagery --------------------------------------------
# Set your API Key
register_google(key = "AIzaSyCUFlGlYPLtIqC99Fv_xy_XabflfVG9XXM")

# Nest data by mean lat and long of watershed for plotting purposes
df_n <- df_sum_sf %>%
  left_join(ws %>%
              group_by(Watershed) %>%
              filter(Shape_Area == max(Shape_Area)) %>%
              mutate(
                        bbox = map(geometry, st_bbox),
                        bbox_poly = map(bbox, st_as_sfc),
                        cent = map(bbox_poly, st_centroid),
                        long_mean = map(cent, c(1, 1)),
                        lat_mean = map(cent, c(1, 2)),
                        ) %>%
              unnest(long_mean, lat_mean) %>%
              as_tibble() %>%
              select(long_mean, lat_mean, Watershed)) %>%
  ungroup() %>%
  nest(-long_mean, -lat_mean, -Watershed, -msmt) %>%
  filter(msmt %in% c("amp_per", "max_per", "min_per", "mean_T"))

# Plotting function
plot_fun <- function(plot_loc, watershed, msmt_type, df = df_sum){
  set.seed(42)
  ws_plot = filter(ws, Watershed == watershed)
  riv_plot = st_intersection(riv, ws_plot)
  brks = tibble(OSTRAHL = c(1,2,3,4,5),
                brks = c(0.6, 0.8, 1, 1.4, 1.6))
  riv_plot = left_join(riv_plot, brks)
  bbox = st_bbox(ws_plot)
  ggmap(plot_loc) + 
    geom_sf(data = ws_plot, alpha = 0.01, 
            color = "black", size = 0.8, inherit.aes = FALSE) +
    geom_sf(data = riv_plot, aes(size = brks),
            color = "blue", inherit.aes = FALSE, show.legend = FALSE) +
    xlab("") + ylab("") +
    ggtitle(watershed) +
    scale_size_identity() +
    geom_point(data = filter(df, Watershed == watershed, msmt == msmt_type),
               aes(x = Longitude,
                   y = Latitude,
                   color = value),
               size = 2.2) +
    geom_text_repel(data = filter(df, Watershed == watershed, msmt == msmt_type),
                     aes(x = Longitude,
                         y = Latitude,
                         label = Site),
                    color = "white") +
    scale_color_viridis_c(name = msmt_type, option = "plasma") +
    # scale_size_continuous(name = "Amp. quo. \n moy. (mg/L)") +
    scale_x_continuous(limits = c(bbox[1]-0.01, bbox[3]+0.01), expand = c(0, 0)) +
    scale_y_continuous(limits = c(bbox[2], bbox[4]), expand = c(0, 0))
}

# Get base map for each watershed
df_n_p <- df_n %>%
  group_by(Watershed) %>%
  mutate(plot_loc = map2(long_mean, lat_mean, ~get_map(location = c(.x, .y),
                                                       maptype = "satellite",
                                                       zoom = 11)
                         ),
         plots = pmap(list(plot_loc, Watershed, msmt), 
                      plot_fun
                      )
         )


# Save data 
pwalk(list(df_n_p$Watershed,
           df_n_p$plots,
           df_n_p$msmt),
      ~ggsave(plot = ..2,
              filename = paste0("Headwaters/Figures/",
                                ..1,
                                "_",
                                ..3,
                                "_satellite_zoom11.tiff"),
              device = "tiff",
              dpi = 600,
              width = 18.4,
              height = 18.4,
              unit = "cm"))

# Static map of min DO with tmap ------------------------------------------------
# Animations
# Join discharge data with geometry data
riv2 <- df_q %>%
  mutate(Site = str_replace(site, "Coise Moulin Brûlé", "Coise amont St Symphorien")) %>%
  right_join(riv %>%
               mutate(Site = ifelse(str_detect(TOPONYME1, "coise"),
                                    "Coise amont St Symphorien",
                                    NA)
                      )
             ) %>%
  st_as_sf()

# Intersect river data with headwaters data
riv_hw <- st_intersection(ws, riv)

df_met_sf$PR <- df_met_sf$mean_GPP / abs(df_met_sf$mean_ER)
# Plot minimum DO with land use
p_tm <- tm_shape(ws) + tm_borders() +
  tm_facets(by = "Watershed") +
  tm_shape(riv_hw) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
           legend.lwd.show = FALSE,
           scale = 1.6) +
  tm_facets(by = "Watershed") +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_shape(lu) + 
  tm_fill(title = "Land use",
          col = "reGROUP", alpha = 0.3,
          breaks = c(11, 12, 21, 22, 31, 32, 50),
          labels = c("Terr. art. dense",
                     "Terr. art. discon.",
                     "Ag. int.",
                     "Ag. fai. imp.",
                     "Forêt",
                     "Pelouses, friches",
                     "Espace ouverts",
                     "Zones humides",
                     "Surface en eaux"),
          palette = "Spectral") +
  tm_facets(by = "Watershed") +
  # tm_shape(df_sum_sf) +
  # tm_symbols(
  #         col = "PR", size = 0.5,
  #         # breaks = c(0, 3, 4, 6, 8, 10),
  #         palette = "viridis",
  #         border.col = "black",
  #         border.lwd = 1.2,
  #         title.col = "Mean P/R") +
  # tm_facets(by = "Watershed")

# Get an inset map
# Inset map of just rivers
region_map <- tm_shape(ws) + tm_polygons() +
  tm_shape(riv_hw) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
           legend.lwd.show = FALSE,
           scale = 2) +
  tm_shape(filter(riv,
                  str_detect(TOPONYME1, "loire"))) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
           legend.lwd.show = FALSE,
           scale = 3) +
  tm_shape(df_met_sf) +
  tm_symbols(
    col = "PR", size = 0.3,
    # breaks = c(0, 3, 4, 6, 8, 10),
    palette = "viridis",
    border.col = "black",
    border.lwd = 1.2,
    title.col = "Mean P/R",
    legend.col.show = FALSE)

# Save map
tmap_save(p_tm, filename = "Figures/land_use_PR.png",
          width = 6, height = 6,
          insets_tm = region_map,
          insets_vp = viewport(0.52, 0.18, width = 0.33, height = 0.45))
  
