# 
# Purpose: To estimate reaeration based on GIS data, and to calculate land use
# at different scales for multiple regression analysis
# Author: Jake Diamond
# Date: October 1, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
setwd("C:/Users/diamo/Dropbox/Projects/Loire_headwaters")

# Load libraries
library(lubridate)
library(readxl)
library(sf)
library(tmap)
library(scales)
library(ggforce)
library(tidyverse)


# Reaeration constant estimation ------------------------------------------
# Load data
# Read in hydraulic data
df_k <- read_xlsx("Headwaters/Data/hydraulic_data.xlsx")

# Estimate K600 with 4 diff equations from Raymond 2012
# Then average, calculate Schmidt number assuming T = 15 deg C
# Then calculate K_O2, and K_2, and the estimated reach length (50% gas loss)
df_k <- df_k %>%
  mutate(k600_eq1 = (velocity_mps * slope_tnet)^0.89 * depth_m^0.54 * 5037,
         k600_eq3 = 1162 * slope_tnet^0.77 * velocity_mps^0.85,
         k600_eq4 = (velocity_mps * slope_tnet)^0.76 * 951.5,
         k600_eq5 = velocity_mps * slope_tnet * 2841 + 2.02) %>%
  mutate(k600_avg = rowMeans(dplyr::select(., starts_with("k600"))),
         Sc = 1801 - 120.1 * 15 + 3.782 * 15^2 -0.0476 * 15^3,
         k_o2 = k600_avg * sqrt(600/Sc),
         k_2 = k_o2 / depth_m,
         reach_length = 0.7 * velocity_mps * 86400 / k_2)

# Intersecting reaches with land use --------------------------------------
# Read in reach data for each point
# These reach lengths are calculated based on estimated k2 and velocity from
# the equation L = 0.7v/k2
# reaches <- st_read("Data/GIS/syrah_reaches.shp")
# reaches <- st_set_crs(reaches, 2154)

# Read in tnet shapefile
tnet <- st_read("Headwaters/Data/GIS/tnet_headwaters.shp")
tnet <- st_set_crs(tnet, 2154)

# Read in metadata for the reaches of concern, and filter tnet data
tnet_reaches <- read_excel("Headwaters/Data/GIS/headwaters_tnet_reaches.xlsx") %>%
  left_join(tnet, by = "ID_ND_I") %>%
  # distinct(c(ID_ND_I, FID), .keep_all = TRUE) %>%
  st_as_sf() %>%
  group_by(site) %>%
  summarize()

# Load land use data
lu <- st_read("Headwaters/Data/GIS/loire_headwaters_landuse.shp")
lu <- st_set_crs(lu, 2154)
# lu <- lu[-(1476:1477),] #weird intersecting geometry issue here, just remove
# plot(lu)

# Load vegetation data (need to combine depts 42 and 69)
veg <- st_read("Headwaters/Data/GIS/vegetation_headwaters.shp")
veg <- st_set_crs(veg, 2154)

# Read in Loire watershed files
ws <- st_read("Headwaters/Data/GIS/watershed_sites_Loire_new2020.shp")
ws <- st_set_crs(ws, 2154)

# Read in site points
pts <- st_read("Headwaters/Data/GIS/site_points_L93.shp")
pts <- st_set_crs(pts, 2154)

# Read in la loire
loire <- st_read("Headwaters/Data/GIS/tnet_out.shp")
loire <- st_set_crs(loire, 2154)

# Read in watershed info
ws_meta <- read_excel("Headwaters/Data/Field measurements/sensor_metadata.xlsx",
                   sheet = "landscape")

# Join that info to the ws file
ws <- ws %>%
  arrange(name_site) %>%
  bind_cols(arrange(drop_na(ws_meta, name_site), name_site) %>%
              select(-name_site))

# Intersect tnet reaches to the watersheds
ws_int <- st_intersection(ws, tnet)

# Add watershed data to pts
pts <- pts %>%
  arrange(Object_ID) %>%
  bind_cols(arrange(drop_na(ws_meta, Object_ID), Object_ID) %>%
              select(-Object_ID))

# # Read in lab chemistry data and summarize
# df_chem <- read_excel("Headwaters/Data/water_chemistry/Water_chemistry_all_v2.xlsx") %>%
#   select(-sensor, -`Sample bottle no.`) %>%
#   group_by(site) %>%
#   mutate_if(is.character, parse_number) %>%  #convert LDL to the DL
#   ungroup() %>%
#   pivot_longer(-c(datetime, site)) %>%
#   left_join(read_excel("Headwaters/Data/water_chemistry/detection_limits.xlsx")) %>%
#   mutate(value = if_else(value < dl, value * 0.5, value)) %>%
#   group_by(site, name) %>%
#   summarize(mean = median(value, na.rm = TRUE))
  
# Intersect landuse and veg to watersheds
lu_ws <- st_intersection(ws, st_make_valid(lu)) %>%
  st_collection_extract("POLYGON") # some of the intersections produce "GEOMETRY" type, don't want
veg_ws <- st_intersection(ws, veg)

# Calculate 1, 2, 5, 10, 20, 50, and 100 m buffers around each reach
buff_dists <- list(buff_dists = c(10, 20, 50, 100, 200, 500))

# Get data in good format with sf as list column for each buffer distance
# Then buffer and intersect, calculate areas of intersection
ints <- as_tibble(buff_dists) %>%
  mutate(data = list(tnet_reaches),
         lu = list(lu)
         # ,veg = list(veg)
         ) %>%
  mutate(buffs = map2(data, buff_dists, st_buffer)) %>%
  mutate(ints_lu = map2(buffs, lu, st_intersection)
         # ,ints_veg = map2(buffs, veg, st_intersection)
         ) %>%
  mutate(area_lu = map(ints_lu, st_area)
         # ,area_veg = map(ints_veg, st_area)
         )

# Summarize intersection data
int_lu_sum <- ints %>%
  unnest_legacy(ints_lu, area_lu, .preserve = buff_dists) %>%
  group_by(site, buff_dists, reGROUP) %>%
  summarize(area_lu_m2 = sum(area_lu)) %>%
  mutate(area_total_m2 = sum(area_lu_m2),
         area_lu_frac = area_lu_m2 / area_total_m2)

int_veg_sum <- ints %>%
  unnest_legacy(ints_veg, area_veg, .preserve = buff_dists) %>%
  group_by(site, buff_dists, NATURE) %>%
  summarize(area_veg_m2 = sum(area_veg)) %>%
  mutate(area_total_m2 = sum(area_veg_m2),
         area_veg_frac = area_veg_m2 / area_total_m2)

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

int_lu_sum <- left_join(int_lu_sum, meta)
# Color palette for plotting
lu_pal7 <- c("#ffffa8", "#e68000", "#80ff00", "#ccffcc", "#80f2e6", "#cc0000", "#ff0000")
# Quick plots of how land use changes at increasing buffer distances
buff_plot <- ggplot(data = int_lu_sum,
       aes(x = buff_dists,
           y = area_lu_frac,
           color = landuse)) +
  geom_line(size = 2, alpha = 0.8) +
  facet_wrap(~site) +
  scale_color_manual(values = lu_pal7) +
  theme_bw() +
  xlab("Buffer distance from stream (m)") +
  ylab("Fraction of land cover")
buff_plot
ggsave(plot = buff_plot,
       filename = "Headwaters/Figures/buffer_plot_larger_new_paper.png",
       device = "png",
       dpi = 600,
       height = 22,
       width = 18.4,
       units = "cm")

buff_plot_veg <- ggplot(data = int_veg_sum,
                    aes(x = buff_dists,
                        y = area_veg_frac,
                        color = NATURE)) +
  geom_line(size = 2, alpha = 0.8) +
  facet_wrap(~site) +
  # scale_color_manual(values = lu_pal7) +
  theme_bw() +
  xlab("Buffer distance from stream (m)") +
  ylab("Fraction of vegetation cover")

ggsave(plot = buff_plot_veg,
       filename = "Headwaters/Figures/buffer_plot_vegetation_larger_new.pdf",
       dpi = 600,
       height = 30,
       width = 42,
       units = "cm")

# Write to disc
saveRDS(int_lu_sum, "Headwaters/Data/buffer_land_use")
saveRDS(int_veg_sum, "Headwaters/Data/buffer_vegetation")
int_sum <- readRDS("Headwaters/Data/buffer_land_use")
# Plotting ----------------------------------------------------------------
# Overall plots of just points
# Want to get the biggest watersheds to prevent over-plotting
ws_large <- ws %>%
  group_by(watershed) %>%
  filter(!(name_site %in% c("Coise aval - Montrond", "Coise amont - St Symphorien"))) %>%
  filter(surf_km2 == max(surf_km2)) %>%
  ungroup()

lu_ws_large <- lu_ws %>%
  group_by(watershed) %>%
  filter(!(name_site %in% c("Coise aval - Montrond", "Coise amont - St Symphorien"))) %>%
  filter(surf_km2 == max(surf_km2)) %>%
  ungroup()

p_lu_pts <- tm_shape(ws_large) + tm_borders() +
  tm_facets(by = "watershed") +
  tm_shape(filter(ws_int, !(name_site %in% c("Coise aval - Montrond", "Coise amont - St Symphorien")))) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
           legend.lwd.show = FALSE,
           scale = 1.6) +
  tm_facets(by = "watershed") +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_shape(lu_ws_large) + 
  tm_fill(col = "reGROUP", alpha = 0.6,
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
  tm_facets(by = "watershed") +
  tm_shape(pts) +
  tm_symbols(size = 0.35,
             col = "type",
             alpha = 0.5,
             border.col = "black",
             border.lwd = 1.2) +
  tm_facets(by = "watershed") +
  tm_layout(scale = 2)
p_lu_pts
# Get an inset map
# Inset map of just rivers
region_map <- tm_shape(ws) + tm_polygons(col = "lightgrey",
                                         border.col = "black") +
  tm_shape(tnet) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
           legend.lwd.show = FALSE,
           scale = 2) +
  tm_shape(filter(loire,
                  str_detect(TOPONYME1, "loire"))) +
  tm_lines(lwd = "OSTRAHL", col = "blue",
           legend.lwd.show = FALSE,
           scale = 3) +
  tm_shape(pts) +
  tm_symbols(
    size = 0.2,
    alpha = 0.2,
    col = "type",
    border.col = "black",
    border.lwd = 1.2,
    title.col = "point") +
  tm_layout(legend.show = FALSE)
region_map
# Save map
library(grid)
tmap_save(p_lu_pts, filename = "Headwaters/Figures/land_use_points_paper_color_pts.pdf",
          dpi = 600,
          width = 18.4, height = 22,
          units = "cm",
          insets_tm = region_map,
          insets_vp = viewport(0.52, 0.25, width = 0.33, height = 0.45))

# Plot maps
buff_data <- pluck(ints, 6, 6) %>%
  left_join(meta)
veg_data <- pluck(ints, 7, 6)

# Define landuse color palette
lu_pal5 <- c("#ffffa8", "#e68000", "#80ff00", "#ccffcc", "#ff0000")

# landuse plot at buffer scale
lu_p <- tm_shape(buff_data) +
  tm_fill(col = "reGROUP", alpha = 0.7,
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
           palette = "Spectral",
           legend.fill.show = FALSE) +
  tm_facets(by = "site") +
  tm_shape(tnet_reaches) +
  tm_lines(col = "blue") +
  tm_facets(by = "site") +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"),
               text.size = 1.5) +
  tm_layout(legend.show = FALSE)
            # panel.label.size = 1.8,
            # legend.text.size = 1.5,
            # legend.title.size = 1.6)

tmap_save(lu_p, filename = "Headwaters/Figures/land_use_buffers_200_new_paper.png",
          dpi = 600,
          width = 18.4, 
          height = 22,
          units = "cm")

veg_p <- 
  tm_shape(veg_data) +
  tm_fill("NATURE") +
  tm_facets(by = "site") +
  tm_shape(tnet_reaches) +
  tm_lines(col = "blue") +
  tm_facets(by = "site") +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"),
               text.size = 1.5) +
  tm_layout(legend.show = FALSE)

tmap_save(veg_p, filename = "Headwaters/Figures/veg_buffers_200_new_no_legend.pdf",
          dpi = 600,
          height = 22,
          width = 18.4,
          units = "cm")

lu_pal7 <- c("#ffffa8", "#e68000", "#80ff00", "#ccffcc", "#80f2e6", "#cc0000", "#ff0000")
# Landuse plot at the watershed scale
lu_ws <- left_join(lu_ws, meta)
lu_ws_p <- tm_shape(lu_ws) +
  tm_fill(col = "reGROUP", alpha = 0.6,
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
          palette = "Spectral",
           legend.fill.show = FALSE) +
  tm_facets(by = "name_site") +
  tm_shape(ws_int) +
  tm_lines(lwd = "OSTRAHL", col = "blue", scale = 2,
           legend.lwd.show = FALSE, alpha = 0.6) +
  tm_facets(by = "name_site") +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"),
               text.size = 1.5) +
  tm_layout(legend.show = FALSE)
            # panel.label.size = 1.8,
            # legend.text.size = 1.3,
            # legend.title.size = 1.4)

tmap_save(lu_ws_p, filename = "Headwaters/Figures/land_use_watershed_new_paper.png",
          dpi = 600,
          height = 22,
          width = 18.4,
          units = "cm")


# Veg plot at the watershed scale
veg_ws_p <- tm_shape(veg_ws) +
  tm_fill("NATURE") +
  tm_facets(by = "name_site") +
  tm_shape(ws_int) +
  tm_lines(lwd = "OSTRAHL", col = "blue", scale = 2,
           legend.lwd.show = FALSE, alpha = 0.6) +
  tm_facets(by = "name_site") +
  tm_compass(type = "arrow", position = c("LEFT", "TOP"), size = 1) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"),
               text.size = 1.5) +
  tm_layout(
            panel.label.size = 1.8,
            legend.text.size = 1.3,
            legend.title.size = 1.4)

tmap_save(veg_ws_p, filename = "Headwaters/Figures/veg_watershed_new.pdf",
          dpi = 600,
          height = 30,
          width = 42,
          units = "cm")

# Try the landscapemetrics package ----------------------------------------
library(landscapemetrics)
library(landscapetools)
library(fasterize)
# # Get data into raster format and calculate all landscape metrics by land use
# sites <- unique(lu_ws$name_site)
# metrics <- data.frame(site = sites, ls_mets = rep(NA,length(sites)))
# for(i in 1:length(sites)){
#   site = sites[i]
#   data = filter(lu_ws, name_site == site)
#   data = st_cast(data, "MULTIPOLYGON")
#   r = raster(data, resolution = 1)
#   rf = fasterize(data, r, field = "reGROUP")
#   metrics[i, 2] = list(calculate_lsm(rf, what = c("lsm_l_ent", "lsm_l_condent",
#                             "lsm_l_joinent", "lsm_l_mutinf", 
#                             "lsm_l_frac", "lsm_l_contag"),
#                 full_name = TRUE))
#   rm(r, rf)
# }
# 
#   lu
#   pluck(6, 1) %>%
#   st_cast("MULTIPOLYGON") %>%
#   group_by(site) %>%
#   nest() %>%
#   mutate(r = map(data, ~raster(., resolution = 100)),
#          rf = map2(data, r, ~fasterize(.x, .y, field = "reGROUP"))) %>%
#   mutate(metrics_ls = map(rf, ~calculate_lsm(., level = "landscape")),
#          metrics_cl = map(rf, ~calculate_lsm(., level = "class")))
# # 
# # Get data in dataframe format
# lu_metrics <- lu_n %>%
#   select(site, metrics_ls) %>%
#   unnest()
# 
# lu_metrics %>%
#   group_by(metric) %>%
#   summarize(mean = mean(value, na.rm = TRUE),
#             sd = sd(value, na.rm = TRUE),
#             cv = sd / mean) %>%
#   filter(cv > 0.75)
#  
# ggplot(data= lu_metrics,
#        aes(x = site,
#            y = value,
#            color = site)) + geom_point() +
#   facet_wrap(~metric,scales = "free_y")
# 
# # Do the same for vegetation patches
# # Get data into raster format and calculate all landscape metrics by land use
# veg_n <- ints %>%
#   filter(buff_dists == 100) %>%
#   pluck(7, 1) %>%
#   st_cast("MULTIPOLYGON") %>%
#   group_by(site) %>%
#   mutate(veg_type = as.factor(NATURE)) %>%
#   nest() %>%
#   mutate(r = map(data, ~raster(., resolution = 100)),
#          rf = map2(data, r, ~fasterize(.x, .y, field = "veg_type"))) %>%
#   mutate(metrics_ls = map(rf, ~calculate_lsm(., level = "landscape")),
#          metrics_cl = map(rf, ~calculate_lsm(., level = "class")))
# 
# # Get data in dataframe format
# veg_metrics <- veg_n %>%
#   ungroup() %>%
#   select(metrics_ls)
# 
# # Look at the correlation in the metrics
# show_correlation(lu_metrics, method = "pearson")
# 

# Get data into raster format and calculate all landscape metrics by land use
lu_ws_lm <- lu_ws %>%
  # filter(surf_km2 < 200) %>%
  st_cast("MULTIPOLYGON") %>%
  group_by(watershed, subwatershed, site, site_code) %>%
  nest() %>%
  mutate(r = map(data, ~raster(., resolution = 100)),
         rf = map2(data, r, ~fasterize(.x, .y, field = "reGROUP"))) %>%
  mutate(metrics_ls = map(rf, ~calculate_lsm(., what = c("lsm_l_ent", "lsm_l_condent",
                                                     "lsm_l_joinent", "lsm_l_mutinf", 
                                                     "lsm_l_frac", "lsm_l_contag"),
                                             full_name = TRUE)),
         metrics_cl = map(rf, ~calculate_lsm(., what = c("lsm_c_clumpy", "lsm_c_cohesion",
                                                         "lsm_c_tca", "lsm_c_frac_mn", 
                                                         "lsm_c_contig_mn"),
                                             full_name = TRUE)))

# Get data in dataframe format
lu_ws_metrics <- lu_ws_lm %>%
  select(watershed, subwatershed, site, site_code, metrics_ls) %>%
  unnest()

ggplot(data = lu_ws_metrics,
       aes(x = site,
           y = value,
           color = site)) +
  geom_point() + facet_wrap(~metric, scales = "free_y")

# Get data in dataframe format
lu_ws_metrics_cl <- lu_ws_lm %>%
  select(watershed, subwatershed, site, site_code, metrics_cl) %>%
  unnest(cols = c(metrics_cl)) %>%
  mutate(reGROUP = class) %>%
  left_join(meta)




lu_buff_lm <- ints %>%
  pluck(5, 6) %>%
  st_cast("MULTIPOLYGON") %>%
  group_by(name_site, site) %>%
  nest() %>%
  mutate(r = map(data, ~raster(., resolution = 100)),
         rf = map2(data, r, ~fasterize(.x, .y, field = "reGROUP"))) %>%
  mutate(metrics_ls = map(rf, ~calculate_lsm(., what = c("lsm_l_ent", "lsm_l_condent",
                                                         "lsm_l_joinent", "lsm_l_mutinf", 
                                                         "lsm_l_frac", "lsm_l_contag"),
                                             full_name = TRUE)),
         metrics_cl = map(rf, ~calculate_lsm(., what = c("lsm_c_clumpy", "lsm_c_cohesion",
                                                         "lsm_c_tca", "lsm_c_frac_mn", 
                                                         "lsm_c_contig_mn"),
                                             full_name = TRUE)))

# Get data in dataframe format
lu_buff_metrics_cl <- lu_buff_lm %>%
  select(name_site, site, metrics_cl) %>%
  unnest() %>%
  mutate(reGROUP = class) %>%
  left_join(meta)
saveRDS(lu_buff_metrics_cl, "Headwaters/Data/landuse_buffers_metrics")
# Get data in dataframe format
lu_buff_metrics <- lu_buff_lm %>%
  select(name_site, site, metrics_ls) %>%
  unnest()
saveRDS(lu_buff_metrics, "Headwaters/Data/landuse_buffers_metrics_by_class")

# Maybe try ArcGIS binding analysis here ----------------------------------
riv <- st_read("Data/GIS/tnet_headwaters.shp")
riv <- st_set_crs(riv, 2154)

# Get rid of duplicates
riv <- distinct(riv, ID_ND_I, .keep_all = TRUE)

# Bind all rivers of the same name
riv_bind <- riv %>%
  mutate(TOPONYME1 = str_remove_all(TOPONYME1, "riviere"),
         TOPONYME1 = str_remove_all(TOPONYME1, "rivisre"),
         TOPONYME1 = str_remove_all(TOPONYME1, "rivitre"),
         TOPONYME1 = str_remove_all(TOPONYME1, "rivivre"),
         TOPONYME1 = str_remove_all(TOPONYME1, "riviire"),
         TOPONYME1 = str_replace_all(TOPONYME1, "charpassone", "charpassonne"),
         TOPONYME1 = str_replace_all(TOPONYME1, "vizizy", "vizézy"),
         TOPONYME1 = str_replace_all(TOPONYME1, "vizuzy", "vizézy"),
         TOPONYME1 = str_replace_all(TOPONYME1, "viznzy", "vizézy"),
         TOPONYME1 = str_replace_all(TOPONYME1, "viz zy", "vizézy"),
         TOPONYME1 = str_replace_all(TOPONYME1, "vizezy", "vizézy"),
         TOPONYME1 = str_replace_all(TOPONYME1, "ruisseau la loise", "la loise"),
         TOPONYME1 = ifelse(is.na(TOPONYME1), FID_tnet_o, TOPONYME1),
         TOPONYME1 = str_trim(TOPONYME1)) %>%
  group_by(TOPONYME1) %>%
  summarize()

# Snap points to lines
sites <- st_read("Data/GIS/site_locations.shp")
sites <- st_set_crs(sites, 2154)
site_snap <- st_snap(sites, riv_bind, tol=100)
x <- st_intersection(riv, site_snap)
plot(x)
reach_sites <- st_snap(sites_snap, riv, tol=1e-9)
reach_sites <- st_intersection(sites, riv)

# Read profile elevation data
prof <- st_read("Data/GIS/TRONCON_COURS_EAU.shp", 
                fid_column_name = "FID")
prof <- st_set_crs(prof, 2154)
?st_segmentize()






parts <- st_collection_extract(lwgeom::st_split(riv_bind$geometry, 
                                                site_snap$geometry),
                               "LINESTRING")

tm_shape(filter(riv_bind, TOPONYME1 == "la coise")) + tm_lines()
tm_shape(parts) + tm_lines() +
  tm_shape(site_snap) +tm_dots(size = 0.5)
# 

# Write to disc
st_write(df, "Data/GIS/tnet_out.shp")




# Saving data -------------------------------------------------------------
df_lu <- int_lu_sum %>%
  ungroup() %>%
  # separate(Toponyme, c("river", "loc"),  sep = "_") %>%
  # mutate(site = str_c(str_to_title(word(river, 3
  # )
  # ),
  # loc, sep = " "
  # ),
  # site = str_squish(site),
  # site = str_trim(site),
  # site = ifelse(site == "Charpassone la Jamarie",
  #               "Charpassonne la Jamarie",
  #               site),
  # site = ifelse(site == "Charpassone de Donzy Salt",
  #               "Charpassone de Donzy",
  #               site),
  # site = ifelse(site == "Doise Doise",
  #               "Doise",
  #               site),
  # site = ifelse(site == "Curraize Curraize",
  #               "Curraize",
  #               site)) %>%
  # left_join(meta) %>%
  mutate(lu_buff = str_c(landuse, "_", buff_dists, "m")) %>%
  # ungroup() %>%
  dplyr::select(-reGROUP, -area_lu_m2,
                -area_total_m2, -landuse, -buff_dists) %>%
  pivot_wider(names_from = lu_buff, values_from = area_lu_frac)

# x <- x %>%
#   separate(Toponyme, c("river", "loc"),  sep = "_") %>%
#   mutate(site = str_c(str_to_title(word(river, 3
#   )
#   ),
#   loc, sep = " "
#   ),
#   site = str_squish(site),
#   site = str_trim(site),
#   site = ifelse(site == "Charpassone la Jamarie",
#                 "Charpassonne la Jamarie",
#                 site),
#   site = ifelse(site == "Charpassone de Donzy Salt",
#                 "Charpassone de Donzy",
#                 site),
#   site = ifelse(site == "Doise Doise",
#                 "Doise",
#                 site),
#   site = ifelse(site == "Curraize Curraize",
#                 "Curraize",
#                 site)) %>%
#   left_join(meta) %>%
#   mutate(lu_buff = str_c(landuse, "_", buff_dists, "m")) %>%
#   ungroup() %>%
#   dplyr::select(-river, -loc, -reGROUP, -area_lu_m2, 
#                 -area_total_m2, -landuse, -buff_dists) %>%
#   spread(lu_buff, area_lu_frac)



df_veg <- int_veg_sum %>%
  # separate(Toponyme, c("river", "loc"),  sep = "_") %>%
  # mutate(site = str_c(str_to_title(word(river, 3)
  # ),
  # loc, 
  # sep = " "),
  # site = str_squish(site),
  # site = str_trim(site),
  # site = ifelse(site == "Charpassone la Jamarie",
  #               "Charpassonne la Jamarie",
  #               site),
  # site = ifelse(site == "Charpassone de Donzy Salt",
  #               "Charpassone de Donzy",
  #               site),
  # site = ifelse(site == "Doise Doise",
  #               "Doise",
  #               site),
  # site = ifelse(site == "Curraize Curraize",
  #               "Curraize",
  #               site)) %>%
  mutate(veg_buff = str_c(NATURE, "_", buff_dists, "m")) %>%
  ungroup() %>%
  dplyr::select(-area_veg_m2, 
                -area_total_m2, -NATURE, -buff_dists) %>%
  pivot_wider(names_from = veg_buff, values_from = area_veg_frac)

# Write to disc for regression
saveRDS(df_lu, "Headwaters/Data/buffer_land_use_regression")
write_csv(df_lu, "Headwaters/Data/buffer_land_use_regression.csv")

# Combine veg and land use
df <- left_join(df_lu, df_veg)

# Save all
saveRDS(df, "Headwaters/Data/buffer_land_use_veg_regression")
write_excel_csv2(df, "Headwaters/Data/buffer_land_use_veg_regression.csv")

