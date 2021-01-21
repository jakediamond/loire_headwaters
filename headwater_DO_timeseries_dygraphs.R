# 
# Purpose: To plot first round of DO data
# Author: Jake Diamond
# Date: July 17, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("//ly-lhq-srv/jake.diamond/Loire_DO")

# Load libraries
library(dygraphs)
library(htmltools)
# library(ggpubr)
library(plotly)
library(lubridate)
library(readxl)
library(scales)
library(tidyverse)

# Set ggplot theme
th <- theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    panel.grid = element_blank()
  )

# Load data
df <- readRDS()
data_path_do <- "Headwaters/Data/HOBO_raw_csv"
files_do <- dir(data_path_do, pattern = "*.csv")

# First get data path and names of files for light
data_path_light <- "Headwaters/Data/HOBO_light_raw_csv"
files_light <- dir(data_path_light, pattern = "*.csv")

# Then load all data into one dataframe
df <- tibble(path = c(rep(data_path_do, length(files_do)),
                      rep(data_path_light, length(files_light))),
             filename = c(files_do, files_light)) %>% 
  mutate(file_contents = map2(path, filename,
                             ~ read_csv(file.path(.x, .y),
                                        skip = 2,
                                        col_names = FALSE))) %>%
  unnest(cols = c(file_contents)) %>%
  select(-X5)

# Load metadata
meta <- read_excel("Headwaters/Data/Field measurements/sensor_metadata.xlsx",
                   sheet = 4) %>%
  mutate(sensor = as.character(sensor))

# add a column for west/east
side <- tibble(Watershed = c("Coise", "Loise", "Toranche", "Lignon", "Mare"),
               side = c("east", "east", "east", "west", "west"))

# Some data cleaning, make filename = sensor serial number, and correct datetime
# Combine sites to sensors, but need to account for datetime...fuzzy join
df2 <- df %>%
  separate(filename, c("sensor", "recoverydate"), "_") %>%
  select(-X1) %>%
  mutate(X2 = mdy_hms(X2),
         Year = year(X2)) %>%
  rename(datetime = X2) %>%
  group_by(recoverydate, sensor) %>%
  nest() %>%
  mutate(min_date = map(data, ~min(.$datetime)),
         max_date = map(data, ~max(.$datetime))) %>%
  ungroup() %>%
  fuzzy_left_join(select(meta, Site, sensor, date_start, date_end, type),
                  by = c("sensor" = "sensor",
                         "min_date" = "date_start",
                         "max_date" = "date_end"),
                  match_fun = list(`==`, `>=`, `<=`)) %>%
  select(-sensor.y) %>%
  rename(sensor = sensor.x) %>%
  distinct() %>% #some files included previous uploads...remove duplicates
  unnest(data) %>%
  distinct() #some files included previous uploads...remove duplicates

# Add back some meta data for each site
df3 <- df2 %>%
  ungroup() %>%
  rename(value = X3, temp = X4) %>%
  select(-sensor, -path, -Year, -recoverydate,
         -min_date, -max_date, -date_start, -date_end) %>%
  left_join(distinct(select(meta, -sensor, -date_start, -date_end, -type, -Year)),
            by = c("Site")) %>%
  left_join(side) %>%
  mutate(site_code = str_c(str_sub(Site, 1, 3),
                           str_pad(round(area_km2), 3, pad = "0"))) %>%
  mutate(side = fct_rev(side)) %>%
  filter(!is.na(type)) %>%
  distinct(Site, datetime, value, temp, .keep_all = TRUE)

# Get into wider format
df_w <- df3 %>%
  pivot_wider(names_from = type, values_from = c(value, temp),
              values_fn = mean) %>%
  rename(DO = value_DO,
         DO_temp = temp_DO,
         lux_water = temp_light_water, #these are switched for light sensors
         lux_water_temp = value_light_water,
         lux_air = temp_light_air,
         lux_air_temp = value_light_air)

# Some data cleaning
df_w <- df_w %>%
  mutate_at(vars(11:16), funs(if_else(. < 0, NA_real_, .))) # get rid of negative values
# Get rid of crazy values
df_w <- df_w %>%
  mutate(DO = if_else(DO > 20, NA_real_, DO),
         DO_temp = if_else(DO_temp > 30, NA_real_, DO_temp),
         lux_air = if_else(lux_air > 100000, NA_real_, lux_air),
         lux_water = if_else(lux_water > 100000, NA_real_, lux_water),
         lux_air_temp = if_else(lux_air_temp > 35, NA_real_, lux_air_temp),
         lux_water_temp = if_else(lux_water_temp > 30, NA_real_, lux_water_temp)) #crazy values out

# Save data
saveRDS(df_w, "Headwaters/Data/DO_time_series_wide")
# Split: one data frame per Species
df_w %>%
  group_split(Site) -> list_of_dfs
# Use the value from the "Species" column to provide a name for the list members
list_of_dfs %>%
  map(~pull(.,Site)) %>% # Pull out Species variable
  map(~unique(.)) -> names(list_of_dfs) # Set this as names for list members

list_of_dfs %>%
  writexl::write_xlsx(path = "Headwaters/Data/DO_time_series.xlsx")

df <- readRDS("Headwaters/Data/DO_time_series")


# Load point measurements
pts <- read_excel("Headwaters/Data/Field measurements/Field_data.xlsx") %>%
  left_join(filter(meta, type == "DO")) %>%
  rename(temp = `T (°C)`, DO = `DO (mg/L)`) %>%
  filter(Datetime > ymd("2020-01-01")) %>%
  distinct()

# Interactive dygraphs
# First need to get nest data, double nested
df_dy_n <- df %>%
  select(datetime, Subwatershed, Subwatershed_order, Site, DO, DO_temp) %>%
  group_by(Subwatershed) %>%
  nest() %>%
  mutate(by_subwatershed = map(data, ~.x %>%
                                 group_by(Site, Subwatershed_order) %>%
                                 nest()))

# First need to get data in correct timeseries format
df_dy <- df_dy_n %>%
  mutate(ts = map(by_subwatershed, "data") %>%
           map_depth(2, ~zoo::zoo(x = c(.$DO, .$DO_temp), 
                                  order.by = .$datetime)))

# Create a graphing function
graph_fun <- function(data, site = "sitename") {
  dygraph(data,
          main = site,
          width=800,height=200) %>%
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO sat. (%)",
           independentTicks = TRUE,
           valueRange = c(0, 160))  %>%
    dyAxis("y2", label = "T (deg C)", 
           valueRange = c(0, 25), 
           independentTicks = TRUE) %>%
    dySeries("DO_temp", axis=('y2')) %>%
    dyRangeSelector()
}

# Apply graph function to data
df_dy_p <- df_dy %>%
  mutate(p = map(by_subwatershed, "data") %>%
           map_depth(2, ~zoo::zoo(x = ., order.by = .$datetime)) %>%
           map_depth(2, ~graph_fun(., unique(.$Site)),
  ))



htmltools::browsable(htmltools::tagList(pluck(df_dy_p, 5)))













pluck(df_dy_p, 5, 2)


ylim.prim <- c(-0.5, 15)   
ylim.sec <- c(5, 30)

# Calculate the plot variables for the axes
b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

# Get x-axis breaks
xbrks <- pretty_dates(df$datetime, 10)

# Get 1% point of x length for label
dt_uni <- unique(df$datetime)
xpos <- dt_uni[order(as.POSIXct(dt_uni))][floor(0.01 * length(dt_uni))]

df <- df %>%
  mutate(DOsat = ifelse(DO_temp == 0,
                        0,
                        14.652 - 0.41022 * DO_temp + 0.007991 * 
                          DO_temp^2 - 0.000077774 * DO_temp^3),
         DO_per = DO * 100/ DOsat)
flood_sites1 <- c("Loise Feurs", "Mare Pont du diable", "Mare aval",
                  "Vizézy amont Bullieux", "Toranche aval", "Mare Azieux")
flood_sites2 <- c("Loise aval Poncins", "Loise Essertine en Donzy")
df_filt <- df %>%
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

# Plot
p <- ggplot() +
  geom_line(data = df, aes(x = datetime,
                           y = DO_per,
                           color = as.factor(Watershed_order))) +
  # geom_line(data = df, aes(x = datetime,
  #               y = a + temp * b,
  #               color = Subwatershed_order),
  #           linetype = "dashed") +
  scale_x_datetime(breaks = xbrks,
                   date_labels = "%d") +
  scale_y_continuous(limits = c(0, 150),
                     breaks = seq(0, 150, 50),
                     # sec.axis = sec_axis(~ (. - a) / b, 
                     #                     name = expression("Stream temperature "
                     #                                       *(degree*C))
                     #                     )
  ) +
  facet_grid(rows = vars(Watershed)) + 
  scale_color_viridis_d(name = "Watershed order") +
  geom_text(data = df, aes(x = ymd_hms(xpos),
                           y = -0.2,
                           label = Watershed,
                           hjust = "left"),
            size = 4) +
  # facet_wrap(~Site,
  #            labeller = label_wrap_gen(width = 18)) + 
  ylab(expression("DO (mg "*L^-1*")")) +
  th +
  theme(
    # panel.grid.major.x = element_line(colour = "light grey", 
    #                                           size = 1.4),
    # strip.text.x = element_text(size = 6,
    #                             margin = margin(0,0,0,0, "cm")),
    # axis.text.x = element_text(size = 6),
    strip.background.y = element_blank(),
    strip.text.y = element_blank()
    # , axis.line.y.right = element_line(color = "red"), 
    # axis.ticks.y.right = element_line(color = "red"),
    # axis.text.y.right = element_text(color = "red"), 
    # axis.title.y.right = element_text(color = "red")
  )

p

# Save
ggsave(filename = "Figures/initial_DO_timeseries_ordered_by_watershed.png",
       device = "png",
       dpi = 300,
       width = 16,
       height = 8,
       units = "in")

# plot DO data
# define limits to axes


# hourly data
# df_h <- df %>%
#   mutate(hour = date(datetime) + hours(hour(datetime))) %>%
#   group_by(Site, hour) %>%
#   summarize_at(vars(-group_cols(), 8:13), mean, na.rm = TRUE)
# 
# df_all <- df_h %>%
#   select(-datetime) %>%
#   rename(datetime = hour) %>%
#   left_join(read_excel("Headwaters/Data/Discharge/toranche.xlsx"))

# Calculate average daily amplitude by site
# df %>%
#   mutate(date = date(datetime)) %>%
#   dplyr::filter(DO > 1) %>%
#   group_by(Subwatershed, Subwatershed_order, Site, date, Location) %>%
#   summarize(mean = mean(DO, na.rm = TRUE),
#             max = max(DO, na.rm = TRUE),
#             min = min(DO, na.rm = TRUE),
#             amp = max - min) %>%
#   gather(msmt, val, amp) %>%
#   ggplot(aes(x = Subwatershed_order, y = val)) +
#   stat_summary(fun.y = mean, geom = "point") + 
#   stat_summary(fun.data = mean_se, geom = "errorbar") +
#   facet_wrap(~Subwatershed) +
#   scale_color_viridis_d() +
#   theme_bw() +
#   ylab('Mean daily DO amplitude')




# if(site %in% c("Toranche St Cyr les Vignes",
#                "Coise aval Montrond",
#                "Lignon aval Poncins",
#                "Mare aval")){
# dygraph(data,
#         main = site,
#         width=800,height=200) %>%
#   dyOptions(drawGrid = F,
#             useDataTimezone = TRUE) %>%
#   # dyAxis("y", label = "DO (mg L<sup>-1</sup>)",
#   #        independentTicks = TRUE,
#   #        valueRange = c(0, 14))  %>%
#   dyAxis("y", label = "DO sat. (%)",
#          independentTicks = TRUE,
#          valueRange = c(0, 130))  %>%
#   dyAxis("y2", label = "Temp", 
#          valueRange = c(0, 30), 
#          independentTicks = TRUE) %>%
#   dySeries("temp", axis=('y2')) %>%
#   dyRangeSelector()
#   
#   
# } else {
# 
# 
#     # dyAxis("y", label = "DO (mg L<sup>-1</sup>)",
#        independentTicks = TRUE,
#        valueRange = c(0, 14))  %>%
#        
#        
df_n <- df_all %>%
  mutate(DOsat = ifelse(DO_temp == 0,
                        0,
                        14.652 - 0.41022 * DO_temp + 0.007991 * 
                          DO_temp^2 - 0.000077774 * DO_temp^3),
         DO_per = DO * 100/ DOsat) %>%
  filter(year(datetime) == 2020) %>%
  select(-c(Watershed, Subwatershed, Subwatershed_order, Location)) %>%
  left_join(df %>% select(Site, Watershed, Subwatershed, Subwatershed_order, Location) %>%
              distinct()) %>%
  arrange(Watershed, Subwatershed, Subwatershed_order) %>%
  select(Subwatershed, Location, Site, DO_per, DO_temp, datetime, q_mmd) %>%
  # filter(Subwatershed %in% c("Coise", "Coizet", "Potenisinet", "Toranche",
  #                            "Loise", "Doise", "Moulin Piquet", "Fontbonne",
  #                            "Charpassonne", "Carrat", "Rieu", "Violay")) %>%
  group_by(Subwatershed, Location) %>%
  nest() %>%
  mutate(ts = map(data, ~zoo::zoo(x = .[, c("DO_per", "q_mmd")], 
                                  order.by = .$datetime)),
         p = map2(ts, data, ~graph_fun(.x, unique(.y$Site)),
         ))


htmltools::browsable(htmltools::tagList(pluck(df_n, 5)))

head(df_n)

head(df_dy2)









test <- df_dy_n %>% 
  mutate(by_subwatershed = map(by_subwatershed, ~.x %>% 
                                 mutate(s = map(by_site,
                                                ~plot_ly(data = .x, 
                                                         x = ~datetime, 
                                                         y = ~DO,
                                                         type = "line"
                                                )
                                 )
                                 )
  ))

t <- df_dy_n %>%
  mutate(dy = map_depth(by_subwatershed, 2, ~zoo::zoo(order.by = .$datetime)))


test %>% unnest(by_subwatershed) %>% slice(1:2)
test %>% unnest %>% filter(Site == "Toranche Pontet") %>% .$s
map(~{
  plot_ly(data = .x, x = ~datetime, y = ~DO, type = "line")
}) %>% 
  subplot(margin = .05)