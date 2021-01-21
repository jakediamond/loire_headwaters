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
library(htmltools)
library(plotly)
library(lubridate)
library(readxl)
library(scales)
library(tidyverse)

# Load data
df <- readRDS("Headwaters/Data/DO_time_series_wide")

# # Load point measurements
# pts <- read_excel("Headwaters/Data/Field measurements/Field_data.xlsx") %>%
#   rename(temp = `T (°C)`, DO = `DO (mg/L)`) %>%
#   # filter(Datetime > ymd("2020-01-01")) %>%
#   distinct()

# Calculate %DO saturation
df <- df %>%
  mutate(DOsat = if_else(DO_temp == 0,
                         0,
                         14.652 - 0.41022 * DO_temp + 0.007991 * 
                           DO_temp^2 - 0.000077774 * DO_temp^3),
         DO_per = DO * 100/ DOsat)

# Use threshold of daily temperature and %sat to see when out of water
df_oow <- df %>%
  mutate(date = date(datetime)) %>%
  group_by(Site,date) %>%
  summarize(do_mean = mean(DO_per, na.rm = TRUE),
            do_amp = max(DO_per, na.rm = T) - min(DO_per, na.rm = T),
            wat_t = max(DO_temp, na.rm = T) - min(DO_temp, na.rm = T)) %>%
  mutate(oow = if_else((do_mean > 90 & wat_t > 5) |
                         (do_mean > 90 & do_amp < 2) |
                         (wat_t > 8), "yes", "no"))

# join this to dataframe
df <- df %>%
  mutate(date = date(datetime)) %>%
  left_join(df_oow)

# Load discharge data
# List all files
data_path <- "Headwaters/Data/Discharge"
files <- dir(data_path, pattern = "*hourly.csv")

# Read in all discharge data
df_q <- tibble(filename = files) %>% 
  mutate(file_contents = map(filename,
                             ~read_csv2(file.path(data_path, .)))
         ) %>%
  unnest()

# Data cleaning
df_q <- df_q %>%
  left_join(read_csv2("Headwaters/Data/Discharge/hourly_discharge_meta.csv")) %>%
  mutate(q_mmd = q_m3s / (area_km2 * 1000^2) * 86400 * 1000,
         datetime = dmy_hm(datetime),
         site = if_else(is.na(site),
                        "Vizezy Poncins",
                        site)) %>%
  rename(site_q = site)

# add watershed info to match to DO data
df_match <- read_xlsx("Headwaters/Data/Discharge/hourly_discharge_match.xlsx")

# Add discharge data to DO data, but first average by hour
df <- df %>%
  mutate(hour = floor_date(datetime, "hour")) %>%
  group_by(Watershed, Subwatershed, site_code, Site, oow, area_km2, hour) %>%
  summarize(DO = mean(DO, na.rm = TRUE),
            DO_temp = mean(DO_temp, na.rm = TRUE),
            DO_per = mean(DO_per, na.rm = TRUE)) %>%
  rename(datetime = hour) %>%
  ungroup() %>%
  mutate(site_code = if_else(site_code == "Le 009",
                             "Rie009",
                             site_code)) %>%
  mutate(site_code = if_else(site_code == "Le 025",
                             "Pot025",
                             site_code))

# Now add discharge data
df <- df %>%
  left_join(df_match) %>%
  left_join(select(df_q, site_q, datetime, q_mmd), 
            by = c("site_q", "datetime"))

# Interactive plotly graphs
# First need to get nest data
df_ply <- df %>%
  select(datetime, Watershed, Subwatershed, site_code, Site, DO, DO_temp, 
         DO_per, q_mmd,oow, area_km2) %>%
  arrange(Watershed, Subwatershed, area_km2) %>%
  select(-Watershed, -area_km2) %>%
  group_by(Subwatershed, site_code) %>%
  nest()

# Create a graphing function
graph_fun_do_temp <- function(data, site = "sitename") {

  # Have to put NAs so that it doesn't connect gapped observations
  data = arrange(data, datetime)
  data_oow = distinct(data, datetime) %>%
    left_join(filter(data, oow == "yes"))
  data_inw = distinct(data, datetime) %>%
    left_join(filter(data, oow == "no"))

  plot_ly() %>%
    add_trace(data = data_inw,
              x=~datetime, y=~DO, type = "scatter", mode='lines',
              color = I("black"), linetype = I("solid"),
              showlegend = FALSE) %>%
    add_trace(data = data_oow,
              x=~datetime, y=~DO, type = "scatter", mode='lines',
              color = I("darkgrey"), linetype = I("dot"),
              showlegend = FALSE) %>%
    add_trace(data = data_inw,
              x=~datetime, y=~DO_temp, type = "scatter", mode='lines',
              color = I("red"), linetype = I("solid"),
              yaxis="y2",
              showlegend = FALSE) %>%
    add_trace(data = data_oow,
              x=~datetime, y=~DO_temp, type = "scatter", mode='lines',
              color = I("tomato"), linetype = I("dot"),
              yaxis="y2",
              showlegend = FALSE) %>%
    layout(yaxis2 = list(overlaying = "y", side = "right",
                         title = "temp. (deg C)",
                         tickfont = list(color = "red"),
                         titlefont = list(color = "red")),
           xaxis = list(title = ""),
           yaxis = list(title = "DO (mg/L)"),
           title = site)
}

# Apply graph function to data
df_ply <- df_ply %>%
  mutate(p = map2(data, site_code, graph_fun_do_temp))


pluck(df_ply, 4, 2)
htmltools::browsable(htmltools::tagList(pluck(df_ply, 4)))


# Create a graphing function for discharge and percent sat
graph_fun_do_q <- function(data, site = "sitename") {
  
  # Have to put NAs so that it doesn't connect gapped observations
  data = arrange(data, datetime)
  data_oow = distinct(data, datetime) %>%
    left_join(filter(data, oow == "yes"))
  data_inw = distinct(data, datetime) %>%
    left_join(filter(data, oow == "no"))
  
  plot_ly() %>%
    add_trace(data = data_inw,
              x=~datetime, y=~DO_per, type = "scatter", mode='lines',
              color = I("black"), linetype = I("solid"),
              showlegend = FALSE) %>%
    add_trace(data = data_oow,
              x=~datetime, y=~DO_per, type = "scatter", mode='lines',
              color = I("darkgrey"), linetype = I("dot"),
              showlegend = FALSE) %>%
    add_trace(data = data,
              x=~datetime, y=~q_mmd, type = "scatter", mode='lines',
              color = I("darkblue"), linetype = I("solid"),
              yaxis="y2",
              showlegend = FALSE) %>%
    layout(yaxis2 = list(overlaying = "y", side = "right",
                         title = "Q (mm/d)",
                         tickfont = list(color = "darkblue"),
                         titlefont = list(color = "darkblue")),
           xaxis = list(title = ""),
           yaxis = list(title = "DO (% sat.)"),
           title = site)
}


df_ply <- df_ply %>%
  mutate(p_q = map2(data, site_code, graph_fun_do_q))

htmltools::browsable(htmltools::tagList(pluck(df_ply, 4)))

colnames(df_ply)
pluck(df_ply, 4, 2)







# also note when sites were flooded?
# flood_sites1 <- c("Loise Feurs", "Mare Pont du diable", "Mare aval",
# "Vizézy amont Bullieux", "Toranche aval", "Mare Azieux")
# flood_sites2 <- c("Loise aval Poncins", "Loise Essertine en Donzy")
# df_filt <- df %>%
#   filter(!(Site %in% flood_sites1 & between(datetime, 
#                                             ymd_hm("2019-08-06 16:00"),
#                                             ymd_hm("2019-08-29 16:45"))),
#          !(Site %in% flood_sites2 & between(datetime, 
#                                             ymd_hm("2019-08-06 16:00"),
#                                             ymd_hm("2019-08-30 10:30"))),
#          !(Site == "Coise aval Montrond" & between(datetime, 
#                                                    ymd_hm("2019-08-06 16:00"),
#                                                    ymd_hm("2019-09-11 11:30"))),
#          !(Site == "Loise amont Doise Salt" & between(datetime, 
#                                                       ymd_hm("2019-07-16 00:30"),
#                                                       ymd_hm("2019-07-20 20:00"))),
#          !(Site == "Loise Essertine en Donzy" & between(datetime, 
#                                                         ymd_hm("2019-07-20 22:15"),
#                                                         ymd_hm("2019-07-23 15:30"))),
#          !(Site == "Vizézy amont Bullieux" & between(datetime, 
#                                                      ymd_hm("2019-07-08 19:00"),
#                                                      ymd_hm("2019-07-16 14:15"))),
#          !(Site == "Vizézy amont Bullieux" & between(datetime, 
#                                                      ymd_hm("2019-07-24 23:00"),
#                                                      ymd_hm("2019-08-29 15:30"))),
#          !(Site == "Toranche Pontet" & between(datetime, 
#                                                ymd_hm("2019-07-12 19:45"),
#                                                ymd_hm("2019-07-21 17:30"))),
#          !(Site == "Toranche Pontet" & between(datetime, 
#                                                ymd_hm("2019-07-23 22:45"),
#                                                ymd_hm("2019-07-29 01:00"))),
#          !(Site == "Toranche Pontet" & between(datetime, 
#                                                ymd_hm("2019-07-31 03:45"),
#                                                ymd_hm("2019-08-06 17:30"))),
#          !(Site == "Doise" & between(datetime, 
#                                      ymd_hm("2019-07-14 15:00"),
#                                      ymd_hm("2019-07-20 17:15"))),
#          !(Site == "Doise" & between(datetime, 
#                                      ymd_hm("2019-08-03 20:15"),
#                                      ymd_hm("2019-08-06 12:30")))
#          
#   )



# Interactive dygraphs
# First need to get nest data
df_dy_filt <- df_filt %>%
  select(datetime, Watershed, Subwatershed, site_code, Site, DO_per, DO_temp, area_km2) %>%
  arrange(Watershed, Subwatershed, area_km2) %>%
  select(-Watershed, -area_km2) %>%
  group_by(Subwatershed, site_code) %>%
  nest()

# Get data in correct timeseries format
df_dy_filt <- df_dy_filt %>%
  mutate(ts = map(data, ~zoo::zoo(x =  .[, c("DO_per", "DO_temp")], 
                                  order.by = .$datetime)))

# Create a graphing function
graph_fun2 <- function(data, site = "sitename") {
  dygraph(data,
          main = site,
          width=800,height=200) %>%
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (%)",
           independentTicks = TRUE,
           valueRange = c(0, 180))  %>%
    dyAxis("y2", label = "T (deg C)", 
           valueRange = c(0, 30), 
           independentTicks = TRUE) %>%
    dySeries("DO_temp", axis=('y2')) %>%
    dyRangeSelector() %>%
    dyLegend(show = "never")
}

# Apply graph function to data
df_dy_filt <- df_dy_filt %>%
  mutate(p = map2(ts, site_code, graph_fun2))



htmltools::browsable(htmltools::tagList(pluck(df_dy_filt, 5)))
