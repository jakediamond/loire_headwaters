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
# th <- theme_bw() +
#   theme(
#     axis.title.x = element_blank(),
#     axis.text.x = element_text(angle = 90),
#     panel.grid = element_blank()
#   )

# Load data
df <- readRDS("Headwaters/Data/DO_time_series_wide")

# Load point measurements
pts <- read_excel("Headwaters/Data/Field measurements/Field_data.xlsx") %>%
  rename(temp = `T (°C)`, DO = `DO (mg/L)`) %>%
  # filter(Datetime > ymd("2020-01-01")) %>%
  distinct()

# Interactive dygraphs
# First need to get nest data
df_dy <- df %>%
  select(datetime, Watershed, Subwatershed, site_code, Site, DO, DO_temp, area_km2) %>%
  arrange(Watershed, Subwatershed, area_km2) %>%
  select(-Watershed, -area_km2) %>%
  group_by(Subwatershed, site_code) %>%
  nest()

# Get data in correct timeseries format
df_dy <- df_dy %>%
  mutate(ts = map(data, ~zoo::zoo(x =  .[, c("DO", "DO_temp")], 
                                   order.by = .$datetime)))

# Create a graphing function
graph_fun <- function(data, site = "sitename") {
  dygraph(data,
          main = site,
          width=800,height=200) %>%
    dyOptions(drawGrid = F,
              useDataTimezone = TRUE) %>%
    dyAxis("y", label = "DO (mg/L)",
           independentTicks = TRUE,
           valueRange = c(0, 18))  %>%
    dyAxis("y2", label = "T (deg C)", 
           valueRange = c(0, 25), 
           independentTicks = TRUE) %>%
    dySeries("DO_temp", axis=('y2')) %>%
    dyRangeSelector()
}

# Apply graph function to data
df_dy <- df_dy %>%
  mutate(p = map2(ts, site_code, graph_fun))



htmltools::browsable(htmltools::tagList(pluck(df_dy, 5)))




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
