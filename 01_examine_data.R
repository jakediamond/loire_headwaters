# 
# Purpose: To do a first look at all the headwaters data
# Author: Jake Diamond
# Date: November 1, 2020
# 

# Set working directory
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
setwd("C:/Users/diamo/Dropbox/Projects/Loire_headwaters")

# Load libraries
library(htmltools)
library(plotly)
library(lubridate)
library(readxl)
library(scales)
library(tidyverse)

# Load data
df <- readRDS("Headwaters/Data/DO_time_series_wide")

# Load point measurements
pts <- read_excel("Headwaters/Data/Field measurements/Field_data.xlsx") %>%
  rename(DO_temp = `T (°C)`, DO = `DO (mg/L)`, DO_per = `DO (%)`, cond = `SC (us/cm)`) %>%
  distinct() %>%
  left_join(distinct(select(df, Site, site_code))) %>%
  mutate(site_code = if_else(site_code == "Le 009",
                             "Rie009",
                             site_code)) %>%
  mutate(site_code = if_else(site_code == "Le 025",
                             "Pot025",
                             site_code))

# Read in daily pressure data, which is sea level pressure
press <- read_csv("Headwaters/Data/weather/pressure.csv") %>%
  mutate(date = ymd(date))

# Load elevation data
elev <- read_excel("Headwaters/Data/Loire_headwaters_data_share.xlsx",
                   sheet = "watershed_meta") %>%
  select(Site = site, elev = altitude_m)

# Calculate %DO saturation
df <- df %>%
  mutate(date = date(datetime)) %>%
  left_join(press) %>%
  left_join(elev) %>%
  mutate(press_e = press*exp(-0.00012*press)) %>%
  mutate(DOsat = streamMetabolizer::calc_DO_sat(DO_temp, press_e),
         DOsat2 = if_else(DO_temp == 0,
                          0,
                          14.652 - 0.41022 * DO_temp + 0.007991 *
                            DO_temp^2 - 0.000077774 * DO_temp^3),
         DO_per = if_else(is.na(DOsat),
                          DO * 100 / DOsat2,
                          DO * 100/ DOsat)
         )

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
df_hour <- df %>%
  mutate(hour = floor_date(datetime, "hour")) %>%
  group_by(Watershed, Subwatershed, site_code, Site, area_km2, hour) %>%
  summarize(DO = mean(DO, na.rm = TRUE),
            DO_temp = mean(DO_temp, na.rm = TRUE),
            DO_per = mean(DO_per, na.rm = TRUE),
            cond = mean(cond, na.rm = TRUE),
            lux = mean(lux_water, na.rm = TRUE)) %>%
  rename(datetime = hour) %>%
  ungroup() %>%
  mutate(site_code = if_else(site_code == "Le 009",
                             "Rie009",
                             site_code)) %>%
  mutate(site_code = if_else(site_code == "Le 025",
                             "Pot025",
                             site_code))

# Now add discharge data
df_hour <- df_hour %>%
  left_join(df_match) %>%
  left_join(select(df_q, site_q, datetime, q_mmd), 
            by = c("site_q", "datetime"))

# Use threshold of daily temperature and %sat to see when out of water
df_oow <- df_hour %>%
  mutate(date = date(datetime)) %>%
  group_by(Site,date) %>%
  summarize(do_mean = quantile(DO_per, 0.1, na.rm = TRUE),
            do_amp = max(DO_per, na.rm = T) - min(DO_per, na.rm = T),
            wat_t = max(DO_temp, na.rm = T) - min(DO_temp, na.rm = T),
            q_mmd = mean(q_mmd, na.rm = TRUE)) %>%
  mutate(oow = if_else((do_mean > 90 & wat_t > 8 & q_mmd < 0.1 & between(month(date), 7, 10)) |
                         (do_mean > 90 & do_amp < 5 & q_mmd < 0.1 & between(month(date), 7, 10)), 
                       "yes", "no"))

# join this to dataframe
df_hour <- df_hour %>%
  mutate(date = date(datetime)) %>%
  left_join(select(df_oow, -q_mmd), 
            by = c("Site", "date"))

# Interactive plotly graphs
# First need to get nest data
df_ply <- df_hour %>%
  select(datetime, Watershed, Subwatershed, site_code, Site, DO, DO_temp, 
         DO_per, cond, lux, q_mmd, oow, area_km2) %>%
  arrange(Watershed, Subwatershed, area_km2) %>%
  select(-Watershed, -area_km2) %>%
  group_by(Subwatershed, site_code) %>%
  nest()

# Create a graphing function
graph_fun <- function(data, y1 = "DO", y2 = "DO_temp", site = "sitename") {
  
  p_y1 <- parse(text = y1)
  p_y2 <- parse(text = y2)
  
  # Have to put NAs so that it doesn't connect gapped observations
  data = arrange(data, datetime) %>%
    select(datetime, {{ y1 }}, {{ y2 }}, oow)
  data_oow = distinct(data, datetime) %>%
    left_join(filter(data, oow == "yes"))
  data_inw = distinct(data, datetime) %>%
    left_join(filter(data, oow == "no"))
  
  # Subset the points dataframe for the site
  pts_site = filter(pts, site_code == site) %>%
    select(Datetime,{{ y1 }})
  
  # yaxis titles
  yaxis_1 = if_else(y1 == "DO", "DO (mg/L)",
                    if_else(y1 == "DO_per",
                            "DO (% sat.)",
                            "specific conductivity (uS/cm)"))
  yaxis_2 = if_else(y2 == "DO_temp", "temp. (deg C)",
                    if_else(y2 == "q_mmd",
                            "Q (mm/d)",
                            "lux"))
  
  plot_ly() %>%
    add_trace(data = data_inw,
              x=~datetime, y=~eval(p_y1), type = "scatter", mode='lines',
              color = I("black"), linetype = I("solid"),
              showlegend = FALSE) %>%
    add_trace(data = data_oow,
              x=~datetime, y=~eval(p_y1), type = "scatter", mode='lines',
              color = I("darkgrey"), linetype = I("dot"),
              showlegend = FALSE) %>%
    add_trace(data = data_inw,
              x=~datetime, y=~eval(p_y2), type = "scatter", mode='lines',
              color = I("red"), linetype = I("solid"),
              yaxis="y2",
              showlegend = FALSE) %>%
    add_trace(data = data_oow,
              x=~datetime, y=~eval(p_y2), type = "scatter", mode='lines',
              color = I("tomato"), linetype = I("dot"),
              yaxis="y2",
              showlegend = FALSE) %>%
    add_trace(data = pts_site,
              color = I("black"),
              x=~Datetime, y=~eval(p_y1), type = "scatter", mode = 'markers',
              showlegend = FALSE) %>%
    # add_trace(data = pts_site,
    #           x=~Datetime, y=~eval(p_y2), type = "scatter", mode = 'markers',
    #           color = I("red"),
    #           yaxis = 'y2',
    #           showlegend = FALSE) %>%
    add_segments(data = pts_site,
                 x=~Datetime, xend =~Datetime,
                 y = 0, yend = 15, type = 'scatter', mode = 'lines',
                 linetype = I("solid"),
                 color = I("black"), name = 'site visit') %>%
    layout(yaxis2 = list(overlaying = "y", side = "right",
                         title = yaxis_2,
                         tickfont = list(color = "red"),
                         titlefont = list(color = "red")),
           xaxis = list(title = ""),
           yaxis = list(title = yaxis_1),
           title = site)
}

# Apply graph function to data
df_ply <- df_ply %>%
  mutate(p_do_temp = pmap(list(data, site = site_code),
                  graph_fun),
         p_doper_dis = pmap(list(data, "DO_per", "q_mmd", site_code),
                            graph_fun),
         p_cond_dis = pmap(list(data, "cond", "q_mmd", site_code),
                           graph_fun))
# Check out the graphs
htmltools::browsable(htmltools::tagList(pluck(df_ply, 4)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 5)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 6)))

# Save the data
savedata <- select(df_ply, site_code, data)
saveRDS(savedata, file = "Headwaters/Data/headwaters_data_part2")
