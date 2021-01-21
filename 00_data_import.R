# 
# Purpose: To grab all DO and light data in clean dataframes
# Author: Jake Diamond
# Date: July 20, 2020
# 

# Set working directory
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
setwd("C:/Users/diamo/Dropbox/Projects/Loire_headwaters")

# Load libraries
library(lubridate)
library(readxl)
library(fuzzyjoin)
library(tidyverse)

# Load data
# First get data path and names of files for DO
data_path_do <- "Headwaters/Data/HOBO_raw_csv"
files_do <- dir(data_path_do, pattern = "*.csv")

# Then get data path and names of files for light
data_path_light <- "Headwaters/Data/HOBO_light_raw_csv"
files_light <- dir(data_path_light, pattern = "*.csv")

# Then get the data for conductivity
data_path_cond <- "Headwaters/Data/HOBO_cond_raw_csv"
files_cond <- dir(data_path_cond, pattern = "*.csv")

# Then load all data into one dataframe
df <- tibble(path = c(rep(data_path_do, length(files_do)),
                      rep(data_path_light, length(files_light)),
                      rep(data_path_cond, length(files_cond))),
             filename = c(files_do, files_light, files_cond)) %>% 
  mutate(file_contents = map2(path, filename,
                              ~ read_csv(file.path(.x, .y),
                                         skip = 2,
                                         col_names = FALSE)),
         tz = map2(path, filename,
                   ~ read_csv2(file.path(.x, .y), n_max = 1) %>%
                     str_extract("(?i)(?<=GMT\\D)\\d+") %>%
                     as.numeric()
                   )
         ) %>%
  unnest(cols = c(file_contents, tz)) %>%
  select(-X5)

# Load metadata
meta <- read_excel("Headwaters/Data/Field measurements/sensor_metadata.xlsx",
                   sheet = "fuzzy_join") %>%
  mutate(sensor = as.character(sensor)) %>%
  mutate(date_start_utc = force_tzs(date_start, tzones = "Europe/Paris"),
         date_end_utc = force_tzs(date_end, tzones = "Europe/Paris"))

# add a column for west/east
side <- tibble(Watershed = c("Coise", "Loise", "Toranche", "Lignon", "Mare"),
               side = c("east", "east", "east", "west", "west"))

# Some data cleaning, make filename = sensor serial number
# Combine sites to sensors, but need to account for datetime...fuzzy join
df2 <- df %>%
  separate(filename, c("sensor", "recoverydate"), "_") %>%
  select(-X1) %>%
  mutate(X2 = mdy_hms(X2),
         Year = year(X2)) %>%
  rename(datetime = X2) %>%
  mutate(datetime_utc = force_tzs(datetime, tzones = paste0("Etc/GMT-", tz))) %>%
  filter(!(sensor == "20613788" & 
             recoverydate == "29juillet2020.csv" & 
             datetime_utc >= dmy_hms("28/07/2020 11:15:00"))) %>%
  filter(!(sensor == "20613789" & 
             recoverydate == "22juillet2020.csv" & 
             datetime_utc < dmy_hms("09/07/2020 11:00:00"))) %>%
  # filter(!(sensor == "20659163" & 
  #            recoverydate == "06aout2020.csv" & 
  #            datetime_utc < dmy_hms("09/07/2020 11:00:00"))) %>%
  distinct() %>% #some files included previous uploads...remove duplicates
  group_by(recoverydate, sensor) %>%
  nest() %>%
  mutate(min_date = map(data, ~min(.$datetime_utc)),
         max_date = map(data, ~max(.$datetime_utc))) %>%
  ungroup() %>%
  fuzzy_left_join(select(meta, Site, sensor, date_start_utc, date_end_utc, type),
                  by = c("sensor" = "sensor",
                         "min_date" = "date_start_utc",
                         "max_date" = "date_end_utc"),
                  match_fun = list(`==`, `>=`, `<=`)) %>%
  select(-sensor.y) %>%
  rename(sensor = sensor.x) %>%
  unnest(data) %>%
  distinct() #some files included previous uploads...remove duplicates

# Add back some meta data for each site
df3 <- df2 %>%
  ungroup() %>%
  rename(value = X3, temp = X4) %>%
  select(-sensor, -path, -Year, -recoverydate,
         -min_date, -max_date, -date_start_utc, -date_end_utc) %>%
  left_join(distinct(select(meta, -sensor, -date_start_utc, -date_end_utc, -type, -Year)),
            by = c("Site")) %>%
  left_join(side) %>%
  mutate(site_code = str_c(str_sub(Site, 1, 3),
                           str_pad(round(area_km2), 3, pad = "0"))) %>%
  mutate(side = fct_rev(side)) %>%
  filter(!is.na(type)) %>%
  distinct(Site, datetime, value, temp, .keep_all = TRUE)

# Get into wider format
df_w <- df3 %>%
  mutate(datetime = with_tz(datetime_utc, "Europe/Berlin")) %>%
  select(-datetime_utc, -tz, -date_start, -date_end) %>%
  pivot_wider(names_from = type, values_from = c(value, temp),
              values_fn = list(temp = mean,
                               value = mean)) %>%
  rename(DO = value_DO,
         DO_temp = temp_DO,
         lux_water = temp_light_water, #these are switched for light sensors
         lux_water_temp = value_light_water,
         lux_air = temp_light_air,
         lux_air_temp = value_light_air,
         cond = value_cond,
         cond_temp = temp_cond)

# Some data cleaning
df_w <- df_w %>%
  mutate_at(vars(11:18), funs(if_else(. < 0, NA_real_, .))) # get rid of negative values

# Get rid of crazy values
df_w <- df_w %>%
  mutate(DO = if_else(DO > 25, NA_real_, DO),
         DO_temp = if_else(DO_temp > 40, NA_real_, DO_temp),
         lux_air = if_else(lux_air > 100000, NA_real_, lux_air),
         lux_water = if_else(lux_water > 100000, NA_real_, lux_water),
         lux_air_temp = if_else(lux_air_temp > 40, NA_real_, lux_air_temp),
         lux_water_temp = if_else(lux_water_temp > 40, NA_real_, lux_water_temp),
         cond = if_else(cond < 0, NA_real_, cond),
         cond_temp = if_else(cond_temp > 40, NA_real_, cond_temp))

# Save data
saveRDS(df_w, "Headwaters/Data/DO_time_series_wide")
# Save as xlsx with sheets
# Split one data frame per site
df_w %>%
  group_split(Site) -> list_of_dfs
# name of each sheet will be the site
list_of_dfs %>%
  map(~pull(., Site)) %>%
  map(~unique(.)) -> names(list_of_dfs)

list_of_dfs %>%
  writexl::write_xlsx(path = "Headwaters/Data/DO_time_series.xlsx")
