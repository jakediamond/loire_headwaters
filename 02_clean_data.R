# -------------------------------------
# Author: Jacob Diamond
# Purpose: Clean up the data based on previous visual examination
# Date: November 11, 2020
# -------------------------------------

# Set working directory
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
setwd("C:/Users/diamo/Dropbox/Projects/Loire_headwaters")

# Load libraries
library(lubridate)
library(fuzzyjoin)
library(broom)
library(tidyverse)

# load data
df <- readRDS("Headwaters/Data/headwaters_data_part2") %>%
  unnest() %>%
  mutate(site_code = tolower(site_code))


# Load metadata for cleaning
clean_meta <- read_csv("Headwaters/Data/data_clean.csv") %>%
  mutate(start = mdy_hm(start),
         end = mdy_hm(end))

# Join the data and the periods of time for cleaning data
df_clean <- df %>%
  fuzzy_left_join(clean_meta,
                  by = c("site_code" = "site_code",
                         "datetime" = "start",
                         "datetime" = "end"),
                  match_fun = list(`==`, `>=`, `<=`))

# First delete data that we are supposed to delete
df_clean2 <- df_clean %>%
  select(-site_code.y) %>%
  rename(site_code = site_code.x) %>%
  filter(!(type == "delete") | is.na(type)) %>%
  arrange(site_code, datetime)

# Remove the linear trend from times when we had sensor drift
df_linear <- df_clean2 %>%
  ungroup() %>%
  filter(type == "linear") %>%
  distinct(site_code, start, end) %>%
  mutate(period = row_number()) %>%
  left_join(df_clean2 %>%
              filter(type == "linear")) %>%
  group_by(site_code, period) %>%
  mutate(index = row_number()) %>%
  nest() %>%
  mutate(mod_DO = map(data, ~lm(.$DO ~ .$index)),
         slope_DO = map(mod_DO, tidy)) %>%
  unnest(slope_DO) %>%
  filter(term == ".$index") %>%
  select(site_code, data, period, estimate) %>%
  mutate(new_DO = map2(data, estimate, ~.x$DO - .y * .x$index))

# Now add that data back
df_final <- df_linear %>%
  mutate(DOsat = map(data, ~.$DO /.$DO_per * 100),
         data_new = pmap(list(data, new_DO, DOsat), ~mutate(..1, DO = ..2, DOsat = ..3))) %>%
  select(site_code, data_new) %>%
  unnest(data_new) %>%
  ungroup() %>%
  select(-period, -index) %>%
  mutate(DO_per = DO * 100 / DOsat) %>% #need to recalculate
  select(-DOsat) %>%
  bind_rows(df_clean2 %>%
              ungroup() %>%
              filter(is.na(type))) %>%
  select(-start, -end, -type) %>%
  arrange(Subwatershed, site_code, datetime)
  
saveRDS(df_final, "Headwaters/Data/headwaters_data_clean")  

