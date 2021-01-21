# 
# Purpose: To get all discharge data for Loire headwaters
# Author: Jake Diamond
# Date: October 10, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(readxl)
library(lubridate)
library(tidyverse)

# Load function for banquehydro
source("loire_headwaters_code/fc.rbanquehydro.get.r")

# Load meta data
meta <- read_xlsx("Headwaters/Data/Discharge/Headwater_discharge_banque_hydro_codes.xlsx")

#Special get function
get_q <- function(code, start, end) {
  # start = paste0("01/10/", start, " 00:00")
  # end = paste0("20/09/", end, " 23:59")
  result = rbanqhydro.get(station = code, 
                 DateHeureDeb = start, 
                 DateHeureFin = end, 
                 procedure = "QTVAR")
  result
}

# Safe function
safe_get_q <- safely(get_q, otherwise = NA_real_)

df <- meta %>%
  mutate(q = map(Code, ~safe_get_q(.x, 
                                   "01/03/2018 00:00",
                                   "20/08/2020 23:59")))

df <- rbanqhydro.get("K0773220", "01/03/2018 00:00", "20/06/2020 23:59") %>%
  bind_rows(rbanqhydro.get("K0643110", "01/03/2018 00:00", "20/06/2020 23:59")) %>%
  bind_rows(rbanqhydro.get("K0704510", "01/03/2018 00:00", "20/06/2020 23:59")) %>%
  bind_rows(rbanqhydro.get("K0673310", "01/03/2018 00:00", "20/06/2020 23:59")) %>%
  bind_rows(rbanqhydro.get("K0663310", "01/03/2018 00:00", "20/06/2020 23:59")) %>%
  bind_rows(rbanqhydro.get("K0690010", "01/03/2018 00:00", "20/06/2020 23:59")) %>%
  bind_rows(rbanqhydro.get("K0700010", "01/03/2018 00:00", "20/06/2020 23:59")) %>%
  bind_rows(rbanqhydro.get("K0714010", "01/03/2018 00:00", "20/06/2020 23:59")) %>%
  bind_rows(rbanqhydro.get("K0733220", "01/03/2018 00:00", "20/06/2020 23:59")) %>%
  bind_rows(rbanqhydro.get("K0753210", "01/03/2018 00:00", "20/06/2020 23:59")) %>%
  bind_rows(rbanqhydro.get("K0763310", "01/03/2018 00:00", "20/06/2020 23:59")) %>%
  bind_rows(rbanqhydro.get("K0773210", "01/03/2018 00:00", "20/06/2020 23:59"))
  
  
  
# 
# 
# meta %>%
#   group_by(Site) %>%
#   summarize(start = seq(start_year, end_year - 4, 4))
