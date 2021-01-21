# 
# Purpose: To grob the water chemistry data from SIMACoise
# Author: Jake Diamond
# Date: October 1, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(lubridate)
library(readxl)
library(scales)
library(tidyverse)


df <- read_xlsx("Headwaters/Data/water_chemistry/Coise_Toutes_Donnees.xlsx", 
                sheet = "Donnees") %>%
  filter(Anal_Param_Code %in%
           (read_xlsx("Headwaters/Data/water_chemistry/Coise_Toutes_Donnees.xlsx", 
                     sheet = "Paramètres_DCE") %>%
           pull(Param_Code))) %>%
  select(site = Code_Station_SIMACOISE, date = Prel_Date_Debut, time = Prel_Heure_Debut,
         parameter = Anal_Param_Libelle_Court, units = Anal_Unité_Libelle,
         value = Anal_Resultat, mdl = Anal_Limite_Quantification) %>%
  mutate(datetime = ymd_hm(paste(date,time)))

(ggplot(data = filter(df, grepl('COISE|Coise|Rieu|Pot', site)),
       aes(x = date,
           y = value,
           color = site,
           group = site)) +
  geom_point() + geom_line() +
    theme_bw() + guides(color = FALSE) +
  scale_color_viridis_d() +
  facet_wrap(~parameter, scales = "free")) %>%
  ggsave(filename = "Headwaters/Figures/SIMAcoise_first_look.png",
         dpi = 600,
         width = 18.4,
         height = 12,
         units = "cm")

write_excel_csv2(df, path = "Headwaters/Data/water_chemistry/simacoise_simple.csv")
  