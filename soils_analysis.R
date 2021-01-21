# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(scales)
library(readxl)
soils <- read_excel("Headwaters/Data/soils/donnees_Loire_metabo.xlsx",
                    sheet = 3) %>%
  left_join(read_excel("Headwaters/Data/soils/donnees_Loire_metabo.xlsx",
                       sheet = 1)) %>%
  pivot_longer(cols = `[COD] mg/L`:`SR`)

(ggplot(data = soils,
       aes(x = `land use`,
           y = value)) +
  theme_bw() +
  stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  facet_wrap(~name, scales = "free_y")) %>%
  ggsave(filename = "Headwaters/Figures/initial_soil_results_landuse.pdf",
         width = 36,
         height = 24,
         units = "cm",
         dpi = 600,
         device = "pdf")

(ggplot(data = soils,
        aes(x = site_name,
            y = value)) +
    theme_bw() +
    stat_summary(fun = mean, geom = "bar") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
    facet_wrap(~name, scales = "free_x") +
    coord_flip()) %>%
  ggsave(filename = "Headwaters/Figures/initial_soil_results_site.pdf",
         width = 36,
         height = 24,
         units = "cm",
         dpi = 600,
         device = "pdf")
