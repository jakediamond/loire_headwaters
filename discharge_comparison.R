# 
# Purpose: To compare discharge among headwater sites
# Author: Jake Diamond
# Date: November 4, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(lubridate)

# List all files
data_path <- "Headwaters/Data/Discharge/QTFIX"
files <- dir(data_path, pattern = "*.txt")

# Read in all discharge data
df <- tibble(filename = files) %>% 
  mutate(file_contents = map(filename,
                             ~read_delim(file.path(data_path, .),
                                         delim = ";", skip = 41))
         ) %>%
  unnest()

# Data cleaning
df <- df %>%
  rename_all(.funs = list(str_trim))
df <- df %>%
  mutate(Qls = as.numeric(Qls),
         Qmmj = as.numeric(Qmmj),
         date = ymd_hm(df$Date),
         site = word(filename, 1, sep = "_"))

# Quick comparison of specific discharge
p_q_comp <- df %>%
  filter(site %in% c("K0673310", "K0643110", "K0704510", "K0773220"),
         year(date) == 2018) %>%
  mutate(datetime = date, 
         date = date(datetime)) %>%
  group_by(site, date) %>%
  summarize(Qs = mean(Qmmj)) %>%
  ggplot(aes(x = date,
                       y = Qs,
                       color = site))+
  geom_line() + 
  ylab("specific discharge (mm/d)") +
  xlab("") +
  scale_color_manual(labels = c("La Mare",
                                "La Coise",
                                "La Toranche",
                                "Le Lignon"),
                     values = c("black",
                                "red",
                                "orange",
                                "grey")) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8))
p_q_comp
ggsave(plot = p_q_comp,
       filename = "Headwaters/Figures/specific_discharge_comparison_2018.png",
       dpi = 600,
       width = 18.4,
       height = 12,
       units = "cm")

# Data spread to wide format
df_w <- df %>%
  select(date, site, Qls) %>%
  pivot_wider(names_from = site,
              values_from = Qls,
              values_fill = list(Qls = NA))
#   
# df_q <- df
# df_w2 <- select(df_w, -date)
# 
# df_w3 <- df_w2 %>%
#   select(1,3) %>%
#   drop_na()
# 
# plot(df_w3)
# 
# plot(df_w2[,1], df_w2[,2])

# Mean summer flow
df_d <- df %>%
  rename(datetime = date) %>%
  mutate(date = date(datetime)) %>%
  group_by(site, date) %>%
  summarize(Q = mean(Qmmj, na.rm = TRUE)) %>%
  mutate(month = month(date)) %>%
  filter(between(month, 7, 10)) %>%
  summarize(sumflow = mean(Q, na.rm = TRUE),
            sdflow = sd(Q, na.rm = TRUE))

# Names of sites
df_sites <- tibble(site = c("K0673310", "K0714010", "K0643110", "K0704510",
                            "K0733220", "K0773210", "K0773220", "K0753210",
                            "K0763310"),
                   name = c("La Coise Moulin Brûlé",
                            "La Loise à Salt-en-Donzy",
                            "La Mare Vérines",
                            "La Toranche à Saint-Cyr-les-Vignes",
                            "Le Lignon Chevelières",
                            "Le Lignon de Chalmazel à Poncins1",
                            "Le Lignon de Chalmazel à Poncins2",
                            "Le Lignon à Boën",
                            "Le Vizezy à La Guillanche"))
df_all <- left_join(df_d, df_sites)

ggplot(df_all,
       aes(x = name,
           y = sumflow)) +
  geom_col()

# df_d_w <- df_d %>%
#   pivot_wider(names_from = site,
#               values_from = Q,
#               values_fill = list(Q = NA)) %>%
#   arrange(date)
# 
# plot(df_d_w$K0704510[16800:17159])
# points(df_d_w$K0673310[16800:17159], col ="red")
# points(df_d_w$K0643110[16800:17159], col ="green")
# 
# 
# df_w3 <- df_d_w %>%
#   select(3,5) %>%
#   drop_na()
# 
# plot(df_w3)
# summary(lm(df_w3))
# 
# 
# 
# df_q <- df_q %>%
#   rename(datetime = date) %>%
#   mutate(date = date(datetime))