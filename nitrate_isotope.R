# 
# Purpose: To analyze isotope data
# Author: Jake Diamond
# Date: October 8, 2020
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("//ly-lhq-srv/jake.diamond/Loire_DO")

# Load libraries
library(lubridate)
library(readxl)
library(tidyverse)

# Load data
df_iso <- read_xlsx("Headwaters/Data/water_chemistry/isotopes/isotope_results.xlsx") %>%
  select(-area) %>%
  left_join(ws_meta)

# Calculate ratio
df_iso <- mutate(df_iso, ratio = `18O` / `15N`)

# Look at area effect
xxx = df_iso %>%
  mutate(season = if_else(month %in% c(7,8), "summer", "winter")) %>%
  left_join(chem %>% select(site, datetime, DOC = `DOC-C`) %>% mutate(month = month(datetime))) %>%
  # left_join(select(df_uv, 3:11), by = c("site", "month")) %>%
  # left_join(select(df_sum, site = Site, month, DO_min) %>%
  #             ungroup() %>%
  #             group_by(site, month) %>%
  #             summarize(DOmin = mean(DO_min, na.rm = T))) %>%
  filter(watershed == "Loise") %>%
  filter(site != "Moulin Piquet Moulin Plat") %>%
  ggplot(aes(x = area_km2,
           y = `15N`/ log(NO3),
           fill = season)) +
  stat_summary(shape = 21) +
  theme_bw() +
  # scale_y_continuous(limits = c(0,15)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 4),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                limits = c(0.7, 1000)) +
  stat_smooth(method = "lm") +
  annotation_logticks(sides = "b") +
  scale_fill_manual(name = "season",
                    values = c("black", "white")) +
  labs(x = expression("area ("*km^2*")"),
       y = expression(delta^15*N*"–"*NO[3])) +
  theme(legend.position = c(0.25, 0.85),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill=alpha(0.01)),
        legend.key = element_rect(fill=alpha(0.01)))
xxx
ggsave(filename = "Headwaters/Figures/isotopes/summer_winter_15N.png",
       dpi = 600,
       width = 9.2,
       height = 9.2,
       units = "cm")

df_iso %>%
  mutate(season = if_else(month %in% c(7,8), "summer", "winter")) %>%
  select(site, watershed, `15N`, area = area_km2, season) %>%
  pivot_wider(names_from = season, values_from = `15N`, values_fn = mean) %>%
  mutate(diff = summer - winter) %>%
  ggplot(aes(x = area,
             y = diff)) +
  stat_summary(shape = 21) +
  theme_bw() +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 4),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                limits = c(0.7, 1000)) +
  annotation_logticks(sides = "b") +
  # scale_fill_manual(name = "season",
  #                   values = c("black", "white")) +
  labs(x = expression("area ("*km^2*")"),
       y = expression(delta^15*N*"–"*NO[3])) +
  theme(legend.position = c(0.25, 0.85),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill=alpha(0.01)),
        legend.key = element_rect(fill=alpha(0.01)))







df_mod <- df %>%
  # group_by(site) %>%
  nest_by(site, watershed, area) %>%
  mutate(mod = list(lm(`15N` ~ log(NO3), data = data))) %>%
  summarise(broom::tidy(mod))

df_mod %>%
  filter(term == "log(NO3)") %>%
  mutate(sig = if_else(p.value > 0.05, "p>0.05", "p<0.05")) %>%
  filter(watershed %in% c("Loise")) %>%
  ggplot(aes(x = log(area), y = estimate,
             color = watershed)) +
  geom_point() +
  scale_y_continuous(limits = c(-20,5)) +
  theme_bw()

filter(df_iso, month %in% c(7),
       watershed %in% ("Loise"))
ggplot(data = df_iso,
       aes(x = log(area_km2),
           y = ratio,
           color = watershed,
           group = site)) +
  stat_summary() +
  theme_bw()

# Quick plot
ggplot(data = df,
       aes(x = `15N`,
           y = `18O`,
           color = as.factor(month),
           size = NO3)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 0.5) +
  theme_bw() +
  # facet_wrap(~watershed) +
  scale_size_area() +
  scale_color_brewer(name = "month",
                     labels = c("february", "july", "august", "december"),
                     type = "qual", palette = "Dark2")
ggsave("Headwaters/Figures/isotope_ratio.png",
       width = 26,
       height = 18.4,
       units = "cm",
       dpi = 600)

ggplot(data = df_iso,
       aes(x = log(NO3),
           y = `15N`,
           color = as.factor(month))) +
  geom_point(alpha = 0.7) +
  theme_bw() +
  # facet_wrap(~site) +
  scale_color_brewer(name = "month",
                     labels = c("february", "july", "august", "december"),
                     type = "qual", palette = "Dark2")
ggsave("Headwaters/Figures/isotope_log.png",
       width = 26,
       height = 18.4,
       units = "cm",
       dpi = 600)


ggplot(data = df,
       aes(x = 1/(NO3),
           y = `15N`,
           color = as.factor(month))) +
  geom_point(alpha = 0.7) +
  theme_bw() +
  # facet_wrap(~watershed, scales = "free") +
  scale_color_brewer(name = "month",
                     labels = c("february", "july", "august", "december"),
                     type = "qual", palette = "Dark2")
ggsave("Headwaters/Figures/isotope_inverse.png",
       width = 26,
       height = 18.4,
       units = "cm",
       dpi = 600)


ggplot(data = df_iso,
       aes(x =NO3,
           y = `15N`,
           color = as.factor(month))) +
  geom_point(alpha = 0.7) +
  theme_bw() +
  # facet_wrap(~watershed, scales = "free") +
  scale_color_brewer(name = "month",
                     labels = c("february", "july", "august", "december"),
                     type = "qual", palette = "Dark2")

ggsave("Headwaters/Figures/isotope_inverse.png",
       width = 26,
       height = 18.4,
       units = "cm",
       dpi = 600)
