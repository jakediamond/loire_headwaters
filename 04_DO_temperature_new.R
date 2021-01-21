# -------------------------------------
# Author: 
# Purpose: 
# Date:
# -------------------------------------

# Compare leaf off and leaf on periods
df_DO_sum <- df_sum %>%
  group_by(watershed, site_code, Site, area, period, date, month, oow) %>%
  select(starts_with("DO")) %>%
  mutate(DO_amp = DO_max - DO_min,
         DO_per_amp = DO_per_max - DO_per_min,
         DO_temp_amp = DO_temp_max - DO_temp_min) %>%
  # select(-ends_with("mean")) %>%
  # pivot_longer(cols = starts_with("DO_per")) %>%
  # drop_na() %>%
  ungroup()

df_sum %>%
  filter(oow == "no") %>%
  filter(between(month, 6, 8)) %>%
  select(DO_temp_min, DO_temp_max) %>%
  pivot_longer(cols = c(DO_temp_min, DO_temp_max)) %>%
  ggplot(aes(x = area,
           y = value,
           fill = name)) +
  stat_summary(shape = 21) +
  theme_bw() +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 4),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                limits = c(1, 1000)) +
  annotation_logticks(sides = "b") +
  scale_fill_manual(name = "daily summer temp.",
                     values = c("black", "white"),
                     labels = c("max.", "min.")) +
  labs(x = expression("area ("*km^2*")"),
       y = expression("temperature ("*degree*C*")")) + 
  theme(legend.position = c(0.25, 0.85),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill=alpha(0.01)),
        legend.key = element_rect(fill=alpha(0.01)))
ggsave(filename = "Headwaters/Figures/DO_temp/daily_max_min_by_area.png",
       dpi = 600,
       width = 9.2,
       height = 9.2,
       units = "cm")

maxmin <- df_DO_sum %>%
  filter(between(month,3,9),
         name != "DO_per_amp") %>%
  ggplot(data = .,
         aes(x = DO_temp_min,
             y = value,
             color = log(area))) +
  facet_wrap(~name, ncol = 1) +
  # scale_x_binned() +
  stat_summary_bin(bins = 25) +
  stat_summary_bin(bins = 25, geom = "line") +
  geom_hline(yintercept = 100) +
  # scale_y_continuous(limits = c(80, 110)) +
  # scale_y_log10() +
  scale_color_distiller() +
  theme_bw()  +
  labs(x = "Maximum daily temperature (deg C)",
       y = "DO minimum (%)",
       title = "Effect of temperature on DO minimum",
       subtitle = "Early summer",
       caption = "data organized by deciles of min. temp.")
maxmin
ggsave("Headwaters/Figures/DO_temp/early_summer_min_vs_maxT.png",
       dpi = 600,
       height = 12.6,
       width = 18.2,
       units = "cm")



quibble <- function(x, y, q = seq(0.1, 0.9, 0.1)) {
  tibble(x = quantile(x, q, na.rm = TRUE), 
         y = quantile(y, q, na.rm = TRUE),
         q = q)
}
df_DO_sum %>% 
  group_by(site_code, month, area) %>% 
  summarise(quibble(DO_temp_min, DO_per_amp)) %>%
  filter(between(month, 3, 3)) %>%
  ggplot(data = .,
         aes(x = x,
             y = y,
             color = area,
             group = site_code)) +
  geom_point() +
  scale_color_viridis_c() +
  facet_wrap(~month, ncol = 1) +
  theme_bw()
  # geom_smooth(method = "lm")



x = df_DO_sum %>% 
  mutate(period2 = if_else(between(month, 3, 4), "spring_leafoff",
                           if_else(between(month, 5, 6), "early_summer", "summer"))) %>%
  group_by(site_code, period2, area) %>% 
  mutate(q_t = ntile(DO_temp_max, n = 10) / 10) %>%
  group_by(Subwatershed, site_code, period2, area, q_t) %>%
  summarize(temp_min = mean(DO_temp_max, na.rm = TRUE),
            DO_amp = mean(DO_per_max, na.rm  = TRUE)) %>%
  filter(period2 == "spring_leafoff") %>%
         # ,str_detect(Subwatershed, 'Coi|Cha|Car|Loi|Doi|Fon|Mou|Pot|Vio|Rie|Tor')) %>%
  ggplot(data = .,
         aes(x = temp_min,
             y = DO_amp,
             color = area,
             group = site_code)) +
  geom_point() +
  geom_line() +
  # scale_y_log10() +
  scale_color_distiller() +
  # annotation_logticks(sides = "l") +
  # scale_x_continuous(limits = c(10, 22)) +
  facet_wrap(~Subwatershed, ncol = 4) +
  theme_bw() +
  labs(x = "Maximum daily temperature (deg C)",
       y = "DO maximum (%)",
       title = "Effect of temperature on DO maximum",
       subtitle = "Spring",
       caption = "data organized by deciles of max. temp.")
x
ggsave("Headwaters/Figures/DO_temp/spring_max_vs_maxT.png",
       dpi = 600,
       height = 12.6,
       width = 18.2,
       units = "cm")

y = df_DO_sum %>% 
  mutate(period2 = if_else(between(month, 3, 4), "spring",
                           if_else(between(month, 5, 6), "summer", "summer_late"))) %>%
  group_by(site_code, period2, area) %>% 
  mutate(q_t = ntile(DO_temp_max, n = 10) / 10) %>%
  group_by(Subwatershed, site_code, period2, area, q_t) %>%
  summarize(temp_max = mean(DO_temp_max, na.rm = TRUE),
            DO_amp = mean(DO_per_min, na.rm  = TRUE)) %>%
  ungroup() %>%
  group_by(Subwatershed, site_code, period2, area) %>%
  filter(n() > 6) %>%
  ungroup() %>%
  nest_by(Subwatershed, site_code, period2, area) %>%
  mutate(mod = list(cor.test(~DO_amp + temp_max, method = "spearman", data = data))) %>%
  summarise(broom::tidy(mod))

z <- ggplot(data = y,
       aes(x = period2,
           y = estimate,
           color = log(area),
           alpha = p.value,
           group = site_code)) +
  geom_point() +
  geom_hline(yintercept = 0) + 
  scale_y_continuous(limits = c(-1,1)) +
  scale_alpha_continuous(range = c(1, 0.1),
                         limits = c(0, 0.05),
                         breaks = c(0.01, 0.05, 1)) +
  geom_line() +
  facet_wrap(~Subwatershed, ncol = 4) +
  theme_bw() +
  scale_color_distiller() +
  labs(x = "",
       y = "correlation",
       title = "Correlation between max. temp. and DO min.",
       subtitle = "Spearman correlation")
z
ggsave("Headwaters/Figures/DO_temp/correlation_min_vs_maxT.png",
       dpi = 600,
       height = 12.6,
       width = 18.2,
       units = "cm")


mod = lm(DO_amp ~ temp_min * site_code,
         data = x)
summary(mod)
plot(mod)
library(interactions)

interact_plot(mod, pred = temp_min, modx = site_code,
              colors= "Set3",
              plot.points = TRUE,
              partial.residuals = TRUE)
?interact_plot
