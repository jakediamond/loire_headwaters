


x= filter(df, site_code %in% c("Tor063", "Tor076")) %>%
  mutate(hour = hour(datetime),
         date = date(datetime)) %>%
  group_by(site_code, date, hour) %>%
  summarize(do = mean(DO, na.rm = T)) %>%
  arrange(date, hour)
x = x[1:24,]
  

ggplot(x, aes(x = hour, y=do, color = site_code)) + 
  geom_polygon(fill = NA, col = 1) +
  geom_point(size = 5) +
  theme_minimal() + 
  coord_polar() +
  scale_x_continuous("", breaks = 0:24, limits = c(0, 24)) #+
  # ylim(90, 115)