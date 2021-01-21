


df_DO <- readRDS("Headwaters/Data/DO_time_series")

# Get rid of bad data
# First, when sensors were buried or when out of water
flood_sites1 <- c("Loise Feurs", "Mare Pont du diable", "Mare aval",
                  "Vizézy amont Bullieux", "Toranche aval", "Mare Azieux")
flood_sites2 <- c("Loise aval Poncins", "Loise Essertine en Donzy")
df_DO <- df_DO %>%
  filter(!(Site %in% flood_sites1 & between(datetime,
                                            ymd_hm("2019-08-06 16:00"),
                                            ymd_hm("2019-08-29 16:45"))),
         !(Site %in% flood_sites2 & between(datetime,
                                            ymd_hm("2019-08-06 16:00"),
                                            ymd_hm("2019-08-30 10:30"))),
         !(Site == "Coise aval Montrond" & between(datetime,
                                                   ymd_hm("2019-08-06 16:00"),
                                                   ymd_hm("2019-09-11 11:30"))),
         !(Site == "Loise amont Doise Salt" & between(datetime,
                                                      ymd_hm("2019-07-16 00:30"),
                                                      ymd_hm("2019-07-20 20:00"))),
         !(Site == "Loise aval Doise Salt" & between(datetime,
                                                     ymd_hm("2019-08-06 19:00"),
                                                     ymd_hm("2019-08-30 10:15"))),
         !(Site == "Loise Essertine en Donzy" & between(datetime,
                                                        ymd_hm("2019-07-20 22:15"),
                                                        ymd_hm("2019-07-23 15:30"))),
         !(Site == "Vizézy amont Bullieux" & between(datetime,
                                                     ymd_hm("2019-07-08 19:00"),
                                                     ymd_hm("2019-07-16 14:15"))),
         !(Site == "Vizézy amont Bullieux" & between(datetime,
                                                     ymd_hm("2019-07-24 23:00"),
                                                     ymd_hm("2019-08-29 15:30"))),
         !(Site == "Toranche Pontet" & between(datetime,
                                               ymd_hm("2019-07-12 19:45"),
                                               ymd_hm("2019-07-21 17:30"))),
         !(Site == "Toranche Pontet" & between(datetime,
                                               ymd_hm("2019-07-23 22:45"),
                                               ymd_hm("2019-07-29 01:00"))),
         !(Site == "Toranche Pontet" & between(datetime,
                                               ymd_hm("2019-07-31 03:45"),
                                               ymd_hm("2019-08-06 17:30"))),
         !(Site == "Doise" & between(datetime,
                                     ymd_hm("2019-07-14 15:00"),
                                     ymd_hm("2019-07-20 17:15"))),
         !(Site == "Doise" & between(datetime,
                                     ymd_hm("2019-08-03 20:15"),
                                     ymd_hm("2019-08-06 12:30")))

  )

# Then really bad data
df_DO <- df_DO %>%
  mutate(DO = if_else(DO > 20 | DO < 0, NA_real_, DO),
         DO_temp = if_else(DO_temp > 35 | DO_temp < 0, NA_real_, DO_temp))

# Calculate saturation
df_DO <- df_DO %>%
  mutate(DOsat = if_else(DO_temp == 0,
                         0,
                         14.652 - 0.41022 * DO_temp + 0.007991 * 
                           DO_temp^2 - 0.000077774 * DO_temp^3),
         DO_per = DO * 100/ DOsat)

# Nest data
df_do_n <- df_DO %>%
  mutate(year = year(datetime)) %>%
  group_by(Site, year) %>%
  nest()

# Plotting function
plot_fun <- function(data){
  # site name and year
  site = paste(unique(data$Subwatershed), unique(data$Location))
  year = unique(year(data$datetime))
  # plot DO data
  # define limits to axes
  ylim.prim = c(-0.5, max(data$DO, na.rm = TRUE))   
  ylim.sec = c(min(data$DO_temp, na.rm = TRUE),
                max(data$DO_temp, na.rm = TRUE))
  
  # Calculate the plot variables for the axes
  b = diff(ylim.prim)/diff(ylim.sec)
  a = b*(ylim.prim[1] - ylim.sec[1])
  
  # Get x-axis breaks
  xbrks = pretty_dates(data$datetime, 10)
  
  # Get 1% point of x length for label
  dt_uni = unique(data$datetime)
  xpos = dt_uni[order(as.POSIXct(dt_uni))][floor(0.01 * length(dt_uni))]
  
  # Plot
  p = ggplot(data = data) +
    geom_line(aes(x = datetime,
                  y = DO),
              color = "black") +
    geom_line(aes(x = datetime,
                  y = a + DO_temp * b),
              color = "red",
              alpha = 0.5)  +
    scale_x_datetime(breaks = xbrks,
                     date_labels = "%d%b") +
    scale_y_continuous(limits = ylim.prim,
                       breaks = seq(0, ylim.prim[2], 3),
                       sec.axis = sec_axis(~ (. - a) / b, 
                                           name = expression("stream temperature "
                                                             *(degree*C))
                                           )
                       ) +
    ylab(expression("DO (mg "*L^-1*")")) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
      strip.background.y = element_blank(),
      strip.text.y = element_blank(),
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.text.y.right = element_text(color = "red"), 
      axis.title.y.right = element_text(color = "red")) +
    ggtitle(paste(site, year))
  
  # Determine data for inset
  # df_inset = filter(data,
  #                   between(datetime, 
  #                           ymd_hm(year(datetime)),
  #                           ymd_hm()))
  # 
  # ## A function to plot the inset 
  # get_inset = function(inset_data){
  #   p_i = ggplot(data=inset_data) +
  #     geom_line(aes(x = datetime,
  #                   y = DO),
  #               color = "black") +
  #     geom_line(aes(x = datetime,
  #                   y = a + DO_temp * b),
  #               color = "red") +
  #     scale_x_datetime(breaks = xbrks,
  #                      date_labels = "%d") +
  #     scale_y_continuous(limits = ylim.prim,
  #                        breaks = seq(0, ylim.prim[2], 3),
  #                        sec.axis = sec_axis(~ (. - a) / b, 
  #                                            name = expression("stream temperature "
  #                                                              *(degree*C))
  #                        )
  #     ) +
  #     ylab(expression("DO (mg "*L^-1*")")) +
  #     th +
  #     theme(axis.title = element_blank(),
  #       strip.background.y = element_blank(),
  #       strip.text.y = element_blank(),
  #       axis.line.y.right = element_line(color = "red"), 
  #       axis.ticks.y.right = element_line(color = "red"),
  #       axis.text.y.right = element_text(color = "red"), 
  #       axis.title.y.right = element_text(color = "red"))
  #   return(p_i)
  # }
  # inset_plot = get_inset(df_data) 
  # 
  # # overall plot
  # p +
  #   annotation_custom(grob=ggplotGrob(inset_plot), 
  #                     ymin = 2, ymax=34, 
  #                     xmin=1955, xmax=2015)

  ggsave(plot = p,
         filename = paste0("Headwaters/Figures/site_plots/time_series_do_t_",
                          site,
                          year,
                          ".png"),
         device = "png",
         dpi = 600,
         width = 18.4,
         height = 12,
         units = "cm")
}

# plot all data
df_do_n %>%
  mutate(plots = map(data, plot_fun))


