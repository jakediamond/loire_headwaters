# 
# Purpose: To summarize grab sample chemistry data for the Loire headwaters
# Author: Jake Diamond
# Date: October 1, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
setwd("C:/Users/diamo/Dropbox/Projects/Loire_headwaters")

# Load libraries
library(tidytext)
library(rlang)
library(lubridate)
library(arsenal)
library(readxl)
library(gtable)
library(tidyverse)
# Analysis ----------------------------------------------------------------
# Add chemistry data to points
chem <- read_excel("Headwaters/Data/Loire_headwaters_data_share.xlsx",
                   sheet = "water_chemistry") %>%
  select(-`sample bottle`) %>%
  group_by(site) %>%
  mutate_if(is.character, parse_number) %>%  #convert LDL to the DL
  ungroup() %>%
  pivot_longer(-c(datetime, site)) %>%
  left_join(read_excel("Headwaters/Data/water_chemistry/detection_limits.xlsx")) %>%
  mutate(value = if_else(value < dl, value * 0.5, value)) %>% #take half of detection limit
  pivot_wider(id_cols = c("site", "datetime"), names_from = name)

# Get data into as P or as N, already done for february 2020 samples!!
chem <- chem %>%
  filter(month(datetime) != 2) %>%
  mutate(NH4 = 14.01 * NH4 / 18.04,
         NO2 = 14.01 * NO2 / 46.01,
         NO3 = 14.01 * NO3 / 62,
         PO4 = 30.97 * PO4 / 94.97) %>%
  bind_rows(filter(chem, month(datetime) == 2))

# Filter for sites with at least 4 samples
chem_full <- filter(chem, site != "Charpassonne spring") %>%
  ungroup() %>%
  # select(-date, -TP) %>%
  group_by(site) %>%
  filter(n() > 2)

# Get data into long format
df <- chem_full %>%
  mutate(date = date(datetime)) %>%
  select(-datetime) %>%
  ungroup() %>%
  pivot_longer(-c(date, site))

# add a column for west/east
side <- tibble(watershed = c("Coise", "Loise", "Toranche", "Lignon", "Mare"),
               side = c("east", "east", "east", "west", "west"))

# Read in watershed info
ws_meta <- read_excel("Headwaters/Data/Loire_headwaters_data_share.xlsx",
                      sheet = "watershed_meta")

# Also categorize data and get plot names
types <- tibble(name = unique(df$name),
                type = c("anions", "anions", "cations", "cations", "cations", "cations",
                         "nutrients", "nutrients", "nutrients", "nutrients", "nutrients",
                         "nutrients",  "nutrients", "nutrients", "nutrients"),
                plotname = c("Cl^{`-`}~(mg~L^{-1})",
                             "SO[4]^{`2-`}~(mg~L^{-1})",
                             "Na^{`+`}~(mg~L^{-1})",
                             "K^{`+`}~(mg~L^{-1})",
                             "Mg^{`2+`}~(mg~L^{-1})",
                             "Ca^{`2+`}~(mg~L^{-1})",
                             "NH[4]^{`+`}*`—`*N~(mg~L^{-1})",
                             "NO[2]^{`-`}*`—`*N~(mg~L^{-1})",
                             "NO[3]^{`-`}*`—`*N~(mg~L^{-1})",
                             "PO[4]^{`3-`}*`—`*P~(mg~L^{-1})",
                             "SiO[2]~(mg~L^{-1})",
                             "COD~(mg~L^{-1})",
                             "NT~(mg~L^{-1})",
                             "PT~(mg~L^{-1})",
                             "TP"))

# Combine all
df_p <- left_join(side, ws_meta) %>%
  right_join(df) %>%
  left_join(types) %>%
  mutate(site_code = str_c(str_sub(site, 1, 3),
                            str_pad(round(area_km2), 3, pad = "0"))) %>%
  mutate(side = fct_rev(side)) %>%
  arrange(watershed, area_km2) %>%
  mutate(site_code = fct_inorder(site_code))

# Rank each site by date and chemistry
df_r <- df_p %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year, month, name, side) %>%
  mutate(rank = row_number(value))

# Quick example plot
df_r %>%
  ungroup() %>%
  mutate(season = if_else(month %in% c(7,8), "summer", "winter")) %>%
  group_by(site_code, name, season) %>%
  mutate(time = row_number(date)) %>%
  filter(name == "NO3") %>%
  ggplot(aes(x = time,
             y = rank,
             color = site_code)) +
  scale_color_viridis_d() +
  geom_point(aes(size = value,
                 shape = watershed)) +
  geom_line() +
  scale_size_area(name = "mg/L") +
  scale_y_continuous(breaks = seq(0, 11, 1)) +
  scale_x_continuos(breaks = seq(1, 3, 1)) + 
  labs(title = "Time series of NO3 ranking",
       y = "rank",
       x = "") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  facet_grid(season~side)
  # facet_wrap(~side)

# Nest the data to make plotting easier
df_n <- filter(df_r,!is.na(side),
               !(name %in% c("TP", "TP_filt", "TN_filt", "NO2"))) %>%
  ungroup() %>%
  mutate(season = if_else(month %in% c(7,8), "summer", "winter")) %>%
  group_by(site_code, name, season) %>%
  mutate(time = row_number(date)) %>%
  ungroup() %>%
  group_by(name) %>%
  nest()

# Plotting function
plot_fun <- function(data, solute){
  p = data %>%
    ggplot(aes(x = time,
               y = rank,
               color = site_code)) +
    scale_color_viridis_d() +
    geom_point(aes(size = value,
                   shape = watershed)) +
    geom_line() +
    scale_size_area(name = "mg/L") +
    scale_shape_manual(values = c(15,16,17,18)) +
    scale_y_continuous(breaks = seq(0, 11, 1)) +
    scale_x_continuous(breaks = seq(1, 3, 1)) + 
    labs(title = paste0("Time series of ", solute, " ranking"),
         y = "rank",
         x = "") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    facet_grid(season~side)

  ggsave(plot = p,
         filename = paste0("Headwaters/Figures/rankings/ranking_season_", solute, ".png"),
         dpi = 600,
         width = 26,
         height = 20,
         units = "cm"
         )
}

df_n <- df_n %>%
  mutate(summary_plots = map2(data, name, plot_fun))

# Rank the sites on the X-axis as a function of the concentration of a given 
# element for July 20, and add their values for the 2 other dates on top of them
# without lines to see the range of variation.
df_r_new <- df_r %>%
  ungroup() %>%
  filter(month == 7, year == 2020) %>%
  mutate(val_j20 = value) %>%
  select(site_code, name, val_j20) %>%
  left_join(df_r) %>%
  mutate(season = if_else(month %in% c(7,8), "summer", "winter")) %>%
  filter(season == "summer") %>%
  mutate(date_simple = ymd(paste(year, month, "01"))) %>%
  group_by(name) %>%
  mutate(time = dense_rank(date_simple)) %>%
  ungroup()

plot_fun2 <- function(data, solute){
  x = data %>%
    mutate(site_code = reorder_within(site_code, val_j20, side)) %>%
    ggplot(aes(x = site_code,
               y = value,
               color = as.factor(time))) +
    scale_color_brewer(type = "qual",
                       palette = "Dark2",
                       labels = c("July 2019", "August 2019", "July 2020"),
                       name = "") +
    geom_jitter(width = 0.1, height = 0) +
    facet_wrap(~side, scales = "free_x") +
    theme_bw() +
    scale_x_reordered() +
    # scale_y_continuous(breaks = seq(0, 3, 0.5)) +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5),
          panel.grid.minor = element_blank()) +
    labs(title = paste0("Ranking of ", solute, " across sites"),
         y = paste0(solute, " (mg/L)"),
         x = "")

  gt = ggplot_gtable(ggplot_build(x))
  gt$widths[5] = 0.5*gt$widths[5]
  
  ggsave(paste0("Headwaters/Figures/rankings/ranking_value_summer_", solute, ".png"),
         plot = gt,
         device = "png",
         dpi = 600,
         width = 184,
         height = 120,
         units = "mm")
}

df_n2 <- df_r_new %>%
  filter(!is.na(side),
         !(name %in% c("TP", "TP_filt", "TN_filt", "NO2"))) %>%
  ungroup() %>%
  group_by(name) %>%
  nest() %>%
  mutate(summary_plots = map2(data, name, plot_fun2))


# nitrate versus sulfate in summer
(df_r %>%
    ungroup() %>%
    mutate(season = if_else(month %in% c(7,8), "summer", "winter")) %>%
    filter(season == "summer",
           name %in% c("NO3", "SO4")) %>%
    select(site_code, side, watershed, subwatershed, area_km2, date, value, name) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    ggplot(aes(x = SO4,
               y = NO3,
               color = watershed)) +
    stat_summary() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_bw() +
    labs(title = "Summertime mean NO3-N vs. SO4 by watershed",
         y = "NO3-N (mg/L)",
         x = "SO4 (mg/L)")) %>%
  ggsave(filename = "Headwaters/Figures/summer_nitrate_vs_sulfate.png",
         device = "png",
         dpi = 600,
         width = 184,
         height = 120,
         units = "mm")

# silica versus sulfate in summer
df_r %>%
    ungroup() %>%
    mutate(season = if_else(month %in% 7, "summer", "winter")) %>%
    filter(season == "summer",
           name %in% c("SO4", "SiO2")) %>%
    select(site_code, side, watershed, subwatershed, area_km2, date, value, name) %>%
    pivot_wider(names_from = name, values_from = value) %>%
  filter(watershed == "Loise") %>%
    ggplot(aes(x = log(area_km2),
               y = SiO2,
               color = watershed)) +
    geom_point() +
    theme_bw() +
    # geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Summertime mean SiO2 vs. SO4 by watershed",
         y = "SiO2 (mg/L)",
         x = "SO4 (mg/L)")
  ggsave(filename = "Headwaters/Figures/summer_silica_vs_sulfate.png",
         device = "png",
         dpi = 600,
         width = 184,
         height = 120,
         units = "mm")

# 15N-NO3 versus sulfate in summer
# Load isotope data
iso <- read_xlsx("Headwaters/Data/water_chemistry/isotopes/isotope_results.xlsx") %>%
  mutate(year = year(date),
         isoratio = `15N`/`18O`) %>%
  select(site, year, month, isoratio, Niso = `15N`, Oiso = `18O`) %>%
  filter(isoratio < 20)

(df_r %>%
    ungroup() %>%
    mutate(season = if_else(month %in% c(7,8), "summer", "winter")) %>%
    filter(season == "summer",
           name %in% c("SO4")) %>%
    select(site, site_code, side, watershed, subwatershed, area_km2, date, value, name) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    left_join(iso) %>%
    ggplot(aes(x = SO4,
               y = Niso,
               color = watershed)) +
    geom_point() +
    theme_bw() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Summertime mean 15N-NO3 vs. SO4 by watershed",
         y = "15N-NO3 (per mil)",
         x = "SO4 (mg/L)")) %>%
  ggsave(filename = "Headwaters/Figures/summer_nitrateiso_vs_sulfate.png",
         device = "png",
         dpi = 600,
         width = 184,
         height = 120,
         units = "mm")


# Ratio of Cl/NO3 in  summer along the different stream networks
dat <- filter(chem, site != "Charpassonne spring") %>%
  ungroup() %>%
  mutate(date = date(datetime)) %>%
  select(-datetime) %>%
  ungroup() %>%
  pivot_longer(-c(date, site))

dat <- left_join(side, ws_meta) %>%
  right_join(dat) %>%
  left_join(types) %>%
  mutate(site_code = str_c(str_sub(site, 1, 3),
                           str_pad(round(area_km2), 3, pad = "0"))) %>%
  mutate(side = fct_rev(side)) %>%
  arrange(watershed, area_km2) %>%
  mutate(site_code = fct_inorder(site_code))


p_rat <- dat %>%
  mutate(year = year(date),
         month = month(date)) %>%
  ungroup() %>%
  mutate(season = if_else(month %in% c(7,8), "summer", "winter")) %>%
  filter(season == "summer",
         name %in% c("NO3", "Cl")) %>%
  select(site_code, side, watershed, subwatershed, area_km2, year, month, value, name) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  filter(!is.na(subwatershed)) %>%
  mutate(rat = Cl / NO3) %>%
  mutate(date_simple = ymd(paste(year, month, "01"))) %>%
  mutate(time = dense_rank(date_simple)) %>%
  ggplot(aes(x = site_code,
             y = rat,
             color = subwatershed,
             group = interaction(time, subwatershed))) +
  geom_point(aes(shape = as.factor(time))) +
  geom_line(aes(linetype = as.factor(time))) +
  theme_bw() +
  scale_linetype_discrete(labels = c("July 2019", "August 2019", "July 2020"),
                          name = "") +
  scale_shape_discrete(labels = c("July 2019", "August 2019", "July 2020"),
                          name = "") +
  facet_wrap(~watershed, scales = "free") +
  ylab("Cl:NO3") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5),
        legend.position = c(0.85, 0.18),
        legend.key.size = unit(0.8, 'lines'),
        legend.background = element_rect(fill=alpha('white', 0)),
        legend.title = element_blank()) +
  guides(color=guide_legend(ncol=2),
         shape = guide_legend(ncol=2),
         linetype = guide_legend(ncol=2))
p_rat
ggsave(filename = "Headwaters/Figures/clno3.png",
       plot = p_rat,
       device = "png",
       dpi = 600,
       width = 240,
       height = 120,
       units = "mm")

