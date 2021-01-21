# 
# Purpose: To conduct PCA of water chemistry data
# Author: Jake Diamond
# Date: October 20, 2020
# 

# Set working directory
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
setwd("C:/Users/diamo/Dropbox/Projects/Loire_headwaters")

# Load libraries
library(htmltools)
library(plotly)
library(lubridate)
library(readxl)
library(scales)
library(tidyverse)
library(factoextra)
library(rlang)
# Analysis ----------------------------------------------------------------
# Load data
chem <- read_excel("Headwaters/Data/water_chemistry/Water_chemistry_all_v2.xlsx") %>%
  select(-sensor, -`Sample bottle no.`) %>%
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
  bind_rows(filter(chem, month(datetime) == 2)) %>%
  mutate(date = date(datetime)) %>%
  select(-datetime)

# Get the date of measurement and three days before hand
df_dates <- chem %>%
  select(site, date)

# Load the DO data
df_do <- readRDS("Headwaters/Data/DO_daily_summary") %>%
  semi_join(df_dates) %>%
  select(site, date, amp_per)

# Get data into long format
df <- chem %>%
  # left_join(chem, df_do) %>%
  filter(site != "Charpassonne spring"
  , !(month(date) %in% c(7,8))) %>%
  ungroup() %>%
  pivot_longer(-c(date, site))

# Get average for each site and put into wide format
chem_w <- df %>%
  group_by(site, name) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = name) %>%
  select(-TP) %>%
  column_to_rownames("site")

# info for west/east
side <- tibble(watershed = c("Coise", "Loise", "Toranche", "Lignon", "Mare"),
               side = c("east", "east", "east", "west", "west"))

# Read in watershed info
meta <- read_excel("Headwaters/Data/Loire_headwaters_data_share.xlsx",
                   sheet = 6)

# Groups for plotting PCA results
groups <- left_join(side, meta) %>%
  filter(site %in% df$site) %>%
  arrange(site) %>%
  pull(watershed) %>%
  as.factor()

# PCA analysis ------------------------------------------------------------
# Do PCA
chem_pca <- prcomp(chem_w, center = TRUE, scale = TRUE)

# Eigenvalues
fviz_eig(chem_pca)


fviz_pca_ind(chem_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



fviz_pca_var(chem_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(chem_pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


fviz_pca_ind(chem_pca,
             col.ind = groups, # color by groups
             # palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)



# Ranking analysis --------------------------------------------------------
# Also categorize data and get plot names
types <- tibble(name = unique(df$name),
                type = c("anions", "anions", "cations", "cations", "cations", "cations",
                         "nutrients", "nutrients", "nutrients", "nutrients", "nutrients",
                         "nutrients",  "nutrients", "nutrients", "nutrients"),
                plotname = c("Cl^{`-`}",
                             "SO[4]^{`2-`}",
                             "Na^{`+`}",
                             "K^{`+`}",
                             "Mg^{`2+`}",
                             "Ca^{`2+`}",
                             "NH[4]^{`+`}*`—`*N",
                             "NO[2]^{`-`}*`—`*N",
                             "NO[3]^{`-`}*`—`*N",
                             "PO[4]^{`3-`}*`—`*P",
                             "SiO[2]",
                             "COD",
                             "NT",
                             "PT",
                             "TP"))

# Combine all meta data to dataframe and nest it
df_n <- left_join(side, select(ws_meta, site = name_site, watershed, area,
                               site_sensor, site_chem)) %>%
  right_join(rename(df, site_chem = site)) %>%
  left_join(types) %>%
  mutate(site_code = str_c(str_sub(site_chem, 1, 3),
                           str_pad(round(area), 3, pad = "0"))) %>%
  mutate(side = fct_rev(side)) %>%
  arrange(watershed, area) %>%
  mutate(site_code = fct_inorder(site_code)) %>%
  group_by(name, plotname) %>%
  nest()

# Plotting function
plot_fun <- function(data, solute){
  # Reorder within function for facets
  reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
  }
  
  # To remove the appended info on the new x axis
  scale_x_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
  }
  # title_name = bquote(.(solute))
  p = data %>% 
    mutate(month= month(datetime, label = TRUE, abbr = FALSE)) %>%
    # mutate(site = fct_reorder(site, value, 'mean')) %>%
    ggplot(aes(x = reorder_within(site, value, month), 
               y = value)) + 
    geom_segment(aes(xend = reorder_within(site, value, month), yend = 0), 
                 colour = "grey50") +
    stat_summary(aes(color = watershed, size = area, shape = side),
                 fun = mean, geom = "point") +
    scale_x_reordered() +
    scale_shape_manual(values = c(1, 19)) +
    coord_flip() +
    facet_grid(month ~ ., scales = "free", space = "free") +
    scale_colour_brewer(palette = "Dark2") +
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "bottom") +
    labs(x = "",
         y = expression(mg~L^{-1})) +
    ggtitle(parse_expr(solute))
}

df_n <- df_n %>%
  mutate(rank_plots = map2(data, plotname, plot_fun))
pluck(df_n, 4, 1)

htmltools::browsable(pluck(df_n, 4))
