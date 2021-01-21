# 
# Purpose: To summarize grab sample chemistry data for the Loire headwaters
# Author: Jake Diamond
# Date: October 1, 2019
# 

# Set working directory
setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")

# Load libraries
library(tidyverse)
library(tidytext)
library(arsenal)
library(readxl)
library(gtable)
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

chem_wint <- filter(chem, site != "Charpassonne spring",
  !(month(date) %in% c(7,8))) %>%
  ungroup() %>%
  select(-date, -TP) %>%
  group_by(site) %>%
  summarize_all(mean, na.rm = TRUE)

# Get data into long format
df <- filter(chem, site != "Charpassonne spring") %>%
  # !(month(datetime) %in% c(7,8))) %>%
  ungroup() %>%
  pivot_longer(-c(datetime, site))

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
df <- left_join(side, ws_meta) %>%
  right_join(df) %>%
  left_join(types) %>%
  mutate(site_code = str_c(str_sub(site, 1, 3),
                            str_pad(round(area_km2), 3, pad = "0"))) %>%
  mutate(side = fct_rev(side)) %>%
  arrange(watershed, area_km2) %>%
  mutate(site_code = fct_inorder(site_code))

# plot
p_n <- ggplot(data = filter(df, 
                     !is.na(side),
                     name != "TP",
                     type == "anions"),
       aes(x = site_code,
           y = value,
           color = watershed)) +
  stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3) +
  facet_grid(plotname~side, scales = "free", switch = "y", labeller = label_parsed) +
  scale_color_brewer(name = "BV", type = "qual", palette = "Dark2") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        # strip.text = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        legend.direction = "horizontal")
p_n

gt = ggplot_gtable(ggplot_build(p_n))
# gtable_show_layout(gt)
gt$widths[7] = 0.5*gt$widths[7]
# grid.draw(gt)
ggsave(filename = "Headwaters/Figures/chemistry_anions_summary.png",
       plot = gt,
       device = "png",
       dpi = 600,
       width = 184,
       height = 120,
       units = "mm")

# Nest the data to make plotting easier
df_n <- filter(df,!is.na(side),
               !(name %in% c("TP", "TP_filt", "TN_filt", "NO2"))) %>%
  mutate(month = month(datetime),
         year = year(datetime)) %>%
  filter(year == 2020) %>%
  # filter(month %in% c(12, 2)) %>%
  filter(side == "east") %>%
  group_by(plotname) %>%
  nest()

# Plotting function
plot_fun <- function(data, solute){
  p = data %>%
  # filter(data,
             # !(name %in% c("TP", "TP_filt", "TN_filt", "NO2", "PO4", "NH4"))
             # , month(datetime) %in% c(12, 2)
             # ) %>% 
    ungroup() %>%
    mutate(#name = as.factor(name),
           site_code = reorder_within(site_code, value, month)) %>%
    ggplot(aes(x = site_code, 
               y = value)) + 
    stat_summary(aes(color = watershed, size = area_km2),
                 fun = mean, geom = "point") +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3) +
    scale_x_reordered() +
    facet_wrap(~month, scales = "free_y", labeller = label_parsed, ncol = 1) +
    scale_color_brewer(name = "BV", type = "qual", palette = "Dark2") +
    coord_flip() +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.border = element_rect(color = "black"),
          axis.title = element_blank(),
          # axis.text.x = element_text(angle = 90, vjust = 0.5),
          # strip.text = element_blank(),
          strip.placement = "outside",
          legend.position = "bottom",
          legend.direction = "horizontal") +
    ggtitle(parse_expr(solute))
  name = unique(data$name)
  ggsave(plot = p,
         filename = paste0("Headwaters/Figures/winter_summer_baseflow_ranking_", name, ".png"),
         dpi = 600,
         width = 18.4,
         height = 20,
         units = "cm"
         )
}

df_n <- df_n %>%
  mutate(summary_plots = map2(data, plotname, plot_fun))
pluck(df_n, 4, 8)















# table format for summary stats
chem_w <- chem %>%
  select(-dl) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  select(-datetime)
  
table_one <- tableby(site ~ ., data = chem)
x = summary(table_one, title = "Water chemistry Data", text = NULL)
write2word(x, "~/table.docx")

chem_sum <- chem_w %>%
  group_by(site) %>%
  summarise(across(.fns = list(mean = mean, 
                               sd = sd,
                               min = min), 
                   na.rm = TRUE, 
                   .names = "{col}_{fn}")) %>%
  transmute_if(is.numeric, paste(.))

chem_sum <- chem %>%
  group_by(site, name) %>%
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            n = n()) %>%
  mutate(text = paste0(round(mean, 2), "%+-%", 
                          round(sd, 2), "(", 
                          n, ")")) %>%
  select


x <- read
            