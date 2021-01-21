# -------------------------------------
# Author: 
# Purpose: 
# Date:
# -------------------------------------

# Load data
chem <- read_excel("Headwaters/Data/water_chemistry/Water_chemistry_all_v2.xlsx") %>%
  select(-sensor, sampleid = `Sample bottle no.`) %>%
  group_by(site) %>%
  mutate_if(is.character, parse_number) %>%  #convert LDL to the DL
  ungroup() %>%
  pivot_longer(-c(datetime, site, sampleid)) %>%
  left_join(read_excel("Headwaters/Data/water_chemistry/detection_limits.xlsx")) %>%
  mutate(value = if_else(value < dl, value * 0.5, value)) %>% #take half of detection limit
  pivot_wider(id_cols = c("site", "datetime", "sampleid"), names_from = name) 

# Get data into as P or as N, already done for february 2020 samples!!
chem <- chem %>%
  filter(month(datetime) != 2) %>%
  mutate(NH4 = 14.01 * NH4 / 18.04,
         NO2 = 14.01 * NO2 / 46.01,
         NO3 = 14.01 * NO3 / 62,
         PO4 = 30.97 * PO4 / 94.97) %>%
  bind_rows(filter(chem, month(datetime) == 2)) %>%
  mutate(date = date(datetime),
         month = month(datetime),
         year = year(datetime)) %>%
  select(-datetime, -TP, -date)

# Load uv doc data
doc <- read_excel("Headwaters/Data/water_chemistry/uv_spectra.xlsx") %>%
  select(-date)

# combine with chemistry data
df <- left_join(chem, doc)

# Load isotope data
iso <- read_xlsx("Headwaters/Data/water_chemistry/isotopes/isotope_results.xlsx") %>%
  mutate(year = year(date),
         isoratio = `15N`/`18O`) %>%
  select(site, year, month, isoratio, Niso = `15N`, Oiso = `18O`) %>%
  filter(isoratio < 20)

# Combine with chemistry data
df <- left_join(df, iso)

# Get all watershed scale data to be plotted in one data nested frame
df_n <- df %>%
  select(-sampleid) %>%
  pivot_longer(-c(site, year, month), names_to = "chem", values_to = "chemval") %>%
  left_join(lu_ws_metrics) %>%
  ungroup() %>%
  filter(!is.na(watershed)) %>%
  group_by(chem) %>%
  nest()

# Create a function to plot
graph_fun <- function(chemistry, data) {
  data = data %>%
    mutate(season = if_else(month %in% c(7,8),
                            "summer",
                            "winter")) %>%
    filter(season == "winter")
  p = ggplot(data = data,
         aes(x = value,
             y = chemval,
             color = watershed,
             group = watershed)) +
    theme_bw() +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    facet_wrap(~name, scales = "free") +
    geom_point() +
    stat_smooth(method = "lm") +
    ggtitle({{ chemistry }})
  
  ggsave(plot = p,
         filename = paste0("Headwaters/Figures/watershed_metric_lm_",
                           {{ chemistry }}, ".png"),
         dpi = 600,
         width = 18.4,
         height = 18.4,
         units = "cm") 
  
}


# Plot all
df_n <- df_n %>%
  mutate(p_ws = map2(chem, data, graph_fun))


# Get all watershed scale, class data to be plotted in one data nested frame
df_n_cl <- df %>%
  select(-sampleid) %>%
  pivot_longer(-c(site, year, month), names_to = "chem", values_to = "chemval") %>%
  left_join(lu_ws_metrics_cl) %>%
  ungroup() %>%
  filter(!is.na(watershed)) %>%
  group_by(chem, landuse) %>%
  nest()

# rename things
df_names <- df_n_cl %>%
  ungroup() %>%
  distinct(landuse) %>%
  mutate(lu = c("urban_dense", "urban_med", "ag_intense", "ag_weak", "forest", "grassland","water"))

# Create a function to plot
graph_fun_cl <- function(chemistry, landuse, data) {
  data = data %>%
    mutate(season = if_else(month %in% c(7,8),
                            "summer",
                            "winter"))
  p = ggplot(data = data,
             aes(x = value,
                 y = chemval,
                 color = watershed,
                 shape = season,
                 group = interaction(watershed, season))) +
    theme_bw() +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    facet_wrap(~name, scales = "free") +
    stat_summary() +
    ggtitle({{ chemistry }},
            subtitle = {{landuse}})
  
  ggsave(plot = p,
         filename = paste0("Headwaters/Figures/watershed_metrics_by_class/watershed_metric_", 
                           {{landuse}},
                           "_", {{ chemistry }}, ".png"),
         dpi = 600,
         width = 18.4,
         height = 18.4,
         units = "cm") 
  
}

# Plot all
df_n_cl <- df_n_cl %>%
  left_join(df_names) %>%
  mutate(p_ws_cl = pmap(list(chem, lu, data), graph_fun_cl))



# Now by buffers ----------------------------------------------------------

# Get all watershed scale data to be plotted in one data nested frame
df_n_buff <- df %>%
  select(-sampleid) %>%
  pivot_longer(-c(site, year, month), names_to = "chem", values_to = "chemval") %>%
  left_join(ws_meta, by = "site") %>%
  mutate(site = tolower(site)) %>%
  left_join(select(ungroup(lu_buff_metrics), -name_site)) %>%
  ungroup() %>%
  filter(!is.na(watershed)) %>%
  group_by(chem) %>%
  nest()

# Create a function to plot
graph_buff_fun <- function(chemistry, data) {
  data = data %>%
    mutate(season = if_else(month %in% c(7,8),
                            "summer",
                            "winter"))
  p = ggplot(data = data,
             aes(x = value,
                 y = chemval,
                 color = watershed,
                 shape = season)) +
    theme_bw() +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    facet_wrap(~name, scales = "free") +
    geom_point() +
    ggtitle({{ chemistry }})
  
  ggsave(plot = p,
         filename = paste0("Headwaters/Figures/500m_buffer_metrics/buffer_metric_",
                           {{ chemistry }}, ".png"),
         dpi = 600,
         width = 18.4,
         height = 18.4,
         units = "cm") 
  
}


# Plot all
df_n_buff <- df_n_buff %>%
  mutate(p_ws = map2(chem, data, graph_buff_fun))





# Get all watershed scale, class data to be plotted in one data nested frame
df_n_buff_cl <- df %>%
  select(-sampleid) %>%
  pivot_longer(-c(site, year, month), names_to = "chem", values_to = "chemval") %>%
  left_join(ws_meta, by = "site") %>%
  mutate(site = tolower(site)) %>%
  left_join(select(ungroup(lu_buff_metrics_cl), -name_site)) %>%
  ungroup() %>%
  filter(!is.na(watershed),
         !is.na(landuse)) %>%
  group_by(chem, landuse) %>%
  nest()

# rename things
df_names <- df_n_buff_cl %>%
  ungroup() %>%
  distinct(landuse) %>%
  mutate(lu = c("ag_intense", "ag_weak", "forest", "urban_med", "grassland"))

# Create a function to plot
graph_buff_fun_cl <- function(chemistry, landuse, data) {
  data = data %>%
    mutate(season = if_else(month %in% c(7,8),
                            "summer",
                            "winter"))
  p = ggplot(data = data,
             aes(x = value,
                 y = chemval,
                 color = watershed,
                 shape = season,
                 group = interaction(watershed, season))) +
    theme_bw() +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    facet_wrap(~name, scales = "free") +
    stat_summary() +
    ggtitle({{ chemistry }},
            subtitle = {{landuse}})
  
  ggsave(plot = p,
         filename = paste0("Headwaters/Figures/500m_buffer_metrics_by_class/500m_metric_", 
                           {{landuse}},
                           "_", {{ chemistry }}, ".png"),
         dpi = 600,
         width = 18.4,
         height = 18.4,
         units = "cm") 
  
}

# Plot all
df_n_buff_cl <- df_n_buff_cl %>%
  left_join(df_names) %>%
  mutate(p_buff_cl = pmap(list(chem, lu, data), graph_buff_fun_cl))
