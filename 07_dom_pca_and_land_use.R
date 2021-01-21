# -------------------------------------
# Author: 
# Purpose: 
# Date:
# -------------------------------------
library(factoextra)
df_uv <- read_xlsx("Headwaters/Data/water_chemistry/uv_spectra.xlsx")
df_match <- read_xlsx("Headwaters/Data/water_chemistry/Water_chemistry_all_v2.xlsx") %>%
  mutate(year = year(datetime),
         month = month(datetime)) %>%
  filter(year == 2020) %>%
  select(site, year, month, sampleid = `Sample bottle no.`)
df_uv <- left_join(df_uv, df_match)
df_uv <- left_join(df_uv, ws_meta)

# Get into correct format, first look at 
df_uv_pca <- df_uv %>%
  filter(month == 7,
         site != "Charpassonne spring") %>%
  select(5:11) %>%
  arrange(site) %>%
  column_to_rownames("site")

# info for west/east
side <- tibble(watershed = c("Coise", "Loise", "Toranche", "Lignon", "Mare"),
               side = c("east", "east", "east", "west", "west"))

# Groups for plotting PCA results
groups <- left_join(side, ws_meta) %>%
  filter(site %in% rownames(df_uv_pca)) %>%
  arrange(site) %>%
  pull(watershed) %>%
  as.factor()

# PCA analysis ------------------------------------------------------------
# Do PCA
chem_pca <- prcomp(df_uv_pca, center = TRUE, scale = TRUE)

# Eigenvalues
fviz_eig(chem_pca, addlabels = TRUE)
pcainfo <- get_pca_var(chem_pca)
pcainfo$cos2
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
x = as.data.frame(get_pca_ind(chem_pca)$coord)
x
y <- df_uv %>%
  filter(month == 7,
         site != "Charpassonne spring") %>%
  select(-c(5:10)) %>%
  arrange(site) %>%
  bind_cols(dim1 = x$Dim.1)

ggplot(data = y,
       aes(x = log(area_km2),
           y = dim1,
           color = watershed)) +
  geom_point() +
  theme_bw()


z = readRDS("Headwaters/Data/watershed_landuse_metrics_by_class")
z_pca <- z %>%
  ungroup() %>%
  filter(landuse == "agricole intensif") %>%
  select(site, name, value) %>%
  pivot_wider() %>%
  arrange(site) %>%
  column_to_rownames("site")

# Do PCA
lu_pca <- prcomp(z_pca, center = TRUE, scale = TRUE)

# Eigenvalues
fviz_eig(lu_pca)


fviz_pca_ind(lu_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



fviz_pca_var(lu_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(lu_pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


lu_x = as.data.frame(get_pca_ind(lu_pca)$coord) %>%
  rownames_to_column() %>%
  select(site = rowname, lu_dim1 = Dim.1, lu_dim2 = Dim.2)
lu_x
w <- left_join(y, lu_x)

ggplot(data = w,
       aes(x = lu_dim1,
           y = dim1,
           color = watershed)) +
  geom_point() +
  theme_bw()


areas <- z %>%
  filter(metric == "frac_mn") %>%
  left_join(y)

areas %>%
  # filter(landuse == "forest", watershed == "Loise",
  #        value > 200) %>%
ggplot(aes(x = value,
           y = dim1,
           color = watershed)) +
  geom_point() +
  theme_bw() +
  # scale_x_continuous(limits = c(4.2,4.6)) +
  facet_wrap(~landuse, scales = "free")
  


# At the buffer scale -----------------------------------------------------

lu_buff = readRDS("Headwaters/Data/buffer_land_use") %>%
  ungroup() %>% unnest() %>%
  left_join(distinct(select(ungroup(areas), reGROUP, landuse))) %>%
  left_join(distinct(mutate(ws_meta, site = tolower(site))))

lu_buff_df <- lu_buff %>%
  ungroup() %>%
  filter(landuse %in% c( "agricole intensif")) %>%
  select(site, frac = area_lu_frac, buff_dists) %>%
  group_by(site, buff_dists) %>%
  summarize(frac = sum(frac))#%>%
  # pivot_wider(names_from = "buff_dists", values_from = frac) %>%
  # arrange(site) # %>%
  # column_to_rownames("site")


buff_areas <- lu_buff_df %>%
  left_join(mutate(y, site = tolower(site)))

buff_areas %>%
  mutate(buff_dist = str_c(buff_dists, " m buffer")) %>%
  # mutate(buff_dist = fct_inseq(buff_dist)) %>%
  # mutate(buff_dist = str_sort(buff_dist, numeric = TRUE)) %>%
  # pivot_longer()
  # filter(landuse == "forest", watershed == "Loise",
  #        value > 200) %>%
  ggplot(aes(x = frac,
             y = dim1)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~buff_dist) +
  labs(x = "fraction of agricultural cover within the buffer",
       y = "PCA1 (69%) of summer organic matter UV spectra") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate(geom = "text", x = 0.57, y = 0.8, label = "more aromatic") +
  annotate(geom = "text", x = 0.57, y = -0.5, label = "less aromatic")
ggsave(filename = "Headwaters/Figures/DOM/intensive_ag_buffer_PCA1_summer.png",
       dpi = 600,
       width = 18.4,
       height = 9.2,
       units = "cm")
