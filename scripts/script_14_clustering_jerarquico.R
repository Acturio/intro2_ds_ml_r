#devtools::install_github("jokergoo/ComplexHeatmap")

pacman::p_load(
  factoextra,
  cluster,
  readr,
  dplyr,
  gplots,
  sf,
  pheatmap,
  patchwork,
  hopkins,
  ComplexHeatmap
)


data("USArrests")
df <- scale(USArrests)

head(df)

res_dist <- dist(df, method = "euclidian")
as.matrix(res_dist)[1:6, 1:6]

res_hc <- hclust(d = res_dist, method = "complete")
res_hc

fviz_dend(res_hc, cex = 0.5)

res_coph <- cophenetic(res_hc)
as.matrix(res_coph)[1:6, 1:6]

cor(res_coph, res_dist)

groups_1 <- cutree(res_hc, k = 4)
head(groups_1)

table(groups_1)

fviz_dend(
 res_hc,
 k = 4,
 cex = 0.5,
 k_colors = c("red", "blue", "green", "purple"),
 color_labels_by_k = TRUE,
 rect = TRUE
)

fviz_dend(
 res_hc,
 k = 4,
 cex = 0.5,
 k_colors = "jco",
 type = "circular",
 show_labels = TRUE
)

fviz_dend(
 res_hc,
 k = 4,
 cex = 0.5,
 k_colors = "jco",
 type = "phylogenic",
 phylo_layout = "layout.gem",
 repel = T,
 show_labels = TRUE
)

fviz_cluster(
 list(data = df, cluster = groups_1),
 palette = c("red", "blue", "green", "purple"),
 ellipse.type = "convex",
 repel = T,
 show.clust.cent = F
)


res_agnes <- agnes(
 x = USArrests,
 stand = T,
 metric = "euclidian",
 method = "ward"
)

res_diana <- diana(
 x = USArrests,
 stand = T,
 metric = "euclidian"
)

fviz_dend(
 res_diana, 
 cex = 0.6, 
 k = 4,
 color_labels_by_k = TRUE,
 rect = TRUE
 )

#### Dendogramas ####

indice_marg <- st_read('data/IMEF_2010.dbf', quiet = TRUE)
indice_marg %>% glimpse()

df <- indice_marg %>% 
  select(ANALF:PO2SM) %>% 
  scale(center = TRUE, scale = TRUE)

row.names(df) <- indice_marg$NOM_ENT

heatmap.2(
  df,
  scale = "none",
  cexCol = 0.7,
  cexRow = 0.5,
  main = "Heatmap - Marginación",
  col = bluered(32),
  trace = "none",
  density.info = "none"
 )


pheatmap(
  df,
  cutree_rows = 5,
  cutree_cols = 2,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "ward.D",
  main = "Heatmap - Ward´s Clustering"
  )



Heatmap(
  df,
  split = indice_marg %>% 
     mutate(GM = case_when(
        GM == "Muy bajo" ~ "MB",
        GM == "Bajo" ~ "B",
        GM == "Medio" ~ "M",
        GM == "Alto" ~ "A",
        GM == "Muy alto" ~ "MA"
     )) %>% pull(GM),
  name = "Scale",
  clustering_distance_rows = "euclidean",
  clustering_method_rows = "ward.D",
  column_title = "Variables",
  row_title_gp = gpar(fontsize = 12),
  column_title_gp = gpar(fontsize = 12),
  row_names_gp = gpar(fontsize = 8)
)


#### Tendencia de factibilidad ####

head(iris, 5)

df <- iris %>% select_if(is.numeric)
random_df <- df %>% 
  apply(2, function(x){runif(length(x), min(x), max(x))}) %>% 
  as_tibble()

df_scaled <- scale(df)
random_df_scaled <- scale(random_df)



iris_plot <- fviz_pca_ind(
  prcomp(df_scaled), title = "PCA - Iris data",
  geom = "point", ggtheme = theme_classic(),
  legend = "bottom"
)

random_plot <- fviz_pca_ind(
  prcomp(random_df_scaled), title = "PCA - Random data",
  geom = "point", ggtheme = theme_classic(),
  legend = "bottom"
)

iris_plot + random_plot



set.seed(12853)
km_res2 <- kmeans(random_df_scaled, 3)
cluster_plot <- fviz_cluster(
  list(data = random_df_scaled, cluster = km_res2$cluster),
  ellipse.type = "convex", geom = "point", stand = F,
  palette = "jco", ggtheme = theme_classic()
)

den_plot <- fviz_dend(
  hclust(dist(random_df_scaled), method = "ward.D"), k = 3, k_colors = "jco",
  as.gplot = T, show_labels = F
)

cluster_plot + den_plot



set.seed(19735)
1 - hopkins::hopkins(df_scaled, nrow(df_scaled)-1, 4)


dis_irirs_plot <- fviz_dist(
  dist(df_scaled),
  show_labels = FALSE) +
  labs(title = "Iris data")


dis_random_plot <- fviz_dist(
  dist(random_df_scaled),
  show_labels = FALSE) +
  labs(title = "Random data")

dis_irirs_plot + dis_random_plot























