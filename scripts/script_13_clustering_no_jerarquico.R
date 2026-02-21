pacman::p_load(
  dplyr,
  patchwork,
  gridExtra,
  factoextra,
  cluster,
  dbscan,
  fpc
)

USArrests_scaled <- scale(USArrests%>% select(-UrbanPop))

#### Distancias homogéneas ####

dist.cor <- get_dist(USArrests_scaled, method = "pearson")

round(as.matrix(dist.cor)[1:7, 1:7], 1)

#### Distancias mixtas ####

data(flower)
glimpse(flower)

dd <- daisy(flower)
round(as.matrix(dd)[1:10, 1:10], 2)

#### Visualización de distancias ####

fviz_dist(dist.cor)


#### Clustering K-means ####

df <- USArrests_scaled 
head(df, n = 5)

set.seed(1235)
k6 <- kmeans(df, centers = 6, nstart = 25)
fviz_cluster(k6, data = df, repel = TRUE)

## Comparación ##

k2 <- kmeans(df, centers = 2, nstart = 25)
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

p2 <- fviz_cluster(k2, geom = "point",  data = df) + ggtitle("K = 2")
p3 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("K = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("K = 4")
p5 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("K = 5")

grid.arrange(p2, p3, p4, p5, nrow = 2)


set.seed(123)
wss_plot <- fviz_nbclust(df, kmeans, method = "wss")
wss_plot

set.seed(123)
final <- kmeans(df, 2, nstart = 25)


kmeans_plot <- fviz_cluster(
  final,
  data = df,
  ellipse.type = "t",
  repel = TRUE) +
  ggtitle("K-Means Plot") +
  theme_minimal() +
  theme(legend.position = "bottom")

kmeans_plot

#### Clusterin PAM ####

# Elbow method
Elbow <- fviz_nbclust(df, pam, method = "wss") +
geom_vline(xintercept = 4, linetype = 2)+
labs(subtitle = "Elbow method")

Elbow

k_mediods <- pam(df, 2)

print(k_mediods)

pam_plot <- fviz_cluster(
  k_mediods,
  palette = c("#00AFBB", "#FC4E07"),
  ellipse.type = "t",
  repel = TRUE,
  ggtheme = theme_minimal()) +
  ggtitle('K-Medoids Plot') +
  theme(legend.position = "bottom")

pam_plot


#### Clustering DBSCAN ####

data("multishapes")

df <- multishapes[, 1:2]

set.seed(123)
kmeans <- kmeans(df, 5, nstart = 25)

fviz_cluster(
  kmeans,
  df,
  geom = "point",
  ellipse= FALSE,
  show.clust.cent = FALSE,
  palette = "jco",
  ggtheme = theme_minimal()
  )

kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)

set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)

fviz_cluster(
  db,
  data = df,
  stand = FALSE,
  ellipse = FALSE,
  show.clust.cent = FALSE,
  geom = "point",
  palette = "jco",
  ggtheme = theme_minimal()
  )

print(db)

# Ejemplo USArrests

df <- scale(USArrests)

kNNdistplot(df, k = 3)
abline(h = 1.175, lty = 1.5)

set.seed(114234)
db <- fpc::dbscan(df, eps = 1.175, MinPts = 2)

print(db)


dbscan_plot <- fviz_cluster(
  db,
  data = df,
  stand = FALSE,
  axes = c(1,2),
  repel = TRUE,
  show.clust.cent = FALSE,
  geom = "point",
  palette = "jco",
  ellipse.type = "t",
  ggtheme = theme_minimal()) +
  ggtitle('DBSCAN Plot') +
  theme(legend.position = "bottom")

dbscan_plot


kmeans_plot + pam_plot + dbscan_plot

