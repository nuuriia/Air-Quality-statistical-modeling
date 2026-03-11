################################################################################
# LIBRERÍAS NECESARIAS
################################################################################
library(scatterplot3d)  # plot 3D
library(flexclust)      # clustering
library(NbClust)        # determinar número óptimo de clusters
library(factoextra)     # visualización de clusters
library(kernlab)        # kernel k-means
library(clValid)        # validación de clusters
library(cluster)        # pam (K-medoids)
library(fossil)         # cálculo de Rand Index
library(clustMixType)   # K-prototype

################################################################################
# CARGA Y PREPROCESAMIENTO DE DATOS
################################################################################
# Cargar datos desde un archivo CSV
air_quality <- read.csv("C:/Users/marcd/OneDrive/Desktop/ME/database_V6.csv")

# Ver estructura inicial de los datos
str(air_quality)
View(air_quality)

# Convertir columnas numéricas con comas a formato numérico
cols_to_convert <- sapply(air_quality, function(x) all(!is.na(as.numeric(gsub(",", ".", x)))))
cols_to_convert <- names(cols_to_convert[cols_to_convert])

for (col in cols_to_convert) {
  air_quality[[col]] <- as.numeric(gsub(",", ".", air_quality[[col]]))
}

# Confirmar conversión correcta
str(air_quality)

# Convertir variables categóricas a factores
for (col in names(air_quality)) {
  if (is.character(air_quality[[col]])) {
    air_quality[[col]] <- as.factor(air_quality[[col]])
  }
}

# Seleccionar solo variables numéricas
numerical_data <- air_quality[sapply(air_quality, is.numeric)]

################################################################################
# DETERMINAR NÚMERO ÓPTIMO DE CLUSTERS
################################################################################
set.seed(123)  # Para reproducibilidad
nbclust_results <- NbClust(
  data = scale(numerical_data),  # Escalar datos numéricos
  distance = "euclidean",        # Métrica de distancia
  min.nc = 2,                    # Número mínimo de clusters
  max.nc = 10,                   # Número máximo de clusters
  method = "kmeans",             # Método de clusterización
  index = "all"                  # Evaluar todos los índices disponibles
)

# Mostrar resultados de NbClust
nbclust_results$Best.nc  # Número óptimo de clusters según los índices
barplot(
  table(nbclust_results$Best.nc[1, ]), 
  xlab = "Número de clusters", 
  ylab = "Frecuencia", 
  main = "Número óptimo de clusters según múltiples índices"
)

################################################################################
# MÉTODO 1: K-MEANS CON VARIABLES NUMÉRICAS ESCALADAS
################################################################################
set.seed(123)
kmeans_result <- kmeans(scale(numerical_data), centers = 5, nstart = 10)

################################################################################
# MÉTODO 2: K-MEDOIDS CON DISTANCIA DE GOWER
################################################################################
dist_matrix <- daisy(air_quality, metric = "gower")
kmedoids_result <- pam(dist_matrix, k = 5)

################################################################################
# MÉTODO 3: K-PROTOTYPE PARA VARIABLES NUMÉRICAS Y CATEGÓRICAS
################################################################################
set.seed(123)
kprototype_result <- kproto(air_quality, k = 5, lambda = NULL)  # `lambda` ajusta pesos

# Extraer clusters generados por K-prototype
kproto_clusters <- kprototype_result$cluster

################################################################################
# COMPARACIÓN DE MÉTODOS
################################################################################
# Comparación entre K-means y K-medoids
table_kmeans_kmedoids <- table(kmeans_result$cluster, kmedoids_result$clustering)
rand_index_kmeans_kmedoids <- rand.index(kmeans_result$cluster, kmedoids_result$clustering)

# Comparación entre K-prototype y K-medoids
table_kproto_kmedoids <- table(kproto_clusters, kmedoids_result$clustering)
rand_index_kproto_kmedoids <- rand.index(kproto_clusters, kmedoids_result$clustering)

# Comparación entre K-prototype y K-means
table_kproto_kmeans <- table(kproto_clusters, kmeans_result$cluster)
rand_index_kproto_kmeans <- rand.index(kproto_clusters, kmeans_result$cluster)

# Mostrar Rand Index
cat("Rand Index entre K-means y K-medoids:", rand_index_kmeans_kmedoids, "\n")
cat("Rand Index entre K-prototype y K-medoids:", rand_index_kproto_kmedoids, "\n")
cat("Rand Index entre K-prototype y K-means:", rand_index_kproto_kmeans, "\n")

################################################################################
# EVALUACIÓN: SILHOUETTE SCORE
################################################################################
# Silhouette para K-means
sil_kmeans <- silhouette(kmeans_result$cluster, dist_matrix)
cat("Silhouette Score promedio para K-means:", mean(sil_kmeans[, 3]), "\n")

# Silhouette para K-medoids
sil_kmedoids <- silhouette(kmedoids_result$clustering, dist_matrix)
cat("Silhouette Score promedio para K-medoids:", mean(sil_kmedoids[, 3]), "\n")

# Silhouette para K-prototype
sil_kproto <- silhouette(kproto_clusters, dist_matrix)
cat("Silhouette Score promedio para K-prototype:", mean(sil_kproto[, 3]), "\n")

################################################################################
# VISUALIZACIÓN DE CLUSTERS
################################################################################
# Visualizar clusters de K-means
fviz_cluster(kmeans_result, data = scale(numerical_data), main = "Clusters K-means")

# Visualizar clusters de K-medoids
fviz_cluster(list(data = dist_matrix, cluster = kmedoids_result$clustering),
             geom = "point", ellipse.type = "convex",
             ggtheme = theme_minimal(),
             main = "Clusters K-medoids")

# Visualizar clusters de K-prototype
fviz_cluster(list(data = air_quality, cluster = kproto_clusters),
             geom = "point", ellipse.type = "convex",
             ggtheme = theme_minimal(),
             main = "Clusters K-prototype")

################################################################################
# ANÁLISIS POR REGIÓN
################################################################################
# Mapear ciudades a regiones
region_mapping <- c(
  "Chicago" = "Norte",
  "Dallas" = "Sur",
  "Houston" = "Sur",
  "Los Angeles" = "Oeste",
  "New York" = "Norte",
  "Philadelphia" = "Norte",
  "Phoenix" = "Oeste",
  "San Antonio" = "Sur",
  "San Diego" = "Oeste",
  "San Jose" = "Oeste"
)
air_quality$Region <- region_mapping[air_quality$City]
air_quality$Region[is.na(air_quality$Region)] <- "Otra Región"

# Crear tabla de contingencia entre clusters de K-means (numérico) y regiones
table_clusters_regions_kmeans <- table(Cluster = kmeans_result$cluster, Region = air_quality$Region)
print("Tabla de contingencia entre clusters de K-means y regiones:")
print(table_clusters_regions_kmeans)

# Proporciones de clusters por región para K-means
prop_table_kmeans <- prop.table(table_clusters_regions_kmeans, margin = 2)
print("Proporción de clusters de K-means en cada región:")
print(round(prop_table_kmeans, 2))






