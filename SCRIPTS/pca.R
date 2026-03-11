library(ggplot2)      # For advanced plotting
library(FactoMineR)   # For PCA and supplementary variables
library(factoextra)   # For PCA visualization
library(readxl)
##-- Load the weather dataset
dd <- read_excel("C:/Users/MM/Desktop/UNI2/ME/DADES/database_V6.xlsx")

##-- Inspect
View(dd)
summary(dd)

dd$CiudadInicial <- case_when(
  dd$City == "Chicago" ~ "C",
  dd$City == "Dallas" ~ "D",
  dd$City == "Houston" ~ "H",
  dd$City == "Los Angeles" ~ "L",
  dd$City == "New York City" ~ "N",
  dd$City == "Philadelphia" ~ "P",
  dd$City == "Phoenix" ~ "X",
  dd$City == "San Antonio" ~ "SA",
  dd$City == "San Diego" ~ "SD",
  dd$City == "San Jose" ~ "SJ",
  TRUE ~ ""  # para manejar cualquier otra ciudad inesperada
)

# Only numeric variables
numeric_dd <- dd[, sapply(dd, is.numeric)]

##-- Step 1: Scale the data to ensure all features have equal weight in PCA
dd_scaled <- scale(numeric_dd)

##-- Step 2: Perform PCA using prcomp, with centering and scaling
pca_result <- prcomp(dd_scaled)

##-- Compare
# VAPs
pca_result$sdev
sqrt(eigen(cov(dd_scaled))$values)
# VEPs
pca_result$rotation[,1]
eigen(cov(dd_scaled))$vec[,1]

##-- Step 3: Scree Plot - visualizing the variance explained by each principal component
plot(pca_result)             # first option
plot(pca_result, type='l')   # second option
# third option
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 50)) +
  ggtitle("Scree Plot: Variance Explained by Each Component") +
  xlab("Principal Component") +
  ylab("Percentage of Variance Explained")

# How many components?
summary(pca_result)
dd$City <- as.factor(dd$City)
# Step 4: Graphical representation
# Individuals
fviz_pca_ind(pca_result, 
                       # Avoid overlapping text labels
             geom = c("point"),
             col.ind = dd$City,
             palette = c("Chicago" = "blue", "Dallas" = "green", "Houston" = "purple", 
                         "Los Angeles" = "orange", "New York City" = "red", 
                         "Philadelphia" = "pink", "Phoenix" = "brown", 
                         "San Antonio" = "cyan", "San Diego" = "yellow", 
                         "San Jose" = "magenta"),     # Color of individuals
             repel = TRUE,
             label = "none",
             title = "PCA de Individus per Ciutat") +
  theme_minimal()

# Variables
fviz_pca_var(pca_result, 
             repel = TRUE,          # Avoid overlapping text labels
             col.var = "blue",       # Color of individuals
             title = "Individuals") +
  theme_minimal()


# Biplot - showing both variables and observations in PCA space
fviz_pca_biplot(pca_result, 
                repel = TRUE,          # Avoid overlapping text labels
                geom = c("point"),
                col.var = "blue",      # Color of variable arrows
                palette = c("Chicago" = "blue", "Dallas" = "green", "Houston" = "purple", 
                            "Los Angeles" = "orange", "New York City" = "red", 
                            "Philadelphia" = "pink", "Phoenix" = "brown", 
                            "San Antonio" = "cyan", "San Diego" = "yellow", 
                            "San Jose" = "magenta"),     # Color of individuals
                col.ind = dd$City,       # Color of individuals
                alpha.ind = 0.7,
                title = "PCA Biplot de Temps") +
  theme_minimal()

# Biplot - Axes 1 and 3
fviz_pca_biplot(pca_result,
                axes = c(1, 3),
                repel = TRUE,          # Avoid overlapping text labels
                geom = c("point"),
                col.var = "blue",      # Color of variable arrows
                col.ind = "red",       # Color of individuals
                alpha.ind = 0.4,
                title = "PCA Biplot of Wine Data") +
  theme_minimal()

# Additional Step: Visualize the individual contributions of variables to each component
fviz_pca_var(pca_result, col.var = "contrib", 
             gradient.cols = c("blue", "yellow", "red"),
             title = "Contribution of Variables to PCA Components") +
  theme_minimal()

# New components
pca_result$x
pca_result$rotation
