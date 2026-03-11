################################################################################
#
# ME - GIA. Profiling
#
#--------------------------------------
# Concepts
#--------------------------------------
#
# Profiling
################################################################################

rm(list=ls()) # remove objects in memory

################################################################################
# Load packages
################################################################################
library(DataExplorer)
library(dendextend)    
library(NbClust)      # number of clusters
library(fpc)          # cluster.stats
library(cluster)
library(ggpubr)       # ggarrange
library(FactoMineR)   # catdes
library(modeest)
library(psych)        # describeBy
library(dplyr)
library(readxl)
################################################################################
# Read data
################################################################################
d <- read_excel("C:/Users/MM/Desktop/UNI2/ME/DADES/database_V6.xlsx")
   setwd("C:/Users/MM/Desktop/UNI2/ME/SCRIPTS")
# Variable type
var_type <- sapply(d, class) # Variable type
var_Num  <- names(d)[var_type %in% c("integer",  "numeric")]
var_Cat  <- names(d)[var_type %in% c("character", "factor")]

#################################################################################
# Introduction to Data Explorer package
# More info: https://boxuancui.github.io/DataExplorer/
################################################################################
introduce(d)       # numeric summary
plot_intro(d)      # graphical summary       
plot_missing(d)    # Missing plot
plot_bar(d)        # Bar charts for categorical
plot_histogram(d)  # Histograms for continuous
plot_density(d)    # Density plots for continuous
plot_qq(d)         # QQ-norms for continuous
plot_correlation(d,
                 type = "cont", 
                 geom_text_args = list(size=1))# Correlations
is.numeric(d$temp_avarege)
is
shapiro.test(d$temp_avarege ~ d$cluster)
shapiro.test(Dsolar_energy ~ d$cluster)
shapiro.test(pressure ~ d$cluster)
#################################################################################
# Clustering with mixed data
#################################################################################
# Distance w/o variable 1 (positive/negative)
summary(d)
d[] <- lapply(d, function(x) if(is.character(x)) as.factor(x) else x)
distances <- daisy(d[-1], metric = "gower")
distances <- distances^2
summary(d)
### Plot clustering
cluster <- hclust(distances, method = "ward.D2")
plot(cluster)

# Cut for 3 clusters
as.dendrogram(cluster) %>% set("branches_k_color", k = 3) %>% plot()

# Allocate clusters
d$cluster <- factor(cutree(cluster, k = 3))
cluster.stats(distances, clustering = as.numeric(d$cluster))

# ==============================================================================
# Profiling
# ==============================================================================

################################################################################
# STAGE 1: VARIABLE SIGNIFICANCE
################################################################################

##-- Create folder for profiling results
pathProfiling <- "Profiling/"
if (!dir.exists(pathProfiling)) dir.create(pathProfiling)

##-- Variables to validate: remove the output and cluster
columns_validate <- colnames(d)[!colnames(d) %in% c("Dictamen", "cluster")]
significant_vars <- c()

sink(file = paste0(pathProfiling, "Test_stage1.txt"))

cV = "temp_avarege"
current_var <- d[, cV, drop=TRUE]
testSH <- shapiro.test(cV)
if (testSH$p.value > 0.05) {
  anova <- aov(current_var ~ d$cluster)
  cat("============ ", d$temp_avarege, " ================\n")
  print(anova); cat("\n")
  p_valor <- summary(anova)[[1]][["Pr(>F)"]][1]
  if (p_valor <= 0.05) {significant_vars <- c(significant_vars, d$temp_avarege)}
} else {
  test <- kruskal.test(current_var ~ d$cluster)
  cat("============ ", d$temp_avarege, " ================\n")
  print(test); cat("\n")
  if (test$p.value <= 0.05) {significant_vars <- c(significant_vars, d$temp_avarege)}
}

current_var= d$pressure
current_var <- as.numeric(current_var)
testSH <- shapiro.test(current_var)


gr_Boxplot <- ggboxplot(d, x = "cluster", y = cV, fill = "cluster")
gr_Hist    <- gghistogram(d, x = cV, add = "mean", rug = TRUE, 
                          color = "cluster", fill = "cluster")
gr <- ggarrange(gr_Boxplot, gr_Hist, heights = c(2, 0.7), 
                ncol = 2, nrow = 1, align = "v")
ggsave(filename = paste0(pathProfiling, "clustering_var_", cV, ".png"),
       plot = gr, bg = "white", width = 6, height = 3)

for (cV in columns_validate) {
  
  current_var <- d[, cV,drop=TRUE]
  
  # Variables numéricas
  if (inherits(current_var, "numeric") || inherits(current_var, "integer")) {
    current_var <- na.omit(current_var)
    if (length(unique(current_var)) > 1) {
      testSH <- shapiro.test(current_var)
      if (testSH$p.value > 0.05) {
        anova <- aov(current_var ~ d$cluster)
        cat("============ ", cV, " ================\n")
        print(anova); cat("\n")
        p_valor <- summary(anova)[[1]][["Pr(>F)"]][1]
        if (p_valor <= 0.05) {significant_vars <- c(significant_vars, cV)}
      } else {
        test <- kruskal.test(current_var ~ d$cluster)
        cat("============ ", cV, " ================\n")
        print(test); cat("\n")
        if (test$p.value <= 0.05) {significant_vars <- c(significant_vars, cV)}
      }
      gr_Boxplot <- ggboxplot(d, x = "cluster", y = cV, fill = "cluster")
      gr_Hist    <- gghistogram(d, x = cV, add = "mean", rug = TRUE, 
                                color = "cluster", fill = "cluster")
      gr <- ggarrange(gr_Boxplot, gr_Hist, heights = c(2, 0.7), 
                      ncol = 2, nrow = 1, align = "v")
      ggsave(filename = paste0(pathProfiling, "clustering_var_", cV, ".png"),
             plot = gr, bg = "white", width = 6, height = 3)
    }
  }
  
  
  # Categorical Variables  -----------------------------------------------------
  if (inherits(current_var, "factor") || inherits(current_var, "character")) {
    
    # Chi2 test
    test <- chisq.test(current_var, d$cluster)
    cat("============ ", cV, " ================\n")
    print(test); cat("\n")
    tabla <- data.frame(table(Var1=current_var, d$cluster))
    colnames(tabla)[which(colnames(tabla) == "Var2")] <- "cluster"
    if (test$p.value <= 0.05) {significant_vars <- c(significant_vars, cV)}
    
    # Graphical representation
    gr <- ggplot(tabla, aes(x = Var1, y = Freq, fill = cluster)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "", x = "", y = "") +
            theme_minimal()
    
    ggsave(filename = paste0(pathProfiling, "clustering_var_", cV, ".png"),
           plot = gr, bg = "white", width = 6, height = 3)
  }
}
sink()
chi_result <- with(mtcars,chisq.test(table(gear, d$cluster)))

##-- Centroids 
sink(file = paste0(pathProfiling, "/Centroides.txt"))

##-- Significant Numeric variables --> Centroids
#' WARNING: The "describeBy" instruction may give an error if no numerical 
#' variable has been significant in phase 1 of the clustering
sel_num <- c(d$pressure, d$temp_avarege)
describeBy(d[,sel_num], d$cluster)

##-- Significant Categorical variables --> Mode
sel_cat <- significant_vars[which(significant_vars %in% var_Cat)]
listaModa <- list()
for (vC in sel_cat){
  tabla   <- data.frame(table(d[,vC], d$cluster))
  calModa <- tabla %>%
              group_by(Var2) %>%
              filter(Freq == max(Freq)) %>%
              select(Var1, Var2)  %>% as.data.frame()
  colnames(calModa) <- c(vC, "cluster")
  cat("\n============ ", vC, " ================\n")
  print(calModa); cat("\n")
  listaModa[[vC]][["moda"]] <- calModa
}
sink()

# ------------------------------------------------------------------------------
### STAGE 2: SIGNIFICANCE OF MODALITIES
### Identify more significant modalities

res_catdes <- catdes(d, num.var = ncol(d))
res_catdes

##-- Barplot
plot(res_catdes, show = "quanti", 
     col.upper = "red", col.lower = "blue", 
     barplot = TRUE, cex.names = 1)

##-- Proportion
par(mfrow = c(1, 1))
plot(res_catdes, show = "quali", 
     col.upper = "red", col.lower = "blue", 
     barplot = FALSE, cex.names = 2)

plot(res_catdes, show = "all", 
     col.upper = "red", col.lower = "blue", barplot = FALSE, cex.names = 2)
