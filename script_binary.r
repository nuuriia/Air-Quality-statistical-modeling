# Instalar las bibliotecas necesarias si no están ya instaladas
# install.packages("pROC")
# install.packages("ggplot2")

# Cargar las bibliotecas
library(ggplot2)
library(pROC)

# Cargar los datos
data <- read.csv("E:/Documentos/RStudio/database_V3.csv", stringsAsFactors = TRUE)

# Convertir 'precipitation.boolean' a variable binaria si no lo está ya
data$precipitation.boolean <- as.numeric(data$precipitation.boolean)

# Asegurarse de que 'temp_avarege' no sea un factor
data$temp_avarege <- as.numeric(as.character(data$temp_avarege))

# Eliminar filas con NA en las variables utilizadas para el modelo
data <- na.omit(data)

data$precipitation <- gsub(",", ".", data$precipitation)
data$precipitation <- as.numeric(data$precipitation)
data$temp_min <- gsub(",", ".", data$temp_min)
data$temp_min <- as.numeric(data$temp_min)
data$cloud_cover <- gsub(",", ".", data$cloud_cover)
data$cloud_cover <- as.numeric(data$cloud_cover)
data$Temp_Range <- gsub(",", ".", data$Temp_Range)
data$Temp_Range <- as.numeric(data$Temp_Range)
data$dew_point <- gsub(",", ".", data$dew_point)
data$dew_point <- as.numeric(data$dew_point)
data$humidity <- gsub(",", ".", data$humidity)
data$humidity <- as.numeric(data$humidity)
data$pressure <- gsub(",", ".", data$pressure)
data$pressure <- as.numeric(data$pressure)

cor.test(data$dew_point, data$precipitation.boolean, method = "pearson")
cor(data$dew_point, data$precipitation, method = "pearson")

cor.test(data$humidity, data$precipitation.boolean, method = "pearson")
cor(data$humidity, data$precipitation, method = "pearson")

cor.test(data$pressure, data$precipitation.boolean, method = "pearson")
cor(data$pressure, data$precipitation, method = "pearson")

modelo_glm <- glm(precipitation.boolean ~ dew_point + pressure, 
                  data = data, family = binomial)
summary(modelo_glm)


# Dividir los datos en entrenamiento (70%) y prueba (30%)
set.seed(123)
train_index <- sample(1:nrow(data), 0.7*nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Ajustar el modelo con los datos de entrenamiento
modelo_glm_train <- glm(precipitation.boolean ~ temp_avarege + Temp_Range + cloud_cover, 
                        data = train_data, family = binomial)

# Predicción en el conjunto de prueba
predicciones_test <- predict(modelo_glm_train, newdata = test_data, type = "response")

# Convertir probabilidades a clases binarias
clases_predichas_test <- ifelse(predicciones_test > 0.5, 1, 0)

# Matriz de confusión para evaluar el rendimiento del modelo en los datos de prueba
table(Predicted = clases_predichas_test, Actual = test_data$precipitation.boolean)

# Calcular la precisión del modelo en los datos de prueba
precision_test <- mean(clases_predichas_test == test_data$precipitation.boolean)
print(paste("Precisión en conjunto de prueba:", precision_test))

# --- Graficar Curva ROC ---
roc_curve <- roc(test_data$precipitation.boolean, predicciones_test)
plot(roc_curve, main="Curva ROC para el modelo logístico")
auc(roc_curve)
# --- Graficar el ajuste del modelo logístico ---
ggplot(data, aes(x=temp_avarege, y=precipitation.boolean)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = binomial), col = "blue") +
  labs(title="Ajuste del modelo logístico", x="Temperatura promedio", y="Probabilidad de precipitación")

# --- Graficar las predicciones frente a los valores reales ---
ggplot(data.frame(Real = test_data$precipitation.boolean, Predicho = predicciones_test), aes(x=Real, y=Predicho)) +
  geom_point(alpha=0.5, color="blue") +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title="Predicciones vs. Valores reales", x="Valor real", y="Valor predicho")


