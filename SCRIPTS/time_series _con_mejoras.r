# -----------------------------------------------------------------
# Cargar librerías necesarias
# -----------------------------------------------------------------
library(dplyr)
library(lubridate)
library(tseries)
library(forecast)

# -----------------------------------------------------------------
# 1. Lectura y preprocesamiento de datos
# -----------------------------------------------------------------

# Leer archivo CSV con los datos
data <- read.csv("C:/Users/marcd/OneDrive/Desktop/AP001.csv")

# Convertir la columna 'From Date' al formato de fecha y hora
data$From_Date <- as.POSIXct(data$From.Date, format="%Y-%m-%d %H:%M:%S")

# Extraer el año y el mes de la columna 'From Date'
data <- data %>%
  mutate(year = year(From_Date),
         month = month(From_Date))

# Calcular la media mensual de CO (mg/m3)
monthly_mean_CO <- data %>%
  group_by(year, month) %>%
  summarise(mean_CO = mean(`CO..mg.m3.`, na.rm = TRUE))

# Visualizar el resultado de las medias mensuales de CO
print(monthly_mean_CO)

# -----------------------------------------------------------------
# 2. Crear serie temporal y prueba de estacionaridad inicial
# -----------------------------------------------------------------

# Crear la serie temporal de CO a partir de las medias mensuales
ts_CO <- ts(monthly_mean_CO$mean_CO, start = c(2016, 7), frequency = 12)

# Graficar la serie temporal
plot(ts_CO, main = "Serie Temporal de CO (media mensual)", ylab = "CO (mg/m3)", xlab = "Tiempo")

# Realizar la prueba de Dickey-Fuller aumentada (ADF) para verificar estacionaridad
adf_test <- adf.test(ts_CO)

# Mostrar el resultado de la prueba ADF
print(adf_test)
# Interpretación: Si el p-valor es menor a 0.05, la serie es estacionaria.

# Graficar ACF y PACF de la serie temporal original
par(mfrow=c(1,2))
acf(ts_CO, main="ACF de la Serie Temporal de CO")
pacf(ts_CO, main="PACF de la Serie Temporal de CO")

# -----------------------------------------------------------------
# 3. Transformación logarímica, Box-Cox y diferenciación
# -----------------------------------------------------------------

# 3.1 Aplicar la transformación logarímica
log_ts_CO <- log(ts_CO)

# 3.2 Aplicar la transformación Box-Cox (lambda se estima automáticamente)
boxcox_lambda <- BoxCox.lambda(ts_CO)
boxcox_ts_CO <- BoxCox(ts_CO, lambda = boxcox_lambda)

# Comparación visual de ambas transformaciones
par(mfrow=c(2,1))
plot(log_ts_CO, main = "Transformación Logarímica", ylab = "Log(CO)")
plot(boxcox_ts_CO, main = paste("Transformación Box-Cox (lambda =", round(boxcox_lambda, 2), ")"), ylab = "Box-Cox(CO)")

# 3.3 Diferenciación de orden 12 para eliminar la estacionalidad
d12_log_ts_CO <- diff(log_ts_CO, lag = 12)
d12_boxcox_ts_CO <- diff(boxcox_ts_CO, lag = 12)

# 3.4 Diferenciación de primer orden para eliminar la tendencia
d1d12_log_ts_CO <- diff(d12_log_ts_CO)
d1d12_boxcox_ts_CO <- diff(d12_boxcox_ts_CO)

# Graficar las series diferenciadas
par(mfrow=c(2,1))
plot(d1d12_log_ts_CO, main = "Diferenciación Logarímica (Orden 12 + 1)")
plot(d1d12_boxcox_ts_CO, main = "Diferenciación Box-Cox (Orden 12 + 1)")

# 3.5 Graficar ACF y PACF de la serie diferenciada para ambas transformaciones
par(mfrow=c(2,2))
acf(d1d12_log_ts_CO, ylim = c(-1, 1), lag.max = 40, main="ACF Logarímica")
pacf(d1d12_log_ts_CO, ylim = c(-1, 1), lag.max = 40, main="PACF Logarímica")
acf(d1d12_boxcox_ts_CO, ylim = c(-1, 1), lag.max = 40, main="ACF Box-Cox")
pacf(d1d12_boxcox_ts_CO, ylim = c(-1, 1), lag.max = 40, main="PACF Box-Cox")

# 3.6 Realizar la prueba ADF nuevamente para verificar estacionaridad
adf_test_log <- adf.test(d1d12_log_ts_CO)
adf_test_boxcox <- adf.test(d1d12_boxcox_ts_CO)

print(adf_test_log)
print(adf_test_boxcox)
# Interpretación: Si el p-valor es menor a 0.05, la serie es estacionaria.

# -----------------------------------------------------------------
# 4. Ajuste del modelo ARMA para la transformación Box-Cox
# -----------------------------------------------------------------

# Definir un rango de valores para los modelos ARMA
arma_orders <- list(
  c(1, 0, 1),
  c(2, 0, 1),
  c(1, 0, 2),
  c(2, 0, 2),
  c(3, 0, 1),
  c(1, 0, 3)
)

# Inicializar una lista para almacenar los modelos y sus AICs
arma_models <- list()
arma_aics <- c()

# Ajustar diferentes modelos ARMA a la serie diferenciada Box-Cox
for (order in arma_orders) {
  model <- arima(d1d12_boxcox_ts_CO, order = order)
  arma_models[[paste(order, collapse = "-")]] <- model
  arma_aics <- c(arma_aics, AIC(model))
}

# Identificar el modelo con el menor AIC
best_model_index <- which.min(arma_aics)
best_model_order <- arma_orders[[best_model_index]]
best_model <- arma_models[[paste(best_model_order, collapse = "-")]]

# Mostrar el mejor modelo y su resumen
cat("El mejor modelo es ARMA(", paste(best_model_order, collapse = ", "), ") con AIC:", min(arma_aics), "\n")
summary(best_model)

# Graficar residuos del mejor modelo
tsdisplay(residuals(best_model), main=paste("Residuos del modelo ARMA(", paste(best_model_order, collapse = ", "), ")"))


Box.test(residuals(best_model), lag = 20, type = "Ljung-Box")


# Realizar predicción para los próximos 12 meses con el mejor modelo
forecast_best_model <- forecast(best_model, h = 12)

# Transformar las predicciones a la escala original
lambda <- boxcox_lambda  # Valor de lambda obtenido anteriormente

inverse_boxcox <- function(y, lambda) {
  if (lambda == 0) exp(y) else (lambda * y + 1)^(1 / lambda)
}

# Aplicar transformación inversa a las predicciones
pred_original <- inverse_boxcox(forecast_best_model$mean, lambda)
lower_original <- inverse_boxcox(forecast_best_model$lower[,2], lambda)  # Intervalo inferior 95%
upper_original <- inverse_boxcox(forecast_best_model$upper[,2], lambda)  # Intervalo superior 95%

# Graficar predicción en la escala original
plot(ts_CO, main = "Predicción del Mejor Modelo en Escala Original", ylab = "CO (mg/m3)", xlab = "Tiempo", xlim = c(2016, 2025))
lines(pred_original, col = "red", lwd = 2)
lines(lower_original, col = "blue", lty = 2)
lines(upper_original, col = "blue", lty = 2)


# -----------------------------------------------------------------
# Fin del script
# -----------------------------------------------------------------
