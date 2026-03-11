library(readxl)
library(effects)
library(car)
library(emmeans)
library(multcomp)     
library(multcompView)
library(dplyr)   
library(ggplot2)

dd <- data

# Prepare data: Create a 'Next_Day_Severity_Score' column
dd <- dd %>%
  arrange(date_time) %>%  # Ensure data is sorted by date
  mutate(Next_Day_Severity_Score = lead(Severity_Score, 1))  # Shift target variable up by 1

dd <- na.omit(dd)

Q1 <- quantile(dd$Next_Day_Severity_Score, 0.25)
Q3 <- quantile(dd$Next_Day_Severity_Score, 0.75)
IQR_value <- IQR(dd$Next_Day_Severity_Score)

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 2 * IQR_value
upper_bound <- Q3 + 2 * IQR_value

# Filter out outliers
dd <- dd %>%
  filter(Next_Day_Severity_Score >= lower_bound & Next_Day_Severity_Score <= upper_bound)


# Normalize the predictor variables
normalize <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

dd <- dd %>%
  mutate(
    heat_humidity = normalize(Heat_Index * humidity),
    visibility = normalize(visibility),
    Heat_Index = log(Heat_Index + 1),
    Next_Day_Severity_Score = normalize(Next_Day_Severity_Score),
    temp_max = normalize(temp_max),
    wind_speed = normalize(log(wind_speed + 1)),
    temp_min = normalize(temp_min^(3)),
    Heat_Index = normalize(Heat_Index),
    solar_radiation = normalize(solar_radiation^3),
    humidity = normalize(humidity),
    precipitation = normalize(log(precipitation + 1)),
    wind_gust = normalize(wind_gust^2),
    Health_Risk_Score = normalize(Health_Risk_Score^2)
  )

str(dd)

# Split data into training (70%) and test (30%) sets
set.seed(1)
total_rows <- nrow(dd)
train_index <- sample(1:total_rows, size = round(0.7 * total_rows))

train_data <- dd[train_index, ]
test_data <- dd[-train_index, ]

# Fit the model
m_train <- glm(
  Next_Day_Severity_Score ~ (Heat_Index + visibility + temp_min + temp_max + solar_radiation + humidity + precipitation + wind_gust + Health_Risk_Score), 
  data = train_data, family = gaussian
)

m_train <- step(m_train)
summary(m_train)


# Predictions
predictions <- predict(m_train, newdata = test_data)

# Performance metrics
mse <- mean((test_data$Next_Day_Severity_Score - predictions)^2)
rmse <- sqrt(mse)
mae <- mean(abs(test_data$Next_Day_Severity_Score - predictions))
ss_total <- sum((test_data$Next_Day_Severity_Score - mean(test_data$Next_Day_Severity_Score))^2)
ss_residual <- sum((test_data$Next_Day_Severity_Score - predictions)^2)
r_squared <- 1 - (ss_residual / ss_total)

cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared:", r_squared, "\n")

# Plot the predicted vs actual Next Day Severity Scores
plot(predictions ~ test_data$Next_Day_Severity_Score,
     xlab = "Actual Next Day Severity Score",
     ylab = "Predicted Next Day Severity Score",
     main = "Predicted vs Actual Next Day Severity Score",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)

