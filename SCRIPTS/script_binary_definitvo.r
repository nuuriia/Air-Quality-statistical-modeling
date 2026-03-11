# Load required libraries
library(caret)
library(dplyr)
library(PRROC)
library(glmnet)

#data<-database

# Convertir 'precipitation.boolean' a variable binaria si no lo está ya
data$precipitation.boolean <- as.factor(data$precipitation.boolean)
data$date <- as.Date(data$date_time)

# ---- City distance matrix ----
# Load required library
library(geosphere)

# Define city coordinates
cities <- data.frame(
  City = c("Houston", "Philadelphia", "Phoenix", "Los Angeles", "New York City",
           "San Antonio", "San Diego", "San Jose", "Chicago"),
  Latitude = c(29.7604, 39.9526, 33.4484, 34.0522, 40.7128, 
               29.4241, 32.7157, 37.3382, 41.8781),
  Longitude = c(-95.3698, -75.1652, -112.0740, -118.2437, -74.0060, 
                -98.4936, -117.1611, -121.8863, -87.6298)
)

# Create lagged variables for the previous 3 days (lag1, lag2, lag3)
data <- data %>%
  arrange(City, date) %>%
  group_by(City) %>%
  mutate(
    dew_point_lag1 = lag(dew_point, 1),
    pressure_lag1 = lag(pressure, 1),
    temp_avarege_lag1 = lag(temp_avarege, 1),
    temp_min_lag1 = lag(temp_min, 1),
    visibility_lag1 = lag(visibility, 1),
    wind_gust_lag1 = lag(wind_gust, 1),
    Severity_Score_lag1 = lag(Severity_Score, 1),
    rain_lag1 = lag(precipitation.boolean, 1),
    Sunrise_cat_lag1 = lag(Sunrise_cat, 1),
    Sunset_cat_lag1 = lag(Sunset_cat, 1),
    Temp_Range_lag1 = lag(Temp_Range, 1),
    
    # Add lag2 and lag3
    dew_point_lag2 = lag(dew_point, 2),
    pressure_lag2 = lag(pressure, 2),
    temp_avarege_lag2 = lag(temp_avarege, 2),
    temp_min_lag2 = lag(temp_min, 2),
    visibility_lag2 = lag(visibility, 2),
    wind_gust_lag2 = lag(wind_gust, 2),
    Severity_Score_lag2 = lag(Severity_Score, 2),
    rain_lag2 = lag(precipitation.boolean, 2),
    Sunrise_cat_lag2 = lag(Sunrise_cat, 2),
    Sunset_cat_lag2 = lag(Sunset_cat, 2),
    Temp_Range_lag2 = lag(Temp_Range, 2),
    
    # Add lag3
    dew_point_lag3 = lag(dew_point, 3),
    pressure_lag3 = lag(pressure, 3),
    temp_avarege_lag3 = lag(temp_avarege, 3),
    temp_min_lag3 = lag(temp_min, 3),
    visibility_lag3 = lag(visibility, 3),
    wind_gust_lag3 = lag(wind_gust, 3),
    Severity_Score_lag3 = lag(Severity_Score, 3),
    rain_lag3 = lag(precipitation.boolean, 3),
    Sunrise_cat_lag3 = lag(Sunrise_cat, 3),
    Sunset_cat_lag3 = lag(Sunset_cat, 3),
    Temp_Range_lag3 = lag(Temp_Range, 3),
    
    # Add interactions
    temp_dew_interaction_lag1 = temp_avarege_lag1 * dew_point_lag1,
    temp_dew_interaction_lag2 = temp_avarege_lag2 * dew_point_lag2,
    temp_dew_interaction_lag3 = temp_avarege_lag3 * dew_point_lag3,
    
    temp_wg_lag1 = temp_avarege_lag1 * wind_gust_lag1,
    temp_wg_lag2 = temp_avarege_lag2 * wind_gust_lag2,
    temp_wg_lag3 = temp_avarege_lag3 * wind_gust_lag3
  ) %>%
  ungroup()

# Ensure that lagged variables are filled correctly by removing NA values
data <- na.omit(data)

# Create new temporal features (optional)
data <- data %>%
  mutate(
    month = as.numeric(format(date, "%m")),
    day_of_year = as.numeric(format(date, "%j"))
  )

# --- Lasso Regularization ---
# Prepare the model matrix (this will include all features for the Lasso model)
x <- model.matrix(precipitation.boolean ~ dew_point_lag1 + dew_point_lag2 + dew_point_lag3 +
                    pressure_lag1 + pressure_lag2 + pressure_lag3 +
                    temp_avarege_lag1 + temp_avarege_lag2 + temp_avarege_lag3 +
                    temp_min_lag1 + temp_min_lag2 + temp_min_lag3 +
                    visibility_lag1 + visibility_lag2 + visibility_lag3 +
                    wind_gust_lag1 + wind_gust_lag2 + wind_gust_lag3 +
                    Severity_Score_lag1 + Severity_Score_lag2 + Severity_Score_lag3 +
                    rain_lag1 + rain_lag2 + rain_lag3 +
                    Sunrise_cat_lag1 + Sunrise_cat_lag2 + Sunrise_cat_lag3 +
                    Sunset_cat_lag1 + Sunset_cat_lag2 + Sunset_cat_lag3 +
                    Temp_Range_lag1 + Temp_Range_lag2 + Temp_Range_lag3 +
                    month + day_of_year + City +
                    temp_dew_interaction_lag1 + temp_dew_interaction_lag2 +
                    temp_dew_interaction_lag3 + temp_wg_lag1 + temp_wg_lag2 +
                    temp_wg_lag3, data = data)
y <- as.factor(data$precipitation.boolean)

# Fit the Lasso model using cv.glmnet (cross-validation for optimal lambda)
lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1)

# Check the lambda value that minimizes cross-validation error
best_lambda <- lasso_model$lambda.min
print(paste("Best lambda for Lasso: ", best_lambda))

# Get the coefficients for the model with the best lambda
lasso_coef <- coef(lasso_model, s = "lambda.min")
print(lasso_coef)

# --- BACKWARD SELECTION with Lasso Selected Features ---
# Build the logistic regression model with the selected features from Lasso#
#modelo_glm_selected_lasso <- glm(precipitation.boolean ~ ., data = data[selected_features], family = binomial)
modelo_glm_selected_lasso <- glm(precipitation.boolean ~ 
                                   pressure_lag3 + 
                                   temp_avarege_lag2 + 
                                   Severity_Score_lag2 + 
                                   Severity_Score_lag3 + 
                                   rain_lag3 + 
                                   Sunrise_cat_lag1 + 
                                   day_of_year +
                                   City +
                                   temp_dew_interaction_lag1,
                                 data = data, 
                                 family = binomial)

# Perform backward selection
modelo_glm_selected <- stats::step(modelo_glm_selected_lasso, direction = "backward")

# Print the summary of the final selected model after backward selection
summary(modelo_glm_selected)

# --- CROSS-VALIDATION ---
# Set up 5-fold cross-validation
train_control <- trainControl(
  method = "cv",         # cross-validation
  number = 5,            # 5 folds
  verboseIter = TRUE,    # show progress
)

# Get the final formula from the selected model
selected_formula <- modelo_glm_selected$formula

# Create the data for cross-validation using the selected formula
cv_data <- model.frame(selected_formula, data)

# Perform 5-fold cross-validation to evaluate the model using the selected variables
cv_results <- train(selected_formula, data = cv_data, method = "glm", family = "binomial", trControl = train_control)

# Print cross-validation results
print(cv_results)

# Get the accuracy from cross-validation
cv_results$results

# Predicciones y evaluación en el conjunto de validación (cross-validation)
cv_predictions <- predict(cv_results, newdata = cv_data)
cv_confusion_matrix <- table(Predicted = cv_predictions, Actual = data$precipitation.boolean)

# Calculate accuracy from cross-validation
cv_accuracy <- mean(cv_predictions == data$precipitation.boolean)
print(paste("Cross-validation accuracy:", round(cv_accuracy, 3)))

# --- Optional: Feature Importance ---
# Check the feature importance (if needed)
importance <- varImp(cv_results, scale = FALSE)
print(importance)

  
