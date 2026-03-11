library(readxl)
library(effects)
library(car)           # function Anova
library(emmeans)       # function emmeans
library(multcomp)      # function cld
library(multcompView)  # function cld    
library(dplyr)   
library(ggplot2)

#carreguem les dades
dd <- read_excel("C:/Users/nuria/OneDrive/Documentos/ME/treball_en_grup/database_V6.xlsx")

#analitzem les dades
str(dd)
summary(dd)
summary(dd$Severity_Score)

ggplot(dd, aes(x = Severity_Score)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Severity Score", x = "Severity Score", y = "Frecuencia") +
  theme_minimal()
qqnorm(dd$Severity_Score)
qqline(dd$Severity_Score, col = "red")

#model utilitzant familia GAUSSIANA
m0_gaus <- glm(Severity_Score ~ ., dd, family=gaussian)
summary(m0_gaus)
plot(m0_gaus)

m1_gaus <- glm(Severity_Score ~ Heat_Index + humidity + precipitation + wind_gust + wind_speed + uv_index + Day_of_Week + Health_Risk_Score, dd, family=gaussian)
summary(m1_gaus)
plot(m1_gaus)

m2_gaus <- glm(Severity_Score ~  Heat_Index + humidity + precipitation + wind_gust + uv_index + Health_Risk_Score, dd, family=gaussian)
summary(m2_gaus)
plot(m2_gaus)

#model utilitzant GAMMA
m1_Gamma <- glm(Severity_Score ~ Heat_Index + humidity + precipitation + wind_gust + wind_speed + uv_index + Health_Risk_Score, dd, family=Gamma())
summary(m1_Gamma)
plot(m1_Gamma)

Anova(m2_gaus, test.statistic = 'LR')

# Predicció del 30% dades (model ajustat amb l'altre 70%)

set.seed(1)
total_rows <- nrow(dd)
train_index <- sample(1:total_rows, size = round(0.7 * total_rows))

train_data <- dd[train_index, ]
test_data <- dd[-train_index, ]

m_train <- glm(Severity_Score ~ Heat_Index + humidity + precipitation + wind_gust + uv_index + Health_Risk_Score, 
               data = train_data, family = gaussian)
summary(m_train)

predictions <- predict(m_train, newdata = test_data)

plot(predictions~test_data$Severity_Score)