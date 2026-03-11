library(readxl)
dd<- read_excel("C:/Users/nuria/OneDrive/Documentos/ME/treball_en_grup/database_V6.xlsx")

install.packages("ggplot2")
library(ggplot2)
library(readxl)
dd <- read_excel("~/ME/treball_en_grup/database_V3.xlsx")
View(dd)

hist(dd$Severity_Score, main = "Histograma de Severity Score", xlab = "Severity Score")
plot(density(dd$Severity_Score), main = "Densitat de Severity Score")
summary(dd$Severity_Score)

model_gamma_all <- glm(Severity_Score  ~ . , family = Gamma(), data = dd)
summary(model_gamma_all)
plot(model_gamma_all)

model_gamma_v1 <- glm(Severity_Score  ~ date_time + City + severe_risk , family = Gamma(), data = dd)
summary(model_gamma_v1)
plot(model_gamma_v1)

model_gamma_v2 <- glm(Severity_Score  ~ City + severe_risk, family = Gamma(), data = dd)
summary(model_gamma_v2)
plot(modelo_gamma_v2)

ggplot2(dd$sunrise)



# Transforma la variable numérica en categórica
dd$sunrise_cat <- cut(dd$sunrise,
                            breaks = c(6:00:00, 6:30:00, 7:00:00 , 7:30:00, 8:15:00), # Cambia estos valores según tus categorías
                            labels = c("very early", "early", "late", "very late"))

# Verifica el resultado
table(datos$var_categorica)

install.packages("dplyr")
library(dplyr)

dd <- dd %>%
  mutate(sunrise_cat = case_when(
    sunrise >= as.POSIXct("06:00:00") & hora < as.POSIXct("06:30:00") ~ "very early",
    sunrise >= as.POSIXct("06:30:00") & hora < as.POSIXct("07:00:00") ~ "early",
    sunrise >= as.POSIXct("07:00:00") & hora < as.POSIXct("07:30:00") ~ "late",
    sunrise >= as.POSIXct("07:30:00") & hora < as.POSIXct("08:15:00") ~ "very late",
    TRUE ~ NA_character_  # Para los casos que no cumplen
  ))
