install.packages("writexl")
library(writexl)

dd <- read.csv2("C:/Users/MM/Desktop/UNI2/ME/DADES/miau2.csv")
head(miau2$severe_risk)
miau2$severe_risk <- as.numeric(as.character(miau2$severe_risk))
miau2$severe_risk <- ifelse(miau2$severe_risk >= 0 & miau2$severe_risk < 7, "Very_low_probability",
                           ifelse(miau2$severe_risk >= 7 & miau2$severe_risk < 15, "Low_probability",
                                  ifelse(miau2$severe_risk >= 15 & miau2$severe_risk < 25, "Medium_low_probability",
                                         ifelse(miau2$severe_risk >= 25 & miau2$severe_risk < 50, "Medium_probability",
                                                ifelse(miau2$severe_risk >= 50 & miau2$severe_risk < 60, "High_probability",
                                                       ifelse(miau2$severe_risk >= 60 & miau2$severe_risk < 100, "Very_high_probability", "WHAT"))))))
write_xlsx(miau2, "C:/Users/MM/Desktop/UNI2/ME/DADES/datafile.xlsx")
write.csv(miau2,file = "C:/Users/MM/Desktop/UNI2/ME/DADES/datafile.csv", row.names = FALSE, col.names = TRUE)

