install.packages("writexl")
library(writexl)

dd <- read.csv2("C:/Users/MM/Desktop/UNI2/ME/DADES/datafile.csv")

datafile$severe_risk <- ifelse(datafile$severe_risk == "Very_low_probability", "Very_low",
                            ifelse(datafile$severe_risk == "Low_probability", "Low",
                                   ifelse(datafile$severe_risk == "Medium_low_probability", "Low_moderate",
                                          ifelse(datafile$severe_risk == "Medium_probability", "Moderate",
                                                 ifelse(datafile$severe_risk == "High_probability", "High",
                                                        ifelse(datafile$severe_risk == "Very_high_probability", "Very_high", "datafile$severe_risk"))))))
write_xlsx(datafile, "C:/Users/MM/Desktop/UNI2/ME/DADES/datafile2.xlsx")
write.csv(datafile,file = "C:/Users/MM/Desktop/UNI2/ME/DADES/datafile2.csv", row.names = FALSE, col.names = TRUE)

