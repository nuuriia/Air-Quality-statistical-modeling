dd <- read.csv2("C:/Users/MM/Desktop/UNI2/ME/DADES/dataweather - Sheet1.csv")
dd
head(dataweather...Sheet1$moon_phase)
dataweather...Sheet1$moon_phase <- ifelse(dataweather...Sheet1$moon_phase >= 0 & dataweather...Sheet1$moon_phase < 0.05, "New_moon",
                        ifelse(dataweather...Sheet1$moon_phase >= 0.05 & dataweather...Sheet1$moon_phase < 0.17, "Waxing_crescent",
                               ifelse(dataweather...Sheet1$moon_phase >= 0.17 & dataweather...Sheet1$moon_phase < 0.33, "First_quarter",
                                      ifelse(dataweather...Sheet1$moon_phase >= 0.33 & dataweather...Sheet1$moon_phase < 0.5, "Waxing_gibbous",
                                             ifelse(dataweather...Sheet1$moon_phase == 0.5, "Full_moon",
                                                    ifelse(dataweather...Sheet1$moon_phase >= 0.5 & dataweather...Sheet1$moon_phase < 0.65, "Waning_gibbous",
                                                           ifelse(dataweather...Sheet1$moon_phase >= 0.65 & dataweather...Sheet1$moon_phase < 0.8, "Third_quarter",
                                                                  ifelse(dataweather...Sheet1$moon_phase >= 0.8 & dataweather...Sheet1$moon_phase < 0.95, "Waning_crescent",
                                                                         ifelse(dataweather...Sheet1$moon_phase >= 0.95 & dataweather...Sheet1$moon_phase <= 1, "New_moon", dataweather...Sheet1$moon_phase)))))))))
write.csv(dataweather...Sheet1,file = "C:/Users/MM/Desktop/UNI2/ME/DADES/miau2.csv", row.names = FALSE, col.names = TRUE)

