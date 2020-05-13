# Humidity Data Cleaning
  # Removal of NA's and outliers
  # Selection of variables of interest
  # Selection of cities of interest

library(tidyverse)

# Import the data

humidity <- read.csv("./data/raw.csv")

# Selection of variables
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
keep_var <- c("Country.or.Territory", "Station.Name", "Period", "Statistic.Description", "Unit", 
              "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Selection of cities of interests
# Elimination of NA's and outliers (check "exploring.R" for details)

wrk_humidity <- humidity %>% select(all_of(keep_var)) %>% filter_at(vars(month), all_vars(. >= 0))
wrk_humidity$Country.or.Territory <- as.factor(wrk_humidity$Country.or.Territory)
new_hum_boxplot <- boxplot(wrk_humidity[month]) # Data is already good to go


selected_humidity <- wrk_humidity %>% filter(wrk_humidity$Station.Name %in% c("Brasilia", 
                                                                              "Rio de Janeiro", 
                                                                              "Sao Paulo", 
                                                                              "SANTIAGO-LOS CERRILLOS", 
                                                                              "OSLO - BLINDERN", 
                                                                              "Zuerich (town)", 
                                                                              "Geneve",
                                                                              "SAN FRANCISCO/INTERNATIONAL A,CA", 
                                                                              "BOSTON/LOGAN INT'L AIRPORT, MA", 
                                                                              "WASHINGTON DC/NATIONAL ARPT VA", 
                                                                              "HOUSTON/INTERCONTINENTAL, TX",
                                                                              "CHICAGO/0'HARE, IL"))
selected_humidity <- selected_humidity %>% filter(selected_humidity$Statistic.Description %in% 
                                                    c("Mean Value", "Mean of 3-Hourly Observations"))

  save(selected_humidity, file = "rda/selected-interm-1.rda")

# Note: Houston is the only city with a different observation period.