library(tidyverse)
#library(reshape)

humidity <- read.csv("./data/raw.csv")
  View(humidity)

### Exploring and cleaning the data before treating it ###
# Note that the data we're interested in is the humidity %

head(humidity)
  summary(humidity) # There are a few "NA" values in the months variables

class(humidity$Jan)
  str(humidity)

## Are there any "NA" values?
mean(humidity$Jan)  # Observe  that it returns "NA" for all months. Probably a wrong observation that'd be worth excluding
mean(humidity$Mar)

summary(humidity$Jan)
summary(humidity$Mar)

# Identify the "NA": Create an index for months with "NA"

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

na_month <- is.na(humidity[month])
  which_na <- which(na_month == "TRUE")
  which_na
sum(na_month)    # Returns 26 ie 2 for each of the 13 months

na_jan <- is.na(humidity$Jan)
na_jan[1:10]
humidity$Station.Name[na_jan]     # Note that this could be an error when importing the data
# [1] "Footnote"               "code for missing value"

## Are there any outliers: Identifying

humidity[month]
hum_boxplot <- boxplot(humidity[month])   # All of the months seem to have an outlier at about -10k, 
                                          # which seems to be the minimum value

summary(humidity$Jan)
summary(humidity$Feb)
summary(humidity$Mar)
summary(humidity[month])    #  Note that some observations still have outliers -9999.9 which seem to be an error (no negative humidity) 
                            #  Because it's irrelevant, let's drop it

# Identifying the observations + creating a working table

keep_var <- c("Country.or.Territory", "Station.Name", "Period", "Element.Statistic.Qualifier.Code", "Statistic.Description", "Unit",
              "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  view(humidity %>% select(all_of(keep_var)) %>% filter_at(vars(month), all_vars(. < 0))) # This code says "for a certain obs, filter if all months are < 0"
  # length = 18. Remember that there are 2 obs with "NA"

wrk_humidity <- humidity %>% select(all_of(keep_var)) %>% filter_at(vars(month), any_vars(. >= 0)) # The expected result is 17 var & 5685 obs
  # This code says "for a certain obs, filter if any month is >= 0"
  # Note that this  table still has a lot of -9999.99 values
  # Rewrite the wrk_humidity

wrk_humidity <- humidity %>% select(all_of(keep_var)) %>% filter_at(vars(month), all_vars(. >= 0))  # This code says "for a certain obs, filter if all month are >= 0"

length(keep_var)  
  ncol(wrk_humidity)
  view(wrk_humidity)

summary(wrk_humidity[month])

## Final check

new_hum_boxplot <- boxplot(wrk_humidity[month])   # This is not what we want to see, but now we know the humidity values are OK!

names(wrk_humidity)
  levels(wrk_humidity$Country.or.Territory)   # Countries as characters. Transform them into factors

wrk_humidity$Country.or.Territory <- as.factor(wrk_humidity$Country.or.Territory)
  levels(wrk_humidity$Country.or.Territory)

summary(wrk_humidity)

## Is the data uniformly reported?

sort(humidity$Country.or.Territory)   # No, it's not. For US cities, use "Mean of 3-Hourly Observations".


### Manipulating the data you want ### 

## Selecting the cities you want in Brazil, Chile, Norway, Switzerland, and the USA

selected_humidity <- wrk_humidity %>% filter(wrk_humidity$Station.Name %in% c("Brasilia", "Rio de Janeiro", "Sao Paulo", "SANTIAGO-LOS CERRILLOS", "OSLO - BLINDERN", "Zuerich (town)", "Geneve",
                                                                              "SAN FRANCISCO/INTERNATIONAL A,CA", "BOSTON/LOGAN INT'L AIRPORT, MA", "WASHINGTON DC/NATIONAL ARPT VA", "HOUSTON/INTERCONTINENTAL, TX",
                                                                              "CHICAGO/0'HARE, IL"))
summary(c("Brasilia", "Rio de Janeiro", "Sao Paulo", "SANTIAGO-LOS CERRILLOS", "OSLO - BLINDERN", "Zuerich (town)", "Geneve",
          "SAN FRANCISCO/INTERNATIONAL A,CA", "BOSTON/LOGAN INT'L AIRPORT, MA", "WASHINGTON DC/NATIONAL ARPT VA", "HOUSTON/INTERCONTINENTAL, TX",
          "CHICAGO/0'HARE, IL"))

selected_humidity <- selected_humidity %>% filter(selected_humidity$Statistic.Description %in% c("Mean Value", "Mean of 3-Hourly Observations"))
summary(selected_humidity$Station.Name)  # Same lenght as the sequence defined above
view(selected_humidity)

intermed_selected <- selected_humidity
save(intermed_selected, file = "rda/intermed-selected.rda")

## Is the reporting period the same across all cities?
view(selected_humidity)

class(selected_humidity$Period)
same_period <- selected_humidity$Period == "1961-1990"
sum(same_period)/nrow(selected_humidity)
selected_humidity$Period[!same_period]
selected_humidity$Station.Name[!same_period]     # Houston is the only city which has an observation period different from 1961-1990

##########################

## Create a plottable data frame
# Data wrangling needed 
# All work below is unfinished

#test0 <- selected_humidity %>%  select(Station.Name, all_of(month))
#test <- reshape(test0,times=names(test0)[-1],timevar="Month",varying=names(test0)[-1],v.names="Humidity %",direction="long")
#test$Month  <- as.factor(test$Month)

# library(ggthemes)
# library(ggrepel)
# object <- test %>% ggplot(aes(Month, `Humidity %`, label = Station.Name)) #define the global
# object + geom_point(size = 3)+ geom_point(aes(col=Station.Name), size = 3) + geom_text_repel(size = 2)

