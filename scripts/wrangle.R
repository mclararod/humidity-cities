library(tidyverse) # run the "cleaning.R" script

head(selected_humidity)

# wide into tidy
hum <- selected_humidity %>% gather(month, humidity_100, Jan:Dec)
  head(hum)

# reorder months to the chronol. order
hum$month <-factor(hum$month, levels = c("Jan", "Feb", "Mar", "Apr",
                                         "May", "Jun", "Jul", "Aug", 
                                         "Sep", "Oct", "Nov", "Dec"))

# graphing - adjustments needed
hum %>% group_by(Station.Name) %>% 
  ggplot(aes(month, humidity_100, color = Station.Name, group = Station.Name)) + 
  geom_point(alpha=0.7) +
  geom_line(aes(color = Station.Name), alpha=0.5) +
  xlab("Month") + ylab("Mean Humidity 1961-1990")
