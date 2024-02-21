library(tidyverse)
library(lubridate)
library(rjson)

# load data from original source
json_path <- "https://climatereanalyzer.org/clim/t2_daily/json/era5_world_t2_day.json"
response <- fromJSON(file = json_path)

# parse and tidy data
data <- lapply(X = response[1:85],
               FUN = function (d) {
                 # get year and respective temperatures
                 year <- as.numeric(unlist(d$name))
                 temps <- unlist(d$data)
                 
                 # is the year a leap year?
                 is_leap_year <- leap_year(year)
                 
                 # if not, fill in a gap for 29th February
                 if (!is_leap_year) {
                   temps <- append(temps, NA, 59)
                 }
                 
                 # fill in gaps, if a year is not complete, yet
                 temps_length <- length(temps)
                 if (temps_length < 366) {
                   temps <- append(temps, rep(NA, 366 - temps_length), temps_length)
                 }
                 
                 # make a dataframe and use year as column name
                 df <- data.frame(temps)
                 colnames(df) <- c(year)
                 
                 return(df)
               }) %>%
  # make one big dataframe
  do.call(cbind, .) %>%
  # add day of year
  mutate(day_of_year = 1:nrow(.)) %>%
  # reorder
  select(day_of_year, everything())

# transform to long format
data_long <- data %>%
  pivot_longer(cols = 2:86, names_to = "year") %>%
  arrange(year, day_of_year)

# save
write_csv(x = data_long, file = "daily_surface_temperatures.csv")
