library(tidyverse)
library(lubridate)
library(rjson)


json_path <- "https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_natlan1_sst_day.json"
response <- fromJSON(file = json_path)

data <- lapply(X = response[2:44],
                            FUN = function (d) {
                              year <- as.numeric(unlist(d$name))
                              temps <- unlist(d$data)
                              
                              is_leap_year <- leap_year(year)
                              
                              if (!is_leap_year) {
                                temps <- append(temps, NA, 59)
                              }
                              
                              temps_length <- length(temps)
                              if (temps_length < 366) {
                                temps <- append(temps, rep(NA, 366 - temps_length), temps_length)
                              }
                              
                              df <- data.frame(temps)
                              colnames(df) <- c(year)
                              
                              return(df)
                            }) %>%
  do.call(cbind, .) %>%
  mutate(day_of_year = 1:nrow(.)) %>%
  select(day_of_year, everything())

data_long <- data %>%
  pivot_longer(cols = 2:44, names_to = "year") %>%
  arrange(year, day_of_year)

write_csv(x = data_long, file = "daily_atlantic_seasurface_temperatures.csv")
