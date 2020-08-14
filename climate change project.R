library(tidyverse)
library(dslabs)
data("temp_carbon")
data("greenhouse_gases")
data("historic_co2")
print(temp_carbon)

# ways of printing last year carbon emmissions are reported
temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>% 
  pull(year) %>% 
  max()
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

# first year for which carbon emissions data are available
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()

# ratio of latest to first carbon data
first <- temp_carbon %>%
  filter(temp_carbon$year==1751) %>%
  select(carbon_emissions)
last <- temp_carbon %>%
  filter(temp_carbon$year==2014) %>%
  select(carbon_emissions)
last/first

# first year for temp anomoly
first_temp_year <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  min()

# last year for temp anomoly
last_temp_year <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  max()

# increase in temp anomoly
temp_anon_last <- temp_carbon %>% 
  filter(temp_carbon$year==last_temp_year) %>% 
  select(temp_anomaly)
temp_anon_first <- temp_carbon %>% 
  filter(temp_carbon$year==first_temp_year) %>% 
  select(temp_anomaly)
temp_anon_last - temp_anon_first

# time series plot temp anomoly
p <- temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>%
  ggplot() + 
  geom_line(aes(year, temp_anomaly))
p

# with 20th century mean
avg_data <- temp_carbon %>% filter(year %in% c(1900:1999)) %>% select(temp_anomaly)
avg <- mean(avg_data$temp_anomaly)
p <- p + geom_hline(aes(yintercept=avg), col = "blue")


# add labels
p <- p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

# add other trendlines
p + geom_line(aes(year, ocean_anomaly), col = "green") +
  geom_line(aes(year, land_anomaly), col = "red") +
  ggthemes::theme_economist()

# create faceted grids for greenhouse gases
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000") +
  ggthemes::theme_economist()

# time series plot of carbon emissions
temp_carbon %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line()

# time series plot of historic co2 data
co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, col = source)) +
  geom_line()

# change x-axis limits to view different spikes
co2_time +
  xlim(-800000, -775000)
co2_time +
  xlim(-375000, -300000)
co2_time +
  xlim(-140000, -120000)
co2_time +
  xlim(-3000, 2018)
