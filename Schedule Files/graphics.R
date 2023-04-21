library(tidyverse)

SOMSC <- read.csv("SOMSCCO2eq1980.csv")

ggplot(data = SOMSC) +
  geom_line(mapping = aes(time, somsc, color = "SOMSC"), size = 2) +
  theme_light() +
  ylab("Soil Organic Matter Soil Carbon (CO2eq kg per hectare)") +
  xlab("Year") +
  ggtitle("Soil Carbon Stock Change for Richards Farm from 1980 to 2022")

CO2eq <- read.csv("ghgemissions.csv")

ggplot(data = CO2eq) +
  geom_line(mapping = aes(time, n2o, color = "N2O"), size = 2) +
  geom_line(mapping = aes(time, ch4, color = "CH4"), size = 2) +
  theme_light() +
  ylab("Nitrous Oxide and Methane Oxide Emissions (CO2eq kg per hectare)") +
  xlab("Year") +
  ggtitle("Greenhouse Gas Emissions for Richards Farm from 2002 to 2022")

