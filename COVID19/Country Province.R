library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)

# read in data from sheet
total <- read_xlsx("Daily report from NCID.xlsx", sheet = "Country")
province <- read_xlsx("Daily report from NCID.xlsx", sheet = "Province")

class(total$Date)

# Per Province
province_long <- province %>% 
  pivot_longer(-Date, names_to = "Province", values_to = "Cases")

p <- ggplot(data = province_long, aes(x = Date, y = Cases, col = Province)) +
  geom_line(size = 1)
ggplotly(p)

# Country wide


# Positive cases vs Active cases
PosCur <- total %>% 
  select(Date, `Positive cases`, Active) %>% 
  pivot_longer(-Date, names_to = "CaseType", values_to = "Cases")

ggplot(PosCur, aes(x = Date, y = Cases, col = CaseType)) +
  geom_line(size = 1)

# Total tested vs negative cases
ggplot(data = total, aes(x = Date, y = NegativeCases)) +
  geom_line()


