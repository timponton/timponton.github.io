# Proper course to follow

library(RCurl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(patchwork)

# read in data from Github
x <- getURL("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_confirmed.csv")
confirmCases <- read.csv(text = x)
y <- getURL("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_transmission_type.csv")
transmission <- read.csv(text = y)
z <- getURL("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_testing.csv")
tests <- read.csv(text = z)


# fix date formats
confirmCases$date <- as.Date(confirmCases$date, format = "%d-%m-%Y")
tests$date <- as.Date(tests$date, format = "%d-%m-%Y")

# calculate positive cases and rename too
positiveCases <- confirmCases %>% 
  group_by(date) %>% 
  tally() %>% 
  mutate(cumulative = cumsum(n)) %>% 
  select(date, cumulative) %>% 
  rename(Positive_Cases = cumulative)

# combine positive to cumulative tests and mutate negative cases
PosNegCase <- tests %>% 
  select(date, cumulative_tests) %>% 
  left_join(., positiveCases, by = "date") %>% 
  filter(Positive_Cases >= 1) %>% 
  mutate(Negative_cases = cumulative_tests - Positive_Cases)

# calculate per province
pro <- confirmCases %>% 
  group_by(date, province) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(province) %>% 
  mutate(cumulative = cumsum(n))


# pivot table wider
AllPro_wide <- pro %>% 
  select(date, province, cumulative) %>% 
  pivot_wider(names_from = province, values_from = cumulative)

# fill in missing zero for first date
AllPro_wide[1, 3:8][is.na(AllPro_wide[1, 3:8])] <- 0


# fill in missing values for cumulative sums
AllPro_wide <- AllPro_wide %>% 
  fill(KZN, GP, WC, MP, LP, FS, EC, .direction = "downup")

# pivot longer for better plotting 
AllPro_long <- AllPro_wide %>% 
  pivot_longer(-date, names_to = "Province", values_to = "NumCases")


# Calculate cases difference from previous day 
positiveCases <- positiveCases %>% 
  mutate(DifferencePrevious = Positive_Cases - lag(Positive_Cases, default = first(Positive_Cases)))

age <- ggplot(data = confirmCases, aes(x = age, fill = gender)) +
  geom_histogram(bins = 28.3, position = "dodge", col = "black") +
  theme_classic() +
  labs(title = "Age distribution and gender of positive cases in South Africa", x = "Age", y = "Number of cases", fill = "Gender") +
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(age)
