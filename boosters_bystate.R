# libraries
library(janitor)
library(plotly)
library(tidyverse)

##-----------DATA LOAD----------
total_data <- read_csv("https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD") %>% 
  clean_names()

##----------ANALYSIS----------
boosters <- total_data %>% 
  select(date, mmwr_week, location, distributed, administered, admin_per_100k,
         administered_dose1_pop_pct, series_complete_pop_pct, additional_doses,
         additional_doses_vax_pct, additional_doses_18plus_vax_pct, additional_doses_50plus_vax_pct,
         additional_doses_65plus_vax_pct, additional_doses_moderna, additional_doses_pfizer,
         additional_doses_janssen) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y"))

##----------PLOTTING----------

x <- boosters %>% 
  group_by(location, date) %>% 
  filter(date > "2021-10-01") 

fig <- ggplot(x, aes(x = date, y = additional_doses_vax_pct, color = location)) +
  geom_line() + 
  labs(title = "Percent of Population Who Have Recieved a Booster Dose\nof the COVID-19 Vaccine By State",
       x = " ",
       y = "Percent of population who receieved a booster") +
  theme_minimal()

ggplotly(fig)
