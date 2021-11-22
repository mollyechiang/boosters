# libraries
library(janitor)
library(tidyverse)

##-----------DATA LOAD----------
total_data <- read_csv("https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD") %>% 
  clean_names()

##----------ANALYSIS----------
x <- total_data %>% 
  select(date, mmwr_week, location, distributed, administered, admin_per_100k,
         administered_dose1_pop_pct, series_complete_pop_pct, additional_doses,
         additional_doses_vax_pct, additional_doses_18plus_vax_pct, additional_doses_50plus_vax_pct,
         additional_doses_65plus_vax_pct, additional_doses_moderna, additional_doses_pfizer,
         additional_doses_janssen)
