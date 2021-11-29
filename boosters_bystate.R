# libraries
library(janitor)
library(plotly)
library(tidyverse)

##-----------DATA LOAD----------
total_data <- read_csv("https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD") %>% 
  clean_names()

states <- c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA",
           "IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD",
           "DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AK", "LA", "OK", "TX","DC",
           "AZ", "CO", "NV", "NM", "UT", "WY", "AR", "CA", "HI", "OR", "WA", "ID", "MT")

regions <- data.frame(state = states, region = c(rep("Northeast", 9),
                                           rep("Midwest", 12),
                                           rep("South", 17),
                                           rep("West", 13)))

current_us_perc_boosted <- total_data %>% 
  filter(location == "US") %>% 
  select(date, location, additional_doses, additional_doses_vax_pct)

##----------ANALYSIS----------
boosters <- total_data %>% 
  select(date, mmwr_week, location, distributed, administered, admin_per_100k,
         administered_dose1_pop_pct, series_complete_pop_pct, additional_doses,
         additional_doses_vax_pct, additional_doses_18plus_vax_pct, additional_doses_50plus_vax_pct,
         additional_doses_65plus_vax_pct, additional_doses_moderna, additional_doses_pfizer,
         additional_doses_janssen) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>% 
  # filter out non-states
  filter(location != "BP2") %>%
  filter(location != "DD2") %>%
  filter(location != "AS") %>%
  filter(location != "FM") %>%
  filter(location != "GU") %>%
  filter(location != "IH2") %>%
  filter(location != "MH") %>%
  filter(location != "MP") %>%
  filter(location != "RP") %>%
  filter(location != "US") %>%
  filter(location != "VA2") %>%
  filter(location != "PR") %>%
  filter(location != "LTC") %>%
  filter(location != "VI")

##----------PLOTTING----------

for_plot <- boosters %>% 
  group_by(location, date) %>% 
  filter(date > "2021-10-08") %>% 
  left_join(regions, by = c("location" = "state")) %>%
  rename(percent_boosted = additional_doses_vax_pct)

fig <- ggplot(for_plot, aes(x = date, y = percent_boosted, 
                            color = location)) +
  geom_line() + 
  labs(title = "Percent of Population Who Have Recieved a Booster Dose of the COVID-19 Vaccine, By State",
       x = " ",
       y = "Percent of population who receieved a booster",
       color = "State",
       subtitle = "Data from CDC") +
  theme_minimal()

# facet_wrap
fig2 <- for_plot %>%
  drop_na() %>% 
  ggplot(aes(x = date, y = percent_boosted, 
             color = interaction(location, region),
             text = paste("state:", location))) +
  geom_line() + 
  labs(title = "Percent of Population Who Have Recieved a Booster Dose of the COVID-19 Vaccine, By State",
       x = " ",
       y = "Percent of population who receieved a booster",
       color = "State",
       subtitle = "Data from CDC") +
  facet_wrap(~region) +
  theme_minimal()

ggplotly(fig)
ggplotly(fig2, tooltip = c("date", "percent_boosted", "text"))
