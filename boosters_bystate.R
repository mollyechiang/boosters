# libraries
library(janitor)
library(plotly)
library(reshape2)
library(zoo)
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
  #filter(location %in% c("US", "MA")) %>% 
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
  filter(location != "VI") %>% 
  filter(location != "PW")

##----------PLOTTING BOOSTED PERCENT----------

# fig 1 - percent boosted by state
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

# fig 2 - facet_wrap by region 
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

# fig 3 - by vaccine type and region
vax_types <- for_plot %>%
  drop_na() %>% 
  group_by(region, date) %>% 
  mutate(mean_pfizer = mean(additional_doses_pfizer),
         mean_moderna = mean(additional_doses_moderna),
         mean_jj = mean(additional_doses_janssen)) %>% 
  select(date, region, mean_pfizer, mean_moderna, mean_jj) %>% 
  melt(id=c("date","region"))
  
fig3 <- vax_types %>% 
  ggplot(aes(x = date, y = value, color = interaction(region, variable))) +
  geom_line() + 
  labs(title = "Regional Administration of Booster Doses By Type",
       x = " ",
       y = "Total number of people to recieve additional dose",
       color = "Region",
       subtitle = "Data from CDC") +
  theme_minimal()

ggplotly(fig3)

# fig 4 - series complete vs boosted 
fig4 <- for_plot %>%
  drop_na() %>% 
  group_by(region,date) %>% 
  mutate(mean_boosted = mean(percent_boosted),
         mean_fully_vax = mean(series_complete_pop_pct)) %>% 
  ggplot(aes(x = date, y = mean_boosted, 
             color = region)) +
  geom_line() + 
  geom_line(aes(x = date, y = mean_fully_vax, 
                color = region)) +
  labs(title = "Regional Percents of Population Who Have Been Fully Vaccinated and Recieved a Booster Dose ",
       x = " ",
       y = "Percent of population",
       color = "Region",
       subtitle = "Data from CDC") +
  theme_minimal()

ggplotly(fig4)


##----------PLOTTING BOOSTERS BY DAY----------
us_byday <- total_data %>% 
  select(date, mmwr_week, location, distributed, administered, series_complete_pop_pct, 
         additional_doses,
         additional_doses_vax_pct, additional_doses_moderna, additional_doses_pfizer,
         additional_doses_janssen) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>%  
  filter(location == 'US') %>%
  filter(date >= '2021-08-23')

additional_administered_byday_list <- c(NA, diff(rev(us_byday$additional_doses)))

us_byday <- us_byday %>% 
  mutate(additional_administered_byday = rev(additional_administered_byday_list))

us_byday_rolling_means <- us_byday %>% 
  group_by(location) %>%
  mutate(boosters_3da = rollmean(additional_administered_byday, k = 3, fill = NA)) %>%
  mutate(boosters_7da = rollmean(additional_administered_byday, k = 7, fill = NA)) %>% 
  ungroup()

# plot
fig_byday <- ggplot(us_byday, aes(x = date, y = additional_administered_byday, 
                            color = location)) +
  geom_line() + 
  labs(title = "Daily National Booster Dose Administration in the US Over Time",
       x = " ",
       y = "Number of Booster Doses Administered",
       color = " ") +
  theme_minimal()

fig_byday_rolling <- ggplot(us_byday_rolling_means, aes(x = date, y = additional_administered_byday, 
                                  color = location)) +
  geom_line() + 
  geom_line(aes(x = date, y = boosters_3da, color = "3 Day Rolling Average")) + 
  geom_line(aes(x = date, y = boosters_7da, color = "7 Day Rolling Average")) + 
  labs(title = "Daily National Booster Dose Administration in the United States",
       x = " ",
       y = "Number of Booster Doses Administered",
       color = " ") +
  theme_minimal()

fig_byday_rolling7 <- ggplot(us_byday_rolling_means, aes(x = date, y = boosters_7da, 
                                                        color = "7 Day Rolling Average")) +
  geom_line() + 
  annotate(geom="text", x=as.Date("2021-11-25"), y=250000, label="1/25/21: Thanksgiving",
           color="black", size = 2.5) + 
  annotate(geom="text", x=as.Date("2021-12-01"), y=1300000, label="12/1/21: First Omicron\nCase Identified in US",
           color="black", size = 2.5) + 
  # https://www.cdc.gov/coronavirus/2019-ncov/variants/omicron-variant.html#:~:text=December%201%2C%202021%3A,of%20Omicron%20was%20identified.
  #annotate(geom="text", x=as.Date("2022-02-27"), y=520000, label="1/25/22: CDC Publishes\nData Indicating that\nOmicron is Less Severe\nthan Other Variants",
           #color="black", size = 2.5) + 
  # https://www.cdc.gov/mmwr/volumes/71/wr/mm7104e4.htm
  annotate(geom="text", x=as.Date("2022-01-31"), y=760000, label="1/15/22: Omicron Represents\n99.5% of all Sequenced\nVirus in the US",
           color="black", size = 2.5) + 
  # https://www.cidrap.umn.edu/news-perspective/2022/01/cdc-confirms-omicron-less-severe-other-variants
  labs(title = "Daily National Booster Dose Administration in the US Over Time",
       x = " ",
       y = "Number of Booster Doses Administered",
       color = " ",
       subtitle = "7 Day Rolling Average of CDC Data") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"))

ggplotly(fig_byday)
ggplotly(fig_byday_rolling)
ggplotly(fig_byday_rolling7)

fig_byday_rolling7





