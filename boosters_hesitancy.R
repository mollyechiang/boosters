#libraries 
library(janitor)
library(stringr)
library(scales)
library(reshape2)
library(ggpubr)
library(geofacet)
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function
library(tidyverse)

# include received_all_covid19_required_doses_exclude_booster? 
# covid_booster_likely_get 
  # 1 = yes, 2 = no, 3 = not sure, 4 = already have booster, 5 = no answer

##---------------DATA LOAD------------------
## 2022 - read in data from Dec 27th to Feb 20th (first 7 weeks of 2022)
# select relevant columns, and pull weights from separate weight data -- and merge
# with responses
surveym102 <- read.csv("2022/CovidNearYou Datafile 02 Jan 09PM (last 7).csv") %>%
  select(response_id, getting_covid19_booster, covid19_booster_likely_to_get, 
         reason_not_get_covid19_booster_mc_inconvenient,
         reason_not_get_covid19_booster_mc_something_else,
         reason_not_get_covid19_booster_mc_too_new,
         reason_not_get_covid19_booster_mc_side_effect,
         reason_not_get_covid19_booster_mc_threat_exaggerated,
         reason_not_get_covid19_booster_mc_distrust_government,
         reason_not_get_covid19_booster_mc_distrust_scientist,
         reason_not_get_covid19_booster_mc_too_political,
         reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated,
         reason_not_get_covid19_booster_mc_reaction,
         reason_not_get_covid19_booster_mc_not_at_risk,
         reason_not_get_covid19_booster_mc_others_before_me,
         reason_not_get_covid19_booster_mc_other_text,
         which_covid19_vaccine,
         get_vaccine_yesno, state, zip_code, start_date, response_date, ideology, race, 
         income, party_id, gender, race_recode, educ4, is_essential_worker, age7, age)

surveym102w <- read.csv("2022/CovidNearYou Weight 02 Jan 09PM (last 7).csv") %>%
  select(response_id, weight_daily_national_13plus, weight_state_weekly, weight_state_monthly)

surveym102 <- surveym102 %>% 
  left_join(surveym102w, by = "response_id")

surveym109 <- read.csv("2022/CovidNearYou Datafile 09 Jan 09PM (last 7).csv") %>%
  select(response_id, getting_covid19_booster, covid19_booster_likely_to_get, 
         reason_not_get_covid19_booster_mc_inconvenient,
         reason_not_get_covid19_booster_mc_something_else,
         reason_not_get_covid19_booster_mc_too_new,
         reason_not_get_covid19_booster_mc_side_effect,
         reason_not_get_covid19_booster_mc_threat_exaggerated,
         reason_not_get_covid19_booster_mc_distrust_government,
         reason_not_get_covid19_booster_mc_distrust_scientist,
         reason_not_get_covid19_booster_mc_too_political,
         reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated,
         reason_not_get_covid19_booster_mc_reaction,
         reason_not_get_covid19_booster_mc_not_at_risk,
         reason_not_get_covid19_booster_mc_others_before_me,
         reason_not_get_covid19_booster_mc_other_text,
         which_covid19_vaccine,
         get_vaccine_yesno, state, zip_code, start_date, response_date, ideology, race, 
         income, party_id, gender, race_recode, educ4, is_essential_worker, age7, age)

surveym109w <- read.csv("2022/CovidNearYou Weight 09 Jan 09PM (last 7).csv") %>%
  select(response_id, weight_daily_national_13plus, weight_state_weekly, weight_state_monthly)

surveym109 <- surveym109 %>% 
  left_join(surveym109w, by = "response_id")

surveym116 <- read.csv("2022/CovidNearYou Datafile 16 Jan 09PM (last 7).csv") %>%
  select(response_id, getting_covid19_booster, covid19_booster_likely_to_get, 
         reason_not_get_covid19_booster_mc_inconvenient,
         reason_not_get_covid19_booster_mc_something_else,
         reason_not_get_covid19_booster_mc_too_new,
         reason_not_get_covid19_booster_mc_side_effect,
         reason_not_get_covid19_booster_mc_threat_exaggerated,
         reason_not_get_covid19_booster_mc_distrust_government,
         reason_not_get_covid19_booster_mc_distrust_scientist,
         reason_not_get_covid19_booster_mc_too_political,
         reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated,
         reason_not_get_covid19_booster_mc_reaction,
         reason_not_get_covid19_booster_mc_not_at_risk,
         reason_not_get_covid19_booster_mc_others_before_me,
         reason_not_get_covid19_booster_mc_other_text,
         which_covid19_vaccine,
         get_vaccine_yesno, state, zip_code, start_date, response_date, ideology, race, 
         income, party_id, gender, race_recode, educ4, is_essential_worker, age7, age)

surveym116w <- read.csv("2022/CovidNearYou Weight 16 Jan 09PM (last 7).csv") %>%
  select(response_id, weight_daily_national_13plus, weight_state_weekly, weight_state_monthly)

surveym116 <- surveym116 %>% 
  left_join(surveym116w, by = "response_id")

surveym123 <- read.csv("2022/CovidNearYou Datafile 23 Jan 09PM (last 7).csv") %>%
  select(response_id, getting_covid19_booster, covid19_booster_likely_to_get, 
         reason_not_get_covid19_booster_mc_inconvenient,
         reason_not_get_covid19_booster_mc_something_else,
         reason_not_get_covid19_booster_mc_too_new,
         reason_not_get_covid19_booster_mc_side_effect,
         reason_not_get_covid19_booster_mc_threat_exaggerated,
         reason_not_get_covid19_booster_mc_distrust_government,
         reason_not_get_covid19_booster_mc_distrust_scientist,
         reason_not_get_covid19_booster_mc_too_political,
         reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated,
         reason_not_get_covid19_booster_mc_reaction,
         reason_not_get_covid19_booster_mc_not_at_risk,
         reason_not_get_covid19_booster_mc_others_before_me,
         reason_not_get_covid19_booster_mc_other_text,
         which_covid19_vaccine,
         get_vaccine_yesno, state, zip_code, start_date, response_date, ideology, race, 
         income, party_id, gender, race_recode, educ4, is_essential_worker, age7, age)

surveym123w <- read.csv("2022/CovidNearYou Weight 23 Jan 09PM (last 7).csv") %>%
  select(response_id, weight_daily_national_13plus, weight_state_weekly, weight_state_monthly)

surveym123 <- surveym123 %>% 
  left_join(surveym123w, by = "response_id")

surveym130 <- read.csv("2022/CovidNearYou Datafile 30 Jan 09PM (last 7).csv") %>%
  select(response_id, getting_covid19_booster, covid19_booster_likely_to_get, 
         reason_not_get_covid19_booster_mc_inconvenient,
         reason_not_get_covid19_booster_mc_something_else,
         reason_not_get_covid19_booster_mc_too_new,
         reason_not_get_covid19_booster_mc_side_effect,
         reason_not_get_covid19_booster_mc_threat_exaggerated,
         reason_not_get_covid19_booster_mc_distrust_government,
         reason_not_get_covid19_booster_mc_distrust_scientist,
         reason_not_get_covid19_booster_mc_too_political,
         reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated,
         reason_not_get_covid19_booster_mc_reaction,
         reason_not_get_covid19_booster_mc_not_at_risk,
         reason_not_get_covid19_booster_mc_others_before_me,
         reason_not_get_covid19_booster_mc_other_text,
         which_covid19_vaccine,
         get_vaccine_yesno, state, zip_code, start_date, response_date, ideology, race, 
         income, party_id, gender, race_recode, educ4, is_essential_worker, age7, age)

surveym130w <- read.csv("2022/CovidNearYou Weight 30 Jan 09PM (last 7).csv") %>%
  select(response_id, weight_daily_national_13plus, weight_state_weekly, weight_state_monthly)

surveym130 <- surveym130 %>% 
  left_join(surveym130w, by = "response_id")


surveym206 <- read.csv("2022/CovidNearYou Datafile 06 Feb 09PM (last 7).csv") %>%
  select(response_id, getting_covid19_booster, covid19_booster_likely_to_get, 
         reason_not_get_covid19_booster_mc_inconvenient,
         reason_not_get_covid19_booster_mc_something_else,
         reason_not_get_covid19_booster_mc_too_new,
         reason_not_get_covid19_booster_mc_side_effect,
         reason_not_get_covid19_booster_mc_threat_exaggerated,
         reason_not_get_covid19_booster_mc_distrust_government,
         reason_not_get_covid19_booster_mc_distrust_scientist,
         reason_not_get_covid19_booster_mc_too_political,
         reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated,
         reason_not_get_covid19_booster_mc_reaction,
         reason_not_get_covid19_booster_mc_not_at_risk,
         reason_not_get_covid19_booster_mc_others_before_me,
         reason_not_get_covid19_booster_mc_other_text,
         which_covid19_vaccine,
         get_vaccine_yesno, state, zip_code, start_date, response_date, ideology, race, 
         income, party_id, gender, race_recode, educ4, is_essential_worker, age7, age)

surveym206w <- read.csv("2022/CovidNearYou Weight 06 Feb 09PM (last 7).csv") %>%
  select(response_id, weight_daily_national_13plus, weight_state_weekly, weight_state_monthly)

surveym206 <- surveym206 %>% 
  left_join(surveym206w, by = "response_id")

surveym213 <- read.csv("2022/CovidNearYou Datafile 13 Feb 09PM (last 7).csv") %>%
  select(response_id, getting_covid19_booster, covid19_booster_likely_to_get, 
         reason_not_get_covid19_booster_mc_inconvenient,
         reason_not_get_covid19_booster_mc_something_else,
         reason_not_get_covid19_booster_mc_too_new,
         reason_not_get_covid19_booster_mc_side_effect,
         reason_not_get_covid19_booster_mc_threat_exaggerated,
         reason_not_get_covid19_booster_mc_distrust_government,
         reason_not_get_covid19_booster_mc_distrust_scientist,
         reason_not_get_covid19_booster_mc_too_political,
         reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated,
         reason_not_get_covid19_booster_mc_reaction,
         reason_not_get_covid19_booster_mc_not_at_risk,
         reason_not_get_covid19_booster_mc_others_before_me,
         reason_not_get_covid19_booster_mc_other_text,
         which_covid19_vaccine,
         get_vaccine_yesno, state, zip_code, start_date, response_date, ideology, race, 
         income, party_id, gender, race_recode, educ4, is_essential_worker, age7, age)

surveym213w <- read.csv("2022/CovidNearYou Weight 13 Feb 09PM (last 7).csv") %>%
  select(response_id, weight_daily_national_13plus, weight_state_weekly, weight_state_monthly)

surveym213 <- surveym213 %>% 
  left_join(surveym213w, by = "response_id")

surveym220 <- read.csv("2022/CovidNearYou Datafile 20 Feb 09PM (last 7).csv") %>%
  select(response_id, getting_covid19_booster, covid19_booster_likely_to_get, 
         reason_not_get_covid19_booster_mc_inconvenient,
         reason_not_get_covid19_booster_mc_something_else,
         reason_not_get_covid19_booster_mc_too_new,
         reason_not_get_covid19_booster_mc_side_effect,
         reason_not_get_covid19_booster_mc_threat_exaggerated,
         reason_not_get_covid19_booster_mc_distrust_government,
         reason_not_get_covid19_booster_mc_distrust_scientist,
         reason_not_get_covid19_booster_mc_too_political,
         reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated,
         reason_not_get_covid19_booster_mc_reaction,
         reason_not_get_covid19_booster_mc_not_at_risk,
         reason_not_get_covid19_booster_mc_others_before_me,
         reason_not_get_covid19_booster_mc_other_text,
         which_covid19_vaccine,
         get_vaccine_yesno, state, zip_code, start_date, response_date, ideology, race, 
         income, party_id, gender, race_recode, educ4, is_essential_worker, age7, age)

surveym220w <- read.csv("2022/CovidNearYou Weight 20 Feb 09PM (last 7).csv") %>%
  select(response_id, weight_daily_national_13plus, weight_state_weekly, weight_state_monthly)

surveym220 <- surveym220 %>% 
  left_join(surveym220w, by = "response_id")

##---------------MERGE DATA------------------
# combine data
twenty22_total <- surveym102 %>% 
  rbind(surveym109) %>% 
  rbind(surveym116) %>% 
  rbind(surveym123) %>% 
  rbind(surveym130) %>%
  rbind(surveym206) %>%
  rbind(surveym213) %>%
  rbind(surveym220) %>% 
  # make response date into a date 
  # MEC - ben used start date instead of response date- why? 
  mutate(response_date = as.Date(response_date)) %>%
  # select the first 6 weeks of 2022 (Jan 1 to Feb 12th)
  filter(response_date >= as.Date("2022-01-01")) %>%
  filter(response_date <= as.Date("2022-02-12"))

twenty22 <- twenty22_total %>% 
  # drop those with no weights 
  drop_na(weight_daily_national_13plus) %>%
  # add dividions by week 
  mutate(week = cut.Date(response_date, breaks = "1 week", labels = FALSE)) %>% 
  # code into vaccinated and unvaccinated - drop those unsure of vaccination 
  filter(get_vaccine_yesno != 3) %>%
  mutate(get_vaccine_yesno = replace(get_vaccine_yesno, get_vaccine_yesno == 1, "Vaccinated")) %>%
  mutate(get_vaccine_yesno = replace(get_vaccine_yesno, get_vaccine_yesno == 2, "Unvaccinated")) %>% 
  select(-start_date) %>% 
  arrange(response_date)

analysis <- twenty22 %>% 
  # filter for only those not willing or unsure of booster (only ones who were asked the questions)
  filter(covid19_booster_likely_to_get %in% c(2,3)) %>% 
  # filter for vaccinated 
  filter(get_vaccine_yesno == "Vaccinated")

excluded <- nrow(twenty22_total) - nrow(analysis)

##-----------------WEIGHTED PERCENTS---------------

# overall in 2022
percent_likely <- analysis %>%
  # get the percentage of vaccinated, but unboosted respondents in 2022 that cited each of the 
  # following reasons for not getting boosted 
  mutate(incv_percent = weighted.mean(reason_not_get_covid19_booster_mc_inconvenient, weight_daily_national_13plus)) %>%
  mutate(smthe_percent = weighted.mean(reason_not_get_covid19_booster_mc_something_else, weight_daily_national_13plus)) %>%
  mutate(new_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_new, weight_daily_national_13plus)) %>%
  mutate(sidee_percent = weighted.mean(reason_not_get_covid19_booster_mc_side_effect, weight_daily_national_13plus)) %>% 
  mutate(texg_percent = weighted.mean(reason_not_get_covid19_booster_mc_threat_exaggerated, weight_daily_national_13plus)) %>% 
  mutate(disgov_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_government, weight_daily_national_13plus)) %>% 
  mutate(dissci_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_scientist, weight_daily_national_13plus)) %>% 
  mutate(toop_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_political, weight_daily_national_13plus)) %>% 
  mutate(gotcovvax_percent = weighted.mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, weight_daily_national_13plus)) %>%
  mutate(rxn_percent = weighted.mean(reason_not_get_covid19_booster_mc_reaction, weight_daily_national_13plus)) %>% 
  mutate(norisk_percent = weighted.mean(reason_not_get_covid19_booster_mc_not_at_risk, weight_daily_national_13plus)) %>% 
  mutate(oths_percent = weighted.mean(reason_not_get_covid19_booster_mc_others_before_me, weight_daily_national_13plus)) %>% 
  #mutate(other_percent = weighted.mean(reason_not_get_covid19_booster_mc_other_text, weight_daily_national_13plus))
  select(incv_percent, smthe_percent, new_percent, sidee_percent, texg_percent, disgov_percent, dissci_percent, toop_percent,
         gotcovvax_percent, rxn_percent, norisk_percent, oths_percent) %>% 
  distinct()

# by day
percent_likely_day <- analysis %>%
  group_by(response_date) %>%
  mutate(incv_percent = weighted.mean(reason_not_get_covid19_booster_mc_inconvenient, weight_daily_national_13plus)) %>%
  mutate(smthe_percent = weighted.mean(reason_not_get_covid19_booster_mc_something_else, weight_daily_national_13plus)) %>%
  mutate(new_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_new, weight_daily_national_13plus)) %>%
  mutate(sidee_percent = weighted.mean(reason_not_get_covid19_booster_mc_side_effect, weight_daily_national_13plus)) %>% 
  mutate(texg_percent = weighted.mean(reason_not_get_covid19_booster_mc_threat_exaggerated, weight_daily_national_13plus)) %>% 
  mutate(disgov_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_government, weight_daily_national_13plus)) %>% 
  mutate(dissci_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_scientist, weight_daily_national_13plus)) %>% 
  mutate(toop_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_political, weight_daily_national_13plus)) %>% 
  mutate(gotcovvax_percent = weighted.mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, weight_daily_national_13plus)) %>%
  mutate(rxn_percent = weighted.mean(reason_not_get_covid19_booster_mc_reaction, weight_daily_national_13plus)) %>% 
  mutate(norisk_percent = weighted.mean(reason_not_get_covid19_booster_mc_not_at_risk, weight_daily_national_13plus)) %>% 
  mutate(oths_percent = weighted.mean(reason_not_get_covid19_booster_mc_others_before_me, weight_daily_national_13plus))

# by week
# MEC - change to week weights 
percent_likely_week <- analysis %>%
  group_by(week) %>%
  mutate(incv_percent = weighted.mean(reason_not_get_covid19_booster_mc_inconvenient, weight_daily_national_13plus)) %>%
  mutate(smthe_percent = weighted.mean(reason_not_get_covid19_booster_mc_something_else, weight_daily_national_13plus)) %>%
  mutate(new_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_new, weight_daily_national_13plus)) %>%
  mutate(sidee_percent = weighted.mean(reason_not_get_covid19_booster_mc_side_effect, weight_daily_national_13plus)) %>% 
  mutate(texg_percent = weighted.mean(reason_not_get_covid19_booster_mc_threat_exaggerated, weight_daily_national_13plus)) %>% 
  mutate(disgov_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_government, weight_daily_national_13plus)) %>% 
  mutate(dissci_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_scientist, weight_daily_national_13plus)) %>% 
  mutate(toop_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_political, weight_daily_national_13plus)) %>% 
  mutate(gotcovvax_percent = weighted.mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, weight_daily_national_13plus)) %>%
  mutate(rxn_percent = weighted.mean(reason_not_get_covid19_booster_mc_reaction, weight_daily_national_13plus)) %>% 
  mutate(norisk_percent = weighted.mean(reason_not_get_covid19_booster_mc_not_at_risk, weight_daily_national_13plus)) %>% 
  mutate(oths_percent = weighted.mean(reason_not_get_covid19_booster_mc_others_before_me, weight_daily_national_13plus))

# by week and state
percent_likely_week_state <- analysis %>%
  group_by(week, state) %>%
  mutate(incv_percent = weighted.mean(reason_not_get_covid19_booster_mc_inconvenient, weight_daily_national_13plus)) %>%
  mutate(smthe_percent = weighted.mean(reason_not_get_covid19_booster_mc_something_else, weight_daily_national_13plus)) %>%
  mutate(new_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_new, weight_daily_national_13plus)) %>%
  mutate(sidee_percent = weighted.mean(reason_not_get_covid19_booster_mc_side_effect, weight_daily_national_13plus)) %>% 
  mutate(texg_percent = weighted.mean(reason_not_get_covid19_booster_mc_threat_exaggerated, weight_daily_national_13plus)) %>% 
  mutate(disgov_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_government, weight_daily_national_13plus)) %>% 
  mutate(dissci_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_scientist, weight_daily_national_13plus)) %>% 
  mutate(toop_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_political, weight_daily_national_13plus)) %>% 
  mutate(gotcovvax_percent = weighted.mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, weight_daily_national_13plus)) %>%
  mutate(rxn_percent = weighted.mean(reason_not_get_covid19_booster_mc_reaction, weight_daily_national_13plus)) %>% 
  mutate(norisk_percent = weighted.mean(reason_not_get_covid19_booster_mc_not_at_risk, weight_daily_national_13plus)) %>% 
  mutate(oths_percent = weighted.mean(reason_not_get_covid19_booster_mc_others_before_me, weight_daily_national_13plus))

# by party 
percent_likely_party <- analysis %>%
  group_by(party_id) %>%
  mutate(incv_percent = weighted.mean(reason_not_get_covid19_booster_mc_inconvenient, weight_daily_national_13plus)) %>%
  mutate(smthe_percent = weighted.mean(reason_not_get_covid19_booster_mc_something_else, weight_daily_national_13plus)) %>%
  mutate(new_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_new, weight_daily_national_13plus)) %>%
  mutate(sidee_percent = weighted.mean(reason_not_get_covid19_booster_mc_side_effect, weight_daily_national_13plus)) %>% 
  mutate(texg_percent = weighted.mean(reason_not_get_covid19_booster_mc_threat_exaggerated, weight_daily_national_13plus)) %>% 
  mutate(disgov_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_government, weight_daily_national_13plus)) %>% 
  mutate(dissci_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_scientist, weight_daily_national_13plus)) %>% 
  mutate(toop_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_political, weight_daily_national_13plus)) %>% 
  mutate(gotcovvax_percent = weighted.mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, weight_daily_national_13plus)) %>%
  mutate(rxn_percent = weighted.mean(reason_not_get_covid19_booster_mc_reaction, weight_daily_national_13plus)) %>% 
  mutate(norisk_percent = weighted.mean(reason_not_get_covid19_booster_mc_not_at_risk, weight_daily_national_13plus)) %>% 
  mutate(oths_percent = weighted.mean(reason_not_get_covid19_booster_mc_others_before_me, weight_daily_national_13plus))

# by age
percent_likely_age <- analysis %>%
  group_by(age7) %>%
  mutate(incv_percent = weighted.mean(reason_not_get_covid19_booster_mc_inconvenient, weight_daily_national_13plus)) %>%
  mutate(smthe_percent = weighted.mean(reason_not_get_covid19_booster_mc_something_else, weight_daily_national_13plus)) %>%
  mutate(new_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_new, weight_daily_national_13plus)) %>%
  mutate(sidee_percent = weighted.mean(reason_not_get_covid19_booster_mc_side_effect, weight_daily_national_13plus)) %>% 
  mutate(texg_percent = weighted.mean(reason_not_get_covid19_booster_mc_threat_exaggerated, weight_daily_national_13plus)) %>% 
  mutate(disgov_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_government, weight_daily_national_13plus)) %>% 
  mutate(dissci_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_scientist, weight_daily_national_13plus)) %>% 
  mutate(toop_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_political, weight_daily_national_13plus)) %>% 
  mutate(gotcovvax_percent = weighted.mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, weight_daily_national_13plus)) %>%
  mutate(rxn_percent = weighted.mean(reason_not_get_covid19_booster_mc_reaction, weight_daily_national_13plus)) %>% 
  mutate(norisk_percent = weighted.mean(reason_not_get_covid19_booster_mc_not_at_risk, weight_daily_national_13plus)) %>% 
  mutate(oths_percent = weighted.mean(reason_not_get_covid19_booster_mc_others_before_me, weight_daily_national_13plus))

# by willingness to be boosted
percent_likely_will <- analysis %>%
  group_by(covid19_booster_likely_to_get) %>%
  mutate(incv_percent = weighted.mean(reason_not_get_covid19_booster_mc_inconvenient, weight_daily_national_13plus)) %>%
  mutate(smthe_percent = weighted.mean(reason_not_get_covid19_booster_mc_something_else, weight_daily_national_13plus)) %>%
  mutate(new_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_new, weight_daily_national_13plus)) %>%
  mutate(sidee_percent = weighted.mean(reason_not_get_covid19_booster_mc_side_effect, weight_daily_national_13plus)) %>% 
  mutate(texg_percent = weighted.mean(reason_not_get_covid19_booster_mc_threat_exaggerated, weight_daily_national_13plus)) %>% 
  mutate(disgov_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_government, weight_daily_national_13plus)) %>% 
  mutate(dissci_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_scientist, weight_daily_national_13plus)) %>% 
  mutate(toop_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_political, weight_daily_national_13plus)) %>% 
  mutate(gotcovvax_percent = weighted.mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, weight_daily_national_13plus)) %>%
  mutate(rxn_percent = weighted.mean(reason_not_get_covid19_booster_mc_reaction, weight_daily_national_13plus)) %>% 
  mutate(norisk_percent = weighted.mean(reason_not_get_covid19_booster_mc_not_at_risk, weight_daily_national_13plus)) %>% 
  mutate(oths_percent = weighted.mean(reason_not_get_covid19_booster_mc_others_before_me, weight_daily_national_13plus))

# by  type of first shot 
percent_likely_vaxtype <- analysis %>%
  group_by(which_covid19_vaccine) %>%
  mutate(incv_percent = weighted.mean(reason_not_get_covid19_booster_mc_inconvenient, weight_daily_national_13plus)) %>%
  mutate(smthe_percent = weighted.mean(reason_not_get_covid19_booster_mc_something_else, weight_daily_national_13plus)) %>%
  mutate(new_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_new, weight_daily_national_13plus)) %>%
  mutate(sidee_percent = weighted.mean(reason_not_get_covid19_booster_mc_side_effect, weight_daily_national_13plus)) %>% 
  mutate(texg_percent = weighted.mean(reason_not_get_covid19_booster_mc_threat_exaggerated, weight_daily_national_13plus)) %>% 
  mutate(disgov_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_government, weight_daily_national_13plus)) %>% 
  mutate(dissci_percent = weighted.mean(reason_not_get_covid19_booster_mc_distrust_scientist, weight_daily_national_13plus)) %>% 
  mutate(toop_percent = weighted.mean(reason_not_get_covid19_booster_mc_too_political, weight_daily_national_13plus)) %>% 
  mutate(gotcovvax_percent = weighted.mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, weight_daily_national_13plus)) %>%
  mutate(rxn_percent = weighted.mean(reason_not_get_covid19_booster_mc_reaction, weight_daily_national_13plus)) %>% 
  mutate(norisk_percent = weighted.mean(reason_not_get_covid19_booster_mc_not_at_risk, weight_daily_national_13plus)) %>% 
  mutate(oths_percent = weighted.mean(reason_not_get_covid19_booster_mc_others_before_me, weight_daily_national_13plus))

##---------------PLOTS----------------
# overall - 2022
percent_likely %>% 
  rename("inconvenient" = incv_percent,
         "something else" = smthe_percent,
         "too new" = new_percent,
         "side effects" = sidee_percent,
         "threat exaggerated" = texg_percent,
         "distrust gov" = disgov_percent,
         "distrust science" = dissci_percent,
         "too political" = toop_percent,
         "got COVID and vaccine" = gotcovvax_percent,
         "reaction" = rxn_percent,
         "not at risk" = norisk_percent, 
         "others before me" = oths_percent) %>% 
  melt(id.vars = NULL) %>% 
  arrange(desc(value)) %>% 
  ggplot(aes(x = reorder(variable, -value), y = value, fill = variable)) +
  geom_col() +
  labs(x = "Reason",
       y = "Percent of Respondents",
       title = "Reasons Vaccinated Respondents Cited For Not Getting the Booster") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  #theme(axis.text.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position = "none")
  
# by week - overall 
percent_likely_week %>% 
  select(response_date, week, sidee_percent, gotcovvax_percent, toop_percent,
         disgov_percent, dissci_percent) %>% 
  mutate(response_date = as.Date(response_date)) %>%
  ggplot(aes(x = response_date)) +
  geom_line(aes(y = sidee_percent, color = "Side Effects")) +
  geom_line(aes(y = gotcovvax_percent, color = "Got COVID and the Vaccine")) +
  geom_line(aes(y = toop_percent, color = "Too Political")) +
  geom_line(aes(y = disgov_percent, color = "Distrust Government")) +
  geom_line(aes(y = dissci_percent, color = "Distrust Science")) +
  labs(y = "Percent of Respondents", 
       x = "Date",
       title = "Reasons for Not Getting the Booster in 2022",
       caption = "Data from SurveyMonkey and OutbreaksNearMe Poll",
       color = "Reason") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  ylim(0.1,0.45)

# by party 
percent_likely_party %>% 
  select(party_id, incv_percent, smthe_percent, new_percent, sidee_percent, texg_percent, disgov_percent, dissci_percent, toop_percent,
         gotcovvax_percent, rxn_percent, norisk_percent, oths_percent) %>% 
  distinct() %>% 
  filter(party_id != 4) %>%
  mutate(party_id = replace(party_id, party_id == 1, "Republican")) %>% 
  mutate(party_id = replace(party_id, party_id == 2, "Democrat")) %>% 
  mutate(party_id = replace(party_id, party_id == 3, "Independent")) %>% 
  rename("inconvenient" = incv_percent,
         "something else" = smthe_percent,
         "too new" = new_percent,
         "side effects" = sidee_percent,
         "threat exaggerated" = texg_percent,
         "distrust gov" = disgov_percent,
         "distrust science" = dissci_percent,
         "too political" = toop_percent,
         "got COVID and vaccine" = gotcovvax_percent,
         "reaction" = rxn_percent,
         "not at risk" = norisk_percent, 
         "others before me" = oths_percent) %>% 
  melt(id.vars = 'party_id') %>% 
  ggplot(aes(x = variable, y = value, fill = party_id)) +
  geom_col() +
  facet_wrap(~party_id, ncol = 1) +
  labs(x = " ",
       y = "Percent of Respondents",
       title = "Reasons Vaccinated Respondents Cited For Not Getting the Booster, By Political Party") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position = "none")

# by willingness
percent_likely_will%>% 
  select(covid19_booster_likely_to_get, incv_percent, smthe_percent, new_percent, sidee_percent, texg_percent, disgov_percent, dissci_percent, toop_percent,
         gotcovvax_percent, rxn_percent, norisk_percent, oths_percent) %>% 
  distinct() %>% 
  mutate(covid19_booster_likely_to_get = replace(covid19_booster_likely_to_get, covid19_booster_likely_to_get == 4, "Already Got")) %>% 
  mutate(covid19_booster_likely_to_get = replace(covid19_booster_likely_to_get, covid19_booster_likely_to_get == 1, "Yes")) %>% 
  mutate(covid19_booster_likely_to_get = replace(covid19_booster_likely_to_get, covid19_booster_likely_to_get == 2, "No")) %>% 
  mutate(covid19_booster_likely_to_get = replace(covid19_booster_likely_to_get, covid19_booster_likely_to_get == 3, "Not Sure")) %>% 
  rename("inconvenient" = incv_percent,
         "something else" = smthe_percent,
         "too new" = new_percent,
         "side effects" = sidee_percent,
         "threat exaggerated" = texg_percent,
         "distrust gov" = disgov_percent,
         "distrust science" = dissci_percent,
         "too political" = toop_percent,
         "got COVID and vaccine" = gotcovvax_percent,
         "reaction" = rxn_percent,
         "not at risk" = norisk_percent, 
         "others before me" = oths_percent) %>% 
  melt(id.vars = 'covid19_booster_likely_to_get') %>% 
  ggplot(aes(x = variable, y = value, fill = covid19_booster_likely_to_get)) +
  geom_col(position = "dodge") +
  labs(x = " ",
       y = "Weighted Percent of Respondents",
       title = "Reasons Vaccinated Respondents Cited For Not Getting the Booster, By Willingness",
       fill = "Willing to Get Booster?") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# by vax type
percent_likely_vaxtype %>% 
  select(which_covid19_vaccine, incv_percent, smthe_percent, new_percent, sidee_percent, texg_percent, disgov_percent, dissci_percent, toop_percent,
         gotcovvax_percent, rxn_percent, norisk_percent, oths_percent) %>% 
  distinct() %>% 
  filter(which_covid19_vaccine != 4) %>% 
  filter(which_covid19_vaccine != 5) %>% 
  mutate(which_covid19_vaccine = replace(which_covid19_vaccine, which_covid19_vaccine == 1, "Moderna")) %>% 
  mutate(which_covid19_vaccine = replace(which_covid19_vaccine, which_covid19_vaccine == 2, "Pfizer")) %>% 
  mutate(which_covid19_vaccine = replace(which_covid19_vaccine, which_covid19_vaccine == 3, "Johnson & Johnson")) %>% 
  rename("inconvenient" = incv_percent,
         "something else" = smthe_percent,
         "too new" = new_percent,
         "side effects" = sidee_percent,
         "threat exaggerated" = texg_percent,
         "distrust gov" = disgov_percent,
         "distrust science" = dissci_percent,
         "too political" = toop_percent,
         "got COVID and vaccine" = gotcovvax_percent,
         "reaction" = rxn_percent,
         "not at risk" = norisk_percent, 
         "others before me" = oths_percent) %>% 
  melt(id.vars = 'which_covid19_vaccine') %>% 
  ggplot(aes(x = variable, y = value, fill = which_covid19_vaccine)) +
  geom_col(position = "dodge", width=0.7) +
  labs(x = " ",
       y = "Weighted Percent of Respondents",
       title = "Reasons Vaccinated Respondents Cited For Not Getting the Booster, By Vaccine Type",
       fill = "Vaccine Type") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# by age group
percent_likely_age %>% 
  select(age7, incv_percent, smthe_percent, new_percent, sidee_percent, texg_percent, disgov_percent, dissci_percent, toop_percent,
         gotcovvax_percent, rxn_percent, norisk_percent, oths_percent) %>% 
  distinct() %>% 
  filter(age7 != 8) %>% 
  mutate(age7 = replace(age7, age7 == 1, "13-24")) %>% 
  mutate(age7 = replace(age7, age7 == 2, "13-24")) %>% 
  mutate(age7 = replace(age7, age7 == 3, "25-44")) %>% 
  mutate(age7 = replace(age7, age7 == 4, "25-44")) %>% 
  mutate(age7 = replace(age7, age7 == 5, "45-64")) %>% 
  mutate(age7 = replace(age7, age7 == 6, "45-64")) %>% 
  mutate(age7 = replace(age7, age7 == 7, "65+")) %>% 
  rename("inconvenient" = incv_percent,
         "something else" = smthe_percent,
         "too new" = new_percent,
         "side effects" = sidee_percent,
         "threat exaggerated" = texg_percent,
         "distrust gov" = disgov_percent,
         "distrust science" = dissci_percent,
         "too political" = toop_percent,
         "got COVID and vaccine" = gotcovvax_percent,
         "reaction" = rxn_percent,
         "not at risk" = norisk_percent, 
         "others before me" = oths_percent) %>% 
  melt(id.vars = 'age7') %>% 
  ggplot(aes(x = variable, y = value, fill = age7)) +
  geom_col(position = "dodge") +
  labs(x = " ",
       y = "Weighted Percent of Respondents",
       title = "Reasons Vaccinated Respondents Cited For Not Getting the Booster, By Age Group",
       fill = "Age Group") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
