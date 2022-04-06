# libraries 
library(janitor) # for cleaning read-in data
library(scales) # for visaluzations 
library(reshape2) # for melt
library(srvyr) # for survey analysis
library(zoo) # for rolling means 
library(tidyverse)

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
         received_all_covid19_required_doses,
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
         received_all_covid19_required_doses,
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
         received_all_covid19_required_doses,
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
         received_all_covid19_required_doses,
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
         received_all_covid19_required_doses,
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
         received_all_covid19_required_doses,
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
         received_all_covid19_required_doses,
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
         received_all_covid19_required_doses,
         get_vaccine_yesno, state, zip_code, start_date, response_date, ideology, race, 
         income, party_id, gender, race_recode, educ4, is_essential_worker, age7, age)

surveym220w <- read.csv("2022/CovidNearYou Weight 20 Feb 09PM (last 7).csv") %>%
  select(response_id, weight_daily_national_13plus, weight_state_weekly, weight_state_monthly)

surveym220 <- surveym220 %>% 
  left_join(surveym220w, by = "response_id")

surveym227 <- read.csv("2022/CovidNearYou Datafile 27 Feb 09PM (last 7).csv") %>%
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
         received_all_covid19_required_doses,
         get_vaccine_yesno, state, zip_code, start_date, response_date, ideology, race, 
         income, party_id, gender, race_recode, educ4, is_essential_worker, age7, age)

surveym227w <- read.csv("2022/CovidNearYou Weight 27 Feb 09PM (last 7).csv") %>%
  select(response_id, weight_daily_national_13plus, weight_state_weekly, weight_state_monthly)

surveym227 <- surveym227 %>% 
  left_join(surveym227w, by = "response_id")

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
  rbind(surveym227) %>% 
  # make response date into a date 
  # MEC - ben used start date instead of response date- why? 
  mutate(response_date = as.Date(response_date)) %>%
  # select the first 8 weeks of 2022 (Jan 1 to Feb 26th)
  filter(response_date >= as.Date("2022-01-01")) %>%
  filter(response_date <= as.Date("2022-02-26"))

# drop no weights and drop under 18 -- all adult responses in first 8 weeks 
twenty22 <- twenty22_total %>% 
  # drop those with no weights 
  drop_na(weight_daily_national_13plus) %>%
  # add dividions by week 
  mutate(week = cut.Date(response_date, breaks = "1 week", labels = FALSE)) %>% 
  # code into vaccinated and unvaccinated 
  mutate(get_vaccine_yesno = replace(get_vaccine_yesno, get_vaccine_yesno == 1, "Vaccinated")) %>%
  mutate(get_vaccine_yesno = replace(get_vaccine_yesno, get_vaccine_yesno == 2, "Unvaccinated")) %>% 
  select(-start_date) %>% 
  # filter for only adults (age 18 and older)
  filter(age >= 18) %>% 
  arrange(response_date) %>% 
  group_by(response_date) %>% 
  mutate(weight_daily_national_18plus = weight_daily_national_13plus * (n()/sum(weight_daily_national_13plus))) %>% 
  ungroup() 

analysis <- twenty22 %>% 
  # filter for only those not willing or unsure of booster (only ones who were asked the questions)
  filter(covid19_booster_likely_to_get %in% c(2,3)) %>% 
  # filter for vaccinated 
  filter(get_vaccine_yesno == "Vaccinated") %>% 
  # filter for only fully vaccinated (not 1 dose ppl)
  filter(received_all_covid19_required_doses == 1)

# of people who are not willing or unsure of booster:
num_notwilling <- analysis %>% filter(covid19_booster_likely_to_get == 2) %>% nrow()
  # 3771 are not willing 
num_unsure <- analysis %>% filter(covid19_booster_likely_to_get == 3) %>% nrow()
  # 6318 are unsure - big category!

excluded <- nrow(twenty22) - nrow(analysis)


##-----------------FACTORING---------------

# factor twenty22 -- and then filter later  
analysis_factored <- twenty22

analysis_factored$party<- factor(twenty22$party_id, levels = c(1,2,3,4), labels = c("Republican", "Democrat", 
                                                                                    "Independent", "Did Not Respond - Party"))

analysis_factored$which_vax<- factor(twenty22$which_covid19_vaccine, levels = c(1,2,3), labels = c("Moderna", "Pfizer", "Johnson & Johnson"))


analysis_factored$edu<- factor(twenty22$educ4, levels = c(1,2,3,4,5), labels = c("High School or Less",
                                                              "Some College",
                                                              "College or More", 
                                                              "Post Graduate Degree","Did Not Respond - Education"))

analysis_factored$race_recode <- ifelse(twenty22$race_recode==6,2,twenty22$race_recode)

analysis_factored$race_recode<- factor(analysis_factored$race_recode, levels = c(1,2,3,4,5,7,8,9,10), labels = c("White, not Hispanic","Single Other Race","Hispanic or Latino/a",
                                                                              "Black of African American", "Asian",
                                                                              "Native Hawaiian or Other Pacific Islander", "American Indian or Alaska Native", 
                                                                              "Did Not Respond - Race", "Multi-Racial"))

analysis_factored$is_essential_worker <- ifelse(is.na(twenty22$is_essential_worker),3,twenty22$is_essential_worker)

analysis_factored$essential_worker<- factor(analysis_factored$is_essential_worker, levels = c(1,2,3), labels = c("Yes",
                                                                                     "No",
                                                                                     "Did Not Respond"))


analysis_factored$household_income<- factor(twenty22$income, levels = c(1,2,3,4,5,6,7,8), labels = c("Under $15,000",
                                                                                  "Between $15,000 and $29,999",
                                                                                  "Between $30,000 and $49,999",
                                                                                  "Between $50,000 and $74,999",
                                                                                  "Between $75,000 and $99,999",
                                                                                  "Between $100,000 and $150,000",
                                                                                  "Over $150,000","Did Not Respond - Income"))


# only looking at vaccinated people 
analysis_factored$vaccination_status <- ifelse(twenty22$get_vaccine_yesno == "Unvaccinated", "Unvaccinated",
                                               ifelse(twenty22$get_vaccine_yesno == 3, "Did Not Respond - Vax Status",
                                                      ifelse(twenty22$received_all_covid19_required_doses == 2, "Partially Vaccinated",
                                                             ifelse(twenty22$received_all_covid19_required_doses == 1 & twenty22$covid19_booster_likely_to_get == 4, "Fully Vaccinated and Boosted",
                                                                    ifelse(twenty22$received_all_covid19_required_doses == 1,"Fully Vaccinated",
                                                                           ifelse(twenty22$received_all_covid19_required_doses == 3,"Fully Vaccinated","Did Not Respond"))))))

analysis_factored$vaccination_status <- ifelse(is.na(analysis_factored$vaccination_status), "Did Not Respond - Vax Status", analysis_factored$vaccination_status)

analysis_factored$vaccination_status <- factor(analysis_factored$vaccination_status, levels=c("Unvaccinated",
                                                                  "Partially Vaccinated",
                                                                  "Fully Vaccinated",
                                                                  "Fully Vaccinated and Boosted","Did Not Respond - Vax Status"))

analysis_factored$age_group <- cut(analysis_factored$age, breaks = c(17.99,29,39,49,64,74,Inf))

levels(analysis_factored$age_group) <-c("18-29 years",
                          "30-39 years",
                          "40-49 years",
                          "50-64 years",
                          "65-74 years",
                          "75+ years")


analysis_factored$gender <- factor(analysis_factored$gender, levels = c(1,2,3,4), labels = c("Male","Female","Transgender or Nonbinary","No answer"))

analysis_factored$gender <- factor(analysis_factored$gender, levels = c("Female","Male","Transgender or Nonbinary","No answer"))

analysis_factored <- analysis_factored  %>% select(response_id, response_date, week, covid19_booster_likely_to_get, 
                            received_all_covid19_required_doses,
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
                            get_vaccine_yesno, state, zip_code, race_recode, gender, party, which_vax, edu,
                            essential_worker, household_income, vaccination_status, age_group,
                            weight_daily_national_18plus, weight_state_weekly)

##-----------------SURVEY ANALYSIS---------------

# make a survey design object - for all data
analysis_factored_surv_all <- analysis_factored %>% as_survey_design(ids = 1, weights = weight_daily_national_18plus)

analysis_factored_surv <- analysis_factored %>% 
  # filter for only those not willing or unsure of booster (only ones who were asked the questions)
  filter(covid19_booster_likely_to_get %in% c(2,3)) %>% 
  # filter for vaccinated 
  filter(get_vaccine_yesno == "Vaccinated") %>% 
  # filter for only fully vaccinated (not 1 dose ppl)
  filter(received_all_covid19_required_doses == 1) %>% 
  filter(vaccination_status != "Did Not Respond") %>% 
  # make into survey design
  as_survey_design(ids = 1, weights = weight_daily_national_18plus)
  
##-----------------LONGITUDINAL ANALYSIS---------------

# calculate average percents and upper and lower CIs for different reasons
side_ef_date <- analysis_factored_surv %>%
  group_by(response_date) %>%
  dplyr::summarise(side_ef = survey_mean(reason_not_get_covid19_booster_mc_side_effect, vartype = "ci", proportion = TRUE, 
                                         prop_method = "beta",  na.rm = TRUE)) %>% 
  mutate(side_ef_7da = rollmean(side_ef, k = 7, fill = NA)) %>%
  mutate(side_ef_upp_7da = rollmean(side_ef_upp, k = 7, fill = NA)) %>% 
  mutate(side_ef_low_7da = rollmean(side_ef_low, k = 7, fill = NA))

got_covandvax_date <- analysis_factored_surv %>%
  group_by(response_date) %>%
  dplyr::summarise(got_covandvax = survey_mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated,
                                               vartype = "ci", proportion = TRUE, 
                                               prop_method = "beta",  na.rm = TRUE)) %>% 
  mutate(got_covandvax_7da = rollmean(got_covandvax, k = 7, fill = NA)) %>%
  mutate(got_covandvax_upp_7da = rollmean(got_covandvax_upp, k = 7, fill = NA)) %>% 
  mutate(got_covandvax_low_7da = rollmean(got_covandvax_low, k = 7, fill = NA))

too_pol_date <- analysis_factored_surv %>%
  group_by(response_date) %>%
  dplyr::summarise(too_pol = survey_mean(reason_not_get_covid19_booster_mc_too_political,
                                         vartype = "ci", proportion = TRUE, 
                                         prop_method = "beta",  na.rm = TRUE)) %>% 
  mutate(too_pol_7da = rollmean(too_pol, k = 7, fill = NA)) %>%
  mutate(too_pol_upp_7da = rollmean(too_pol_upp, k = 7, fill = NA)) %>% 
  mutate(too_pol_low_7da = rollmean(too_pol_low, k = 7, fill = NA))

distrust_gov_date <- analysis_factored_surv %>%
  group_by(response_date) %>%
  dplyr::summarise(distrust_gov = survey_mean(reason_not_get_covid19_booster_mc_distrust_government,
                                         vartype = "ci", proportion = TRUE, 
                                         prop_method = "beta",  na.rm = TRUE)) %>% 
  mutate(distrust_gov_7da = rollmean(distrust_gov, k = 7, fill = NA)) %>%
  mutate(distrust_gov_upp_7da = rollmean(distrust_gov_upp, k = 7, fill = NA)) %>% 
  mutate(distrust_gov_low_7da = rollmean(distrust_gov_low, k = 7, fill = NA))

distrust_sci_date <- analysis_factored_surv %>%
  group_by(response_date) %>%
  dplyr::summarise(distrust_sci = survey_mean(reason_not_get_covid19_booster_mc_distrust_scientist,
                                              vartype = "ci", proportion = TRUE, 
                                              prop_method = "beta",  na.rm = TRUE)) %>% 
  mutate(distrust_sci_7da = rollmean(distrust_sci, k = 7, fill = NA)) %>%
  mutate(distrust_sci_upp_7da = rollmean(distrust_sci_upp, k = 7, fill = NA)) %>% 
  mutate(distrust_sci_low_7da = rollmean(distrust_sci_low, k = 7, fill = NA))

inconv_date <- analysis_factored_surv %>%
  group_by(response_date) %>%
  dplyr::summarise(inconv = survey_mean(reason_not_get_covid19_booster_mc_inconvenient,
                                              vartype = "ci", proportion = TRUE, 
                                              prop_method = "beta",  na.rm = TRUE)) %>% 
  mutate(inconv_7da = rollmean(inconv, k = 7, fill = NA)) %>%
  mutate(inconv_upp_7da = rollmean(inconv_upp, k = 7, fill = NA)) %>% 
  mutate(inconv_low_7da = rollmean(inconv_low, k = 7, fill = NA))

no_risk_date <- analysis_factored_surv %>%
  group_by(response_date) %>%
  dplyr::summarise(no_risk = survey_mean(reason_not_get_covid19_booster_mc_not_at_risk,
                                              vartype = "ci", proportion = TRUE, 
                                              prop_method = "beta",  na.rm = TRUE)) %>% 
  mutate(no_risk_7da = rollmean(no_risk, k = 7, fill = NA)) %>%
  mutate(no_risk_upp_7da = rollmean(no_risk_upp, k = 7, fill = NA)) %>% 
  mutate(no_risk_low_7da = rollmean(no_risk_low, k = 7, fill = NA))

# setting info for plot 
geom.text.size = 3.2
theme.size = (14/5) * geom.text.size
break.vec <- c(seq(from = as.Date("2022-01-05"), to = as.Date("2022-02-23"),
                   by = "1 week"))

# plot
ggplot() + 
  # side ef
  geom_line(aes(y=side_ef_7da*100,x=response_date, color="Side Effects"), data=side_ef_date) +
  geom_point(aes(y=side_ef_7da*100,x=response_date, color="Side Effects"), data=side_ef_date) + 
  geom_ribbon(aes(ymin=side_ef_low_7da*100,ymax=side_ef_upp_7da*100, x=response_date, fill="Side Effects"),alpha=0.1, data=side_ef_date) +
  # got cov and vax 
  geom_line(aes(y=got_covandvax_7da*100,x=response_date, color="Got COVID and Vax"), data=got_covandvax_date) +
  geom_point(aes(y=got_covandvax_7da*100,x=response_date, color="Got COVID and Vax"), data=got_covandvax_date) + 
  geom_ribbon(aes(ymin=got_covandvax_low_7da*100,ymax=got_covandvax_upp_7da*100, x=response_date, fill="Got COVID and Vax"),alpha=0.1, data=got_covandvax_date) +
  # too political
  geom_line(aes(y=too_pol_7da*100,x=response_date, color="Too Political"), data=too_pol_date) +
  geom_point(aes(y=too_pol_7da*100,x=response_date, color="Too Political"), data=too_pol_date) + 
  geom_ribbon(aes(ymin=too_pol_low_7da*100,ymax=too_pol_upp_7da*100, x=response_date, fill="Too Political"),alpha=0.1, data=too_pol_date) +
  # distrust gov 
  #geom_line(aes(y=distrust_gov_7da*100,x=response_date, color="Distrust Government"), data=distrust_gov_date) +
  #geom_point(aes(y=distrust_gov_7da*100,x=response_date, color="Distrust Government"), data=distrust_gov_date) + 
  #geom_ribbon(aes(ymin=distrust_gov_low_7da*100,ymax=distrust_gov_upp_7da*100, x=response_date, fill="Distrust Government"),alpha=0.1, data=distrust_gov_date) +
  # distrust sci
  #geom_line(aes(y=distrust_sci_7da*100,x=response_date, color="Distrust Scientists"), data=distrust_sci_date) +
  #geom_point(aes(y=distrust_sci_7da*100,x=response_date, color="Distrust Scientists"), data=distrust_sci_date) + 
  #geom_ribbon(aes(ymin=distrust_sci_low_7da*100,ymax=distrust_sci_upp_7da*100, x=response_date, fill="Distrust Scientists"),alpha=0.1, data=distrust_sci_date) +
  # inconv
  geom_line(aes(y=inconv_7da*100,x=response_date, color="Inconvenient"), data=inconv_date) +
  geom_point(aes(y=inconv_7da*100,x=response_date, color="Inconvenient"), data=inconv_date) + 
  geom_ribbon(aes(ymin=inconv_low_7da*100,ymax=inconv_upp_7da*100, x=response_date, fill="Inconvenient"),alpha=0.1, data=inconv_date) +
  # no risk
  geom_line(aes(y=no_risk_7da*100,x=response_date, color="Not at Risk"), data=no_risk_date) +
  geom_point(aes(y=no_risk_7da*100,x=response_date, color="Not at Risk"), data=no_risk_date) + 
  geom_ribbon(aes(ymin=no_risk_low_7da*100,ymax=no_risk_upp_7da*100, x=response_date, fill="Not at Risk"),alpha=0.1, data=no_risk_date) +
  
  theme_classic() +
  scale_y_continuous(limit=c(-2,55),expand = c(0, 0))+
  scale_x_date(date_labels = "%b %d", breaks=break.vec, limits = c(as.Date("2022-01-05"),as.Date("2022-02-23")))+
  labs(x = " ", 
       y= "Percent of Respondents (%)",
       color = "Reason",
       title = "Reasons Vaccinated Respondents Cited for Not Getting Boosted\nin the First 8 Weeks of 2022",
       subtitle = "7-day Rolling Averages with 95% Confidence Intervals") +
  theme(legend.position = "right") +
  theme(legend.text=element_text(size=theme.size), plot.title = element_text(face = "bold")) +
  guides(fill = FALSE)


##---------------PLOTS - NEW OVERALL----------------
# get the overall percents and cis for each response
overall <- analysis_factored_surv %>%
  dplyr::summarise(inconv = survey_mean(reason_not_get_covid19_booster_mc_inconvenient, vartype = "ci", 
                                        proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   someth_else = survey_mean(reason_not_get_covid19_booster_mc_something_else, vartype = "ci", 
                                        proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   too_new = survey_mean(reason_not_get_covid19_booster_mc_too_new, vartype = "ci", 
                                             proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   side_ef = survey_mean(reason_not_get_covid19_booster_mc_side_effect, vartype = "ci", 
                                             proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   threat_exag = survey_mean(reason_not_get_covid19_booster_mc_threat_exaggerated, vartype = "ci", 
                                             proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   dis_gov = survey_mean(reason_not_get_covid19_booster_mc_distrust_government, vartype = "ci", 
                                             proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   dis_sci = survey_mean(reason_not_get_covid19_booster_mc_distrust_scientist, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   too_pol = survey_mean(reason_not_get_covid19_booster_mc_too_political, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   got_covandvax = survey_mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   rxn = survey_mean(reason_not_get_covid19_booster_mc_reaction, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   no_risk = survey_mean(reason_not_get_covid19_booster_mc_not_at_risk, vartype = "ci", 
                                               proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   others = survey_mean(reason_not_get_covid19_booster_mc_others_before_me, vartype = "ci", 
                                               proportion = TRUE, prop_method = "beta",  na.rm = TRUE))

# list of reasons
columns_list <- c("inconvenient", "something else", "too new", "side effects", "threat exaggerated",
             "distrust gov", "distrust science", "too political", "got COVID and vaccine", "reaction",
             "not at risk", "others before me")

# make a dataframe of the reasons and their mean value
values <- cbind(overall$inconv, overall$someth_else, overall$too_new, overall$side_ef, 
                overall$threat_exag, overall$dis_gov, overall$dis_sci, overall$too_pol, 
                overall$got_covandvax, overall$rxn, overall$no_risk, overall$others)

# add row of the nammes to the values df
colnames(values) <- columns_list

# make data vertical and rename columns 
values <- values %>% 
  melt(id = NULL)%>% 
  rename(reason = Var2) %>% 
  select(-Var1)

# make a list of the upper cis and lower cis 
upps <- list(overall$inconv_upp, overall$someth_else_upp, overall$too_new_upp, overall$side_ef_upp, 
             overall$threat_exag_upp, overall$dis_gov_upp, overall$dis_sci_upp, overall$too_pol_upp, 
             overall$got_covandvax_upp, overall$rxn_upp, overall$no_risk_upp, overall$others_upp)

lows <- list(overall$inconv_low, overall$someth_else_low, overall$too_new_low, overall$side_ef_low, 
             overall$threat_exag_low, overall$dis_gov_low, overall$dis_sci_low, overall$too_pol_low, 
             overall$got_covandvax_low, overall$rxn_low, overall$no_risk_low, overall$others_low)


# add upper and lower cis as columns in values df
values <- values %>% 
  mutate(upper_ci = unlist(upps)) %>% 
  mutate(lower_ci = unlist(lows))

# plot - with error bars
values %>% 
  mutate(value = value*100) %>% 
  mutate(upper_ci = upper_ci*100) %>% 
  mutate(lower_ci = lower_ci*100) %>% 
  ggplot(aes(x = reorder(reason,value), y = value)) +
  geom_col() +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.2,
               position=position_dodge(.9)) +
  labs(x = " ",
       y = "Percent of Respondents (%)",
       title = "Reasons Vaccinated Respondents Cited For Not Getting a COVID-19 Booster",
       subtitle = "Momentive Survey Data From the First 8 Weeks of 2022 (Jan 1 to Feb 26)") +
  theme_minimal() +
  ylim(0,40) + 
  coord_flip() + 
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = "none")

##---------------PLOTS - NEW WILLINGNESS----------------
# get the overall percents and cis for each response
likely_get <- analysis_factored_surv %>%
  group_by(covid19_booster_likely_to_get) %>%
  dplyr::summarise(inconv = survey_mean(reason_not_get_covid19_booster_mc_inconvenient, vartype = "ci", 
                                        proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   someth_else = survey_mean(reason_not_get_covid19_booster_mc_something_else, vartype = "ci", 
                                             proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   too_new = survey_mean(reason_not_get_covid19_booster_mc_too_new, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   side_ef = survey_mean(reason_not_get_covid19_booster_mc_side_effect, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   threat_exag = survey_mean(reason_not_get_covid19_booster_mc_threat_exaggerated, vartype = "ci", 
                                             proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   dis_gov = survey_mean(reason_not_get_covid19_booster_mc_distrust_government, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   dis_sci = survey_mean(reason_not_get_covid19_booster_mc_distrust_scientist, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   too_pol = survey_mean(reason_not_get_covid19_booster_mc_too_political, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   got_covandvax = survey_mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, vartype = "ci", 
                                               proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   rxn = survey_mean(reason_not_get_covid19_booster_mc_reaction, vartype = "ci", 
                                     proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   no_risk = survey_mean(reason_not_get_covid19_booster_mc_not_at_risk, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   others = survey_mean(reason_not_get_covid19_booster_mc_others_before_me, vartype = "ci", 
                                        proportion = TRUE, prop_method = "beta",  na.rm = TRUE))

# list of reasons
columns_list <- c("inconvenient", "something else", "too new", "side effects", "threat exaggerated",
                  "distrust gov", "distrust science", "too political", "got COVID and vaccine", "reaction",
                  "not at risk", "others before me")

# make a dataframe of the reasons and their mean value
likely_get_values <- cbind(likely_get$inconv, likely_get$someth_else, likely_get$too_new, likely_get$side_ef, 
                likely_get$threat_exag, likely_get$dis_gov, likely_get$dis_sci, likely_get$too_pol, 
                likely_get$got_covandvax, likely_get$rxn, likely_get$no_risk, likely_get$others)

# add row of the nammes to the values df
colnames(likely_get_values) <- columns_list

# make data vertical and rename columns 
likely_get_values <- as.data.frame(likely_get_values) %>% 
  mutate(covid19_booster_likely_to_get = c(2,3)) %>% 
  melt(id = 'covid19_booster_likely_to_get') %>% 
  rename(reason = variable)

# make a list of the upper cis and lower cis 
lg_upps <- list(likely_get$inconv_upp, likely_get$someth_else_upp, likely_get$too_new_upp, likely_get$side_ef_upp, 
             likely_get$threat_exag_upp, likely_get$dis_gov_upp, likely_get$dis_sci_upp, likely_get$too_pol_upp, 
             likely_get$got_covandvax_upp, likely_get$rxn_upp, likely_get$no_risk_upp, likely_get$others_upp)

lg_lows <- list(likely_get$inconv_low, likely_get$someth_else_low, likely_get$too_new_low, likely_get$side_ef_low, 
             likely_get$threat_exag_low, likely_get$dis_gov_low, likely_get$dis_sci_low, likely_get$too_pol_low, 
             likely_get$got_covandvax_low, likely_get$rxn_low, likely_get$no_risk_low, likely_get$others_low)


# add upper and lower cis as columns in values df
likely_get_values <- likely_get_values %>% 
  mutate(upper_ci = unlist(lg_upps)) %>% 
  mutate(lower_ci = unlist(lg_lows))

# plot - with error bars
likely_get_values %>% 
  mutate(covid19_booster_likely_to_get = replace(covid19_booster_likely_to_get, 
                                                 covid19_booster_likely_to_get == 2, "Not Willing")) %>%
  mutate(covid19_booster_likely_to_get = replace(covid19_booster_likely_to_get, 
                                                 covid19_booster_likely_to_get == 3, "Not Sure")) %>% 
  mutate(value = value*100) %>% 
  mutate(upper_ci = upper_ci*100) %>% 
  mutate(lower_ci = lower_ci*100) %>% 
  ggplot(aes(x = reason, y = value, fill = covid19_booster_likely_to_get)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.2,
                position=position_dodge(.9)) +
  labs(x = " ",
       y = "Percent of Respondents (%)",
       title = "Reasons Vaccinated Respondents Cited For Not Getting a\nCOVID-19 Booster, By Willingness to be Boosted",
       subtitle = "Momentive Survey Data From the First 8 Weeks of 2022 (Jan 1 to Feb 26)",
       fill = "Willingness to be Boosted") +
  scale_fill_manual(values = c("#333333", "#999999")) +
  theme_minimal() +
  ylim(0,50) + 
  coord_flip() + 
  theme(plot.title = element_text(face = "bold"))

##---------------PLOTS - NEW VAX TYPE----------------
# get the overall percents and cis for each response
vax_type <- analysis_factored_surv %>%
  group_by(which_vax) %>%
  dplyr::summarise(inconv = survey_mean(reason_not_get_covid19_booster_mc_inconvenient, vartype = "ci", 
                                        proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   someth_else = survey_mean(reason_not_get_covid19_booster_mc_something_else, vartype = "ci", 
                                             proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   too_new = survey_mean(reason_not_get_covid19_booster_mc_too_new, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   side_ef = survey_mean(reason_not_get_covid19_booster_mc_side_effect, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   threat_exag = survey_mean(reason_not_get_covid19_booster_mc_threat_exaggerated, vartype = "ci", 
                                             proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   dis_gov = survey_mean(reason_not_get_covid19_booster_mc_distrust_government, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   dis_sci = survey_mean(reason_not_get_covid19_booster_mc_distrust_scientist, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   too_pol = survey_mean(reason_not_get_covid19_booster_mc_too_political, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   got_covandvax = survey_mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, vartype = "ci", 
                                               proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   rxn = survey_mean(reason_not_get_covid19_booster_mc_reaction, vartype = "ci", 
                                     proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   no_risk = survey_mean(reason_not_get_covid19_booster_mc_not_at_risk, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   others = survey_mean(reason_not_get_covid19_booster_mc_others_before_me, vartype = "ci", 
                                        proportion = TRUE, prop_method = "beta",  na.rm = TRUE)) %>% 
  drop_na()

# list of reasons
columns_list <- c("inconvenient", "something else", "too new", "side effects", "threat exaggerated",
                  "distrust gov", "distrust science", "too political", "got COVID and vaccine", "reaction",
                  "not at risk", "others before me")

# make a dataframe of the reasons and their mean value
vax_type_values <- cbind(vax_type$inconv, vax_type$someth_else, vax_type$too_new, vax_type$side_ef, 
                           vax_type$threat_exag, vax_type$dis_gov, vax_type$dis_sci, vax_type$too_pol, 
                           vax_type$got_covandvax, vax_type$rxn, vax_type$no_risk, vax_type$others)

# add row of the nammes to the values df
colnames(vax_type_values) <- columns_list

# make data vertical and rename columns 
vax_type_values <- as.data.frame(vax_type_values) %>% 
  mutate(which_vax = c("Moderna", "Pfizer", "Johnson & Johnson")) %>% 
  melt(id = 'which_vax') %>% 
  rename(reason = variable)

# make a list of the upper cis and lower cis 
lg_upps <- list(vax_type$inconv_upp, vax_type$someth_else_upp, vax_type$too_new_upp, vax_type$side_ef_upp, 
                vax_type$threat_exag_upp, vax_type$dis_gov_upp, vax_type$dis_sci_upp, vax_type$too_pol_upp, 
                vax_type$got_covandvax_upp, vax_type$rxn_upp, vax_type$no_risk_upp, vax_type$others_upp)

lg_lows <- list(vax_type$inconv_low, vax_type$someth_else_low, vax_type$too_new_low, vax_type$side_ef_low, 
                vax_type$threat_exag_low, vax_type$dis_gov_low, vax_type$dis_sci_low, vax_type$too_pol_low, 
                vax_type$got_covandvax_low, vax_type$rxn_low, vax_type$no_risk_low, vax_type$others_low)


# add upper and lower cis as columns in values df
vax_type_values <- vax_type_values %>% 
  mutate(upper_ci = unlist(lg_upps)) %>% 
  mutate(lower_ci = unlist(lg_lows))

# plot - with error bars
vax_type_values %>% 
  mutate(value = value*100) %>% 
  mutate(upper_ci = upper_ci*100) %>% 
  mutate(lower_ci = lower_ci*100) %>% 
  ggplot(aes(x = reason, y = value, fill = which_vax)) +
  geom_col(position = position_dodge2(width = 10, padding = 0.1)) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.2,
                position=position_dodge(.9)) +
  labs(x = " ",
       y = "Percent of Respondents (%)",
       title = "Reasons Vaccinated Respondents Cited For Not Getting a\nCOVID-19 Booster, By Vaccine Type",
       subtitle = "Momentive Survey Data From the First 8 Weeks of 2022 (Jan 1 to Feb 26)",
       fill = "Vax Type") +
  theme_minimal() +
  ylim(0,50) + 
  coord_flip() + 
  scale_fill_manual(values = c("#999999","#333333", "#666666")) + 
  theme(plot.title = element_text(face = "bold"))

##---------------PLOTS - NEW POLITICS----------------
# get the overall percents and cis for each response
party <- analysis_factored_surv %>%
  group_by(party) %>%
  dplyr::summarise(inconv = survey_mean(reason_not_get_covid19_booster_mc_inconvenient, vartype = "ci", 
                                        proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   someth_else = survey_mean(reason_not_get_covid19_booster_mc_something_else, vartype = "ci", 
                                             proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   too_new = survey_mean(reason_not_get_covid19_booster_mc_too_new, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   side_ef = survey_mean(reason_not_get_covid19_booster_mc_side_effect, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   threat_exag = survey_mean(reason_not_get_covid19_booster_mc_threat_exaggerated, vartype = "ci", 
                                             proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   dis_gov = survey_mean(reason_not_get_covid19_booster_mc_distrust_government, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   dis_sci = survey_mean(reason_not_get_covid19_booster_mc_distrust_scientist, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   too_pol = survey_mean(reason_not_get_covid19_booster_mc_too_political, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   got_covandvax = survey_mean(reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated, vartype = "ci", 
                                               proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   rxn = survey_mean(reason_not_get_covid19_booster_mc_reaction, vartype = "ci", 
                                     proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   no_risk = survey_mean(reason_not_get_covid19_booster_mc_not_at_risk, vartype = "ci", 
                                         proportion = TRUE, prop_method = "beta",  na.rm = TRUE),
                   others = survey_mean(reason_not_get_covid19_booster_mc_others_before_me, vartype = "ci", 
                                        proportion = TRUE, prop_method = "beta",  na.rm = TRUE)) %>% 
  drop_na()

# list of reasons
columns_list <- c("inconvenient", "something else", "too new", "side effects", "threat exaggerated",
                  "distrust gov", "distrust science", "too political", "got COVID and vaccine", "reaction",
                  "not at risk", "others before me")

# make a dataframe of the reasons and their mean value
party_values <- cbind(party$inconv, party$someth_else, party$too_new, party$side_ef, 
                         party$threat_exag, party$dis_gov, party$dis_sci, party$too_pol, 
                         party$got_covandvax, party$rxn, party$no_risk, party$others)

# add row of the nammes to the values df
colnames(party_values) <- columns_list

# make data vertical and rename columns 
party_values <- as.data.frame(party_values) %>% 
  mutate(party = c("Republican", "Democrat", "Independent")) %>% 
  melt(id = 'party') %>% 
  rename(reason = variable)

# make a list of the upper cis and lower cis 
lg_upps <- list(party$inconv_upp, party$someth_else_upp, party$too_new_upp, party$side_ef_upp, 
                party$threat_exag_upp, party$dis_gov_upp, party$dis_sci_upp, party$too_pol_upp, 
                party$got_covandvax_upp, party$rxn_upp, party$no_risk_upp, party$others_upp)

lg_lows <- list(party$inconv_low, party$someth_else_low, party$too_new_low, party$side_ef_low, 
                party$threat_exag_low, party$dis_gov_low, party$dis_sci_low, party$too_pol_low, 
                party$got_covandvax_low, party$rxn_low, party$no_risk_low, party$others_low)


# add upper and lower cis as columns in values df
party_values <- party_values %>% 
  mutate(upper_ci = unlist(lg_upps)) %>% 
  mutate(lower_ci = unlist(lg_lows))

# plot - with error bars
party_values %>% 
  mutate(value = value*100) %>% 
  mutate(upper_ci = upper_ci*100) %>% 
  mutate(lower_ci = lower_ci*100) %>% 
  ggplot(aes(x = reason, y = value, fill = party)) +
  geom_col(position = position_dodge2(width = 10, padding = 0.1)) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.2,
                position=position_dodge(.9)) +
  labs(x = " ",
       y = "Percent of Respondents (%)",
       title = "Reasons Vaccinated Respondents Cited For Not Getting a\nCOVID-19 Booster, By Political Party",
       subtitle = "Momentive Survey Data From the First 8 Weeks of 2022 (Jan 1 to Feb 26)",
       fill = "Political Party") +
  theme_minimal() +
  ylim(0,50) + 
  coord_flip() + 
  scale_fill_manual(values = c("#3232ff", "#198C19", "#E52C2C")) + 
  theme(plot.title = element_text(face = "bold"))

##---------------TABLE ONE----------------
surv_race_all<-analysis_factored_surv_all %>%
  group_by(race_recode) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  dplyr::select(Characteristic=race_recode,Weighted) %>% 
  add_row(Characteristic ="Race", .before=1)

surv_race<-analysis_factored_surv %>%
  group_by(race_recode) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  dplyr::select(Characteristic=race_recode,Weighted) %>% 
  add_row(Characteristic ="Race", .before=1)

surv_gender_all<-analysis_factored_surv_all %>%
  group_by(gender) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=gender,Weighted) %>% 
  add_row(Characteristic ="Gender", .before=1)

surv_gender<-analysis_factored_surv %>%
  group_by(gender) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=gender,Weighted) %>% 
  add_row(Characteristic ="Gender", .before=1)

surv_edu_all<-analysis_factored_surv_all %>%
  group_by(edu) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=edu,Weighted) %>% 
  add_row(Characteristic ="Education", .before=1)

surv_edu<-analysis_factored_surv %>%
  group_by(edu) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=edu,Weighted) %>% 
  add_row(Characteristic ="Education", .before=1)


surv_age_group_all<-analysis_factored_surv_all %>%
  group_by(age_group) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=age_group,Weighted) %>% 
  add_row(Characteristic ="Age", .before=1)

surv_age_group<-analysis_factored_surv %>%
  group_by(age_group) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=age_group,Weighted) %>% 
  add_row(Characteristic ="Age", .before=1)

surv_household_income_all<-analysis_factored_surv_all %>%
  group_by(household_income) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=household_income,Weighted) %>% 
  add_row(Characteristic ="Household Income", .before=1)

surv_household_income<-analysis_factored_surv %>%
  group_by(household_income) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=household_income,Weighted) %>% 
  add_row(Characteristic ="Household Income", .before=1)

surv_vaccination_status_all<-analysis_factored_surv_all %>%
  group_by(vaccination_status) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=vaccination_status,Weighted) %>% 
  add_row(Characteristic ="COVID-19 Vaccine", .before=1)

surv_vaccination_status<-analysis_factored_surv %>%
  group_by(vaccination_status) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=vaccination_status,Weighted) %>% 
  add_row(Characteristic ="COVID-19 Vaccine", .before=1)

surv_party_all<-analysis_factored_surv_all %>%
  group_by(party) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=party,Weighted) %>% 
  add_row(Characteristic ="Political Party", .before=1)

surv_party<-analysis_factored_surv %>%
  group_by(party) %>%
  dplyr::summarize(proportion = survey_mean(),total = survey_total()) %>%
  mutate(Weighted = paste0(format(round(total,1),nsmall=1)," (",format(round(proportion*100,1),nsmall=1),")")) %>%
  select(Characteristic=party,Weighted) %>% 
  add_row(Characteristic ="Political Party", .before=1)


table_one_col1 <- rbind(surv_race,surv_gender,surv_edu,surv_age_group,surv_household_income,
                   surv_vaccination_status,surv_party)
table_one_col2 <- rbind(surv_race_all,surv_gender_all,surv_edu_all,surv_age_group_all,
                        surv_household_income_all,surv_vaccination_status_all,surv_party_all)
names(table_one_col1) <- c("Characteristic","Vaccinated But Not Boosted Survey Respondents, N (%)")
names(table_one_col2) <- c("Characteristic", "All Survey Respondents, N (%)")

table_one <- table_one_col2 %>% left_join(table_one_col1, by = "Characteristic")

##---------------PLOTS - OLD----------------
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
  ggplot(aes(x = reorder(variable, value), y = value, fill = variable)) +
  geom_col() +
  labs(x = " ",
       y = "Percent of Respondents",
       title = "Reasons Vaccinated Respondents Cited For Not Getting a COVID-19 Booster",
       subtitle = "Momentive Survey Data From the First Six Weeks of 2022 (Jan 1 to Feb 12)") +
  theme_minimal() +
  coord_flip() + 
  theme(plot.title = element_text(face = "bold")) +
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

##-----------------WEIGHTED PERCENTS - OLD---------------

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



