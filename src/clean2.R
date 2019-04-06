library(tidyverse)
library(lubridate)


clean_survey_sep_days <- read_csv('data/Arrival Time_April 4, 2019_08.20.csv') %>%
  select(contains('Q')) %>%
  slice(-c(1:2)) %>%
  rename('distance_km' = 'Q1',
         'mw_arrival' = 'Q3',
         'tt_arrival' = 'Q4',
         'mode_of_transport' = 'Q5') %>% 
  mutate(mw_arrival = hm(mw_arrival),
         mw_arrival = hour(mw_arrival)*60 + minute(mw_arrival),
         mw_arrival = if_else(mw_arrival < 400, 540 + mw_arrival, mw_arrival),
         mw_arrival = 9*60 - mw_arrival) %>%
  mutate(tt_arrival = hm(tt_arrival),
         tt_arrival = hour(tt_arrival)*60 + minute(tt_arrival),
         tt_arrival = if_else(tt_arrival < 430, 570 + tt_arrival, tt_arrival),
         tt_arrival = 9.5*60 - tt_arrival) %>%
  mutate(mode_of_transport = fct_recode(mode_of_transport, 
                                        driving = '1',
                                        transit = '2',
                                        walking = '3',
                                        cycling = '4'))

clean_survey_sep_days
clean_survey_sep_days$distance_km <- as.numeric(clean_survey_sep_days$distance_km)


View(clean_survey_sep_days)

levels(clean_survey_sep_days$mode_of_transport)


clean_survey_mw <- clean_survey_sep_days %>% 
  select(-c(tt_arrival)) %>% 
  rename(arrival = mw_arrival)

clean_survey_tt <- clean_survey_sep_days %>% 
  select(-c(mw_arrival)) %>% 
  rename(arrival = tt_arrival)

clean_survey_all_days <- bind_rows(clean_survey_mw, clean_survey_tt)


# write_csv(clean_survey, 'data/clean_survey_responses.csv')
