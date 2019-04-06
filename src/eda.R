library(tidyverse)

survey_data <- read_csv("data/clean_survey_responses.csv")
# survey_data <- as.data.frame(survey_data)
head(survey_data)

survey_data$mode_of_transport <- as.factor(survey_data$mode_of_transport)
# levels(survey_data$mode_of_transport)

# typeof(survey_data$distance_km)
# typeof(survey_data$mw_arrival)

clean_survey_sep_days
typeof(clean_survey_sep_days$distance_km)
clean_survey_sep_days$distance_km <- as.numeric(clean_survey_sep_days$distance_km)


clean_survey_all_days
typeof(clean_survey_all_days$distance_km)



# typeof(clean_survey2$mw_arrival)

dim(survey_data)
dim(clean_survey_sep_days)
dim(clean_survey_all_days)


#############

### TABLE OF SUMMARY STATISTICS

summary(clean_survey_all_days) %>% 
  knitr::kable()

summary(clean_survey_sep_days) %>% 
  knitr::kable()


# by mode of transit
by(clean_survey_all_days, clean_survey_all_days$mode_of_transport, summary)


#############

### INITIAL HISTOGRAMS FOR MW & TT

clean_survey_sep_days %>% 
  ggplot(aes(x = mw_arrival)) +
  geom_histogram()

clean_survey_sep_days %>% 
  ggplot(aes(x = tt_arrival)) +
  geom_histogram()


### INITIAL SCATTERPLOTS FOR MW

clean_survey_sep_days %>% 
  ggplot(aes(x = mw_arrival, y = distance_km)) +
  geom_point()

### FACET ON MODE OF TRANSPORT
clean_survey_sep_days %>% 
  ggplot(aes(x = mw_arrival, y = distance_km)) +
  geom_point() +
  facet_wrap(~ mode_of_transport)

clean_survey_sep_days %>% 
  ggplot(aes(x = mw_arrival, y = distance_km)) +
  geom_point() +
  facet_wrap(~ mode_of_transport) +
  geom_smooth(method = "lm", se = F)

### COLOR INSTEAD OF FACET
clean_survey_sep_days %>% 
  ggplot(aes(x = mw_arrival, y = distance_km)) +
  geom_point(aes(color = mode_of_transport))


### SCATTERPLOT FOR ALL DAYS, AND FACETED

clean_survey_all_days %>% 
  ggplot(aes(x = arrival, y = distance_km)) +
  geom_point()

clean_survey_all_days %>% 
  ggplot(aes(x = arrival, y = distance_km)) +
  geom_point(aes(color = mode_of_transport), alpha = .5) +
  facet_wrap(~ mode_of_transport)

clean_survey_all_days %>% 
  ggplot(aes(x = arrival, y = distance_km)) +
  geom_point() +
  facet_wrap(~ mode_of_transport)



### HISTOGRAMS FOR ALL DAYS, FACETED ON MODE OF TRANSPORT   ### maybe keep this one or the next?

clean_survey_all_days %>% 
  ggplot(aes(x = arrival)) +
  geom_histogram() + 
  facet_wrap(~ mode_of_transport)

### DENSITY *****

clean_survey_all_days %>% 
  ggplot(aes(x = arrival)) +
  geom_density() + 
  facet_wrap(~ mode_of_transport)



### BOXPLOT & VIOLIN PLOT FOR ALL DAYS

clean_survey_all_days %>% 
  ggplot(aes(x = mode_of_transport, y = arrival)) +
  geom_boxplot()


clean_survey_all_days %>% 
  ggplot(aes(x = mode_of_transport, y = arrival)) +
  geom_violin() +
  geom_jitter(width = .2, alpha = .3)


# clean_survey_all_days %>% 
#   ggplot(aes(x = mode_of_transport, y = arrival)) +
#   geom_point(alpha = .5)

