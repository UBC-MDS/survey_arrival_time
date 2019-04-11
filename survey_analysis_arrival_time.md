Survey Analysis Arrival Time
================
Akansha Vashisth, Ian Flores Siaca, Rachel K. Riggs, Milos Milic
2019-04-13

## Libraries

``` r
library(tidyverse)
library(tidybayes)
library(brms)
library(broom)
library(knitr)
library(gridExtra)
```

## Load the data

``` r
clean_survey_all_days <- read_csv('https://raw.githubusercontent.com/UBC-MDS/survey_arrival_time/master/data/clean_survey_responses_all_days.csv')
clean_survey_sep_days <- read_csv('https://raw.githubusercontent.com/UBC-MDS/survey_arrival_time/master/data/clean_survey_responses_sep_days.csv')
```

# Survey question

**How does distance from campus influence arrival time to lectures?**

We conducted an observational study to explore if there is a
relationship between the distance lived from class and arrival time. We
also wanted to test a potential confounder for this relationship which
is the mode of transportation a student takes to class.

# Methods

## Survey study design

We asked our survey respondents to answer the following questions:

  - How far from Hugh Dempster do you live in kilometers via the mode of
    transport you use(a google maps link was provided to help with the
    distance estimation)?

  - What time do you typically arrive at Hugh Dempster on Mondays and
    Wednesdays? please enter in the format hh:mm

  - What time do you typically arrive at Hugh Dempster on Mondays and
    Wednesdays? please enter in the format hh:mm

  - What is your typical mode of transit? (drive, public transit, walk,
    or bike)

## Data collection methods

Data was collected by this
[survey](https://ubc.ca1.qualtrics.com/jfe/form/SV_eo1whP0fPfWPCw5)
hosted by Qualtrics. The survey had 56 participants from the MDS
students 2018-2019 cohort and responses were anonymized.

## Analysis methods

We performed initial [exploratory data
analysis](https://github.com/UBC-MDS/survey_arrival_time/blob/v2.0/milestone2.md)
on our data.

To analyze the data, we consider 3 groups: - all days grouped together -
Mondays and Wednesdays together - Tuesdays and Thursdays together

Below we fit a linear regression using the distance as the predictor
variable and the arrival time as the response variable. We compare this
linear regression model with a null model through an ANOVA test. To
validate the estimates of the frequentist approach, given the
possibility of a small sample size, we use a Bayesian linear regression.
After this, we will move on to using the mode of transportation as a
confounder variable and fit a linear regression model with these
variables. We will compare this model with the null model through an
ANOVA test, and again validate using a Bayesian linear regression.

# EDA

``` r
### Exploratory Data Analysis
plot1 <- clean_survey_all_days %>%
  ggplot(height = 17 , width = 2) +
  geom_histogram(aes(x=distance_km)) + 
  theme(axis.title=element_text(size=10),
        plot.title = element_text(size = 10, face = "bold")) +  
  labs(y= "Frequency", x = "Distance (in km)", title = "Number of MDS Students v/s their distance of travel") 

plot2 <- clean_survey_all_days %>%
  ggplot() +
  geom_histogram(aes(x=arrival)) +  
  theme(axis.title=element_text(size=10),
        plot.title = element_text(size = 10, face = "bold")) +  
  labs(y= "Frequency", x = "Arrival time", title = "Number of MDS Students v/s their arrival time") 

plot3 <- clean_survey_sep_days %>%
  ggplot(size = 10) +
  geom_bar(aes(x=mode_of_transport)) +
  theme(axis.title=element_text(size=10),
        plot.title = element_text(size = 10, face = "bold")) +  
  labs(y= "Frequency", x = "Mode of transport", title = "Number of MDS Students using different modes of transport") 

grid.arrange(plot1, plot2, plot3, ncol=3)
```

![](survey_analysis_arrival_time_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# Analysis and results

## Without Confounders

### Distance and Arrival Time (Overall)

``` r
# Frequentist
fit_all <- lm(arrival ~ distance_km, data = clean_survey_all_days)

tidy(fit_all) %>% 
    kable()
```

| term         |    estimate | std.error |  statistic |   p.value |
| :----------- | ----------: | --------: | ---------: | --------: |
| (Intercept)  |   6.7567108 | 3.8362921 |   1.761261 | 0.0809729 |
| distance\_km | \-0.9445517 | 0.3466962 | \-2.724436 | 0.0074945 |

``` r
fit_all_bayes <- brm(arrival ~ distance_km, data = clean_survey_all_days, iter = 5000, cores = -1)
```

    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 5.5e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.55 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.117098 seconds (Warm-up)
    ## Chain 1:                0.150493 seconds (Sampling)
    ## Chain 1:                0.267591 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 1.3e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.117942 seconds (Warm-up)
    ## Chain 2:                0.463689 seconds (Sampling)
    ## Chain 2:                0.581631 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 1.2e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.12 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.125671 seconds (Warm-up)
    ## Chain 3:                0.214929 seconds (Sampling)
    ## Chain 3:                0.3406 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 1.1e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.11 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.112736 seconds (Warm-up)
    ## Chain 4:                0.266914 seconds (Sampling)
    ## Chain 4:                0.37965 seconds (Total)
    ## Chain 4:

``` r
# Returns the 95% estimates
fit_all_bayes %>%
    gather_draws(b_Intercept, b_distance_km, sigma) %>%
    median_qi() %>%
    kable()
```

| .variable       |      .value |      .lower |     .upper | .width | .point | .interval |
| :-------------- | ----------: | ----------: | ---------: | -----: | :----- | :-------- |
| b\_distance\_km | \-0.9498449 | \-1.6309889 | \-0.269201 |   0.95 | median | qi        |
| b\_Intercept    |   6.8294891 | \-0.5314202 |  14.263183 |   0.95 | median | qi        |
| sigma           |  27.9048569 |  24.6425481 |  31.975787 |   0.95 | median | qi        |

``` r
fit_all_bayes %>%
    gather_draws(b_Intercept, b_distance_km) %>%
    ggplot(aes(.value)) +
    geom_density() +
    facet_wrap(~ .variable, scales = 'free') +
    labs(x = 'Value',
         title = 'Bayesian All Days Model')
```

![](survey_analysis_arrival_time_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Distance and Arrival Time (Monday & Wednesday)

``` r
fit_mw <- lm(mw_arrival ~ distance_km, data = clean_survey_sep_days)

tidy(fit_mw) %>% 
    kable()
```

| term         |    estimate | std.error |  statistic |   p.value |
| :----------- | ----------: | --------: | ---------: | --------: |
| (Intercept)  |   6.8203611 | 4.9133152 |   1.388138 | 0.1707956 |
| distance\_km | \-0.8732192 | 0.4440297 | \-1.966578 | 0.0543802 |

``` r
fit_mw_bayes <- brm(mw_arrival ~ distance_km, data = clean_survey_sep_days, iter = 5000, cores = -1)
```

    ## 
    ## SAMPLING FOR MODEL '8383dc356debc299916ea59c2351681d' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 3.4e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.34 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.113204 seconds (Warm-up)
    ## Chain 1:                0.141289 seconds (Sampling)
    ## Chain 1:                0.254493 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '8383dc356debc299916ea59c2351681d' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 1e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.101072 seconds (Warm-up)
    ## Chain 2:                0.09333 seconds (Sampling)
    ## Chain 2:                0.194402 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '8383dc356debc299916ea59c2351681d' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 1e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.112609 seconds (Warm-up)
    ## Chain 3:                0.088354 seconds (Sampling)
    ## Chain 3:                0.200963 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '8383dc356debc299916ea59c2351681d' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 1e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.104681 seconds (Warm-up)
    ## Chain 4:                0.088624 seconds (Sampling)
    ## Chain 4:                0.193305 seconds (Total)
    ## Chain 4:

``` r
fit_mw_bayes %>%
    gather_draws(b_Intercept, b_distance_km, sigma) %>%
    median_qi() %>%
    kable()
```

| .variable       |      .value |     .lower |     .upper | .width | .point | .interval |
| :-------------- | ----------: | ---------: | ---------: | -----: | :----- | :-------- |
| b\_distance\_km | \-0.8705153 | \-1.747040 |  0.0150409 |   0.95 | median | qi        |
| b\_Intercept    |   7.0456612 | \-2.296062 | 16.5065617 |   0.95 | median | qi        |
| sigma           |  25.1560300 |  21.179783 | 30.6182504 |   0.95 | median | qi        |

``` r
fit_mw_bayes %>%
    gather_draws(b_Intercept, b_distance_km) %>%
    ggplot(aes(.value)) +
    geom_density() +
    facet_wrap(~ .variable, scales = 'free') +
    labs(x = 'Value',
         title = 'Bayesian Monday & Wednesday Model')
```

![](survey_analysis_arrival_time_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Distance and Arrival Time (Tuesday and Thursdays)

``` r
fit_tt <- lm(tt_arrival ~ distance_km, data = clean_survey_sep_days)

tidy(fit_tt) %>% 
    kable()
```

| term         |   estimate | std.error |  statistic |   p.value |
| :----------- | ---------: | --------: | ---------: | --------: |
| (Intercept)  |   6.693060 | 5.9802866 |   1.119187 | 0.2680140 |
| distance\_km | \-1.015884 | 0.5404548 | \-1.879684 | 0.0655509 |

``` r
fit_tt_bayes <- brm(tt_arrival ~ distance_km, data = clean_survey_sep_days, iter = 5000, cores = -1)
```

    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 3.3e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.33 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.10027 seconds (Warm-up)
    ## Chain 1:                0.096926 seconds (Sampling)
    ## Chain 1:                0.197196 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 1e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.096889 seconds (Warm-up)
    ## Chain 2:                0.250453 seconds (Sampling)
    ## Chain 2:                0.347342 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 1.1e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.11 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.112657 seconds (Warm-up)
    ## Chain 3:                0.097533 seconds (Sampling)
    ## Chain 3:                0.21019 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 9e-06 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.101043 seconds (Warm-up)
    ## Chain 4:                0.093316 seconds (Sampling)
    ## Chain 4:                0.194359 seconds (Total)
    ## Chain 4:

``` r
fit_tt_bayes %>%
    gather_draws(b_Intercept, b_distance_km, sigma) %>%
    median_qi() %>%
    kable()
```

| .variable       |     .value |     .lower |     .upper | .width | .point | .interval |
| :-------------- | ---------: | ---------: | ---------: | -----: | :----- | :-------- |
| b\_distance\_km | \-1.016316 | \-2.069501 |  0.0438733 |   0.95 | median | qi        |
| b\_Intercept    |   6.943948 | \-3.977560 | 18.2564401 |   0.95 | median | qi        |
| sigma           |  30.548283 |  25.643995 | 37.2849633 |   0.95 | median | qi        |

``` r
fit_tt_bayes %>%
    gather_draws(b_Intercept, b_distance_km) %>%
    ggplot(aes(.value)) +
    geom_density() +
    facet_wrap(~ .variable, scales = 'free') +
    labs(x = 'Value',
         title = 'Bayesian Tuesday & Thursday Model')
```

![](survey_analysis_arrival_time_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## With Confounders

### Distance and Arrival Time (Overall)

``` r
fit_all_transp <- lm(arrival ~ distance_km + mode_of_transport, data = clean_survey_all_days)

tidy(fit_all_transp) %>% 
    kable()
```

| term                       |     estimate |  std.error |   statistic |   p.value |
| :------------------------- | -----------: | ---------: | ----------: | --------: |
| (Intercept)                |    7.0621190 | 10.3411601 |   0.6829136 | 0.4961374 |
| distance\_km               |  \-0.8020198 |  0.4588034 | \-1.7480687 | 0.0833205 |
| mode\_of\_transportDriving | \-11.6266849 | 13.3317079 | \-0.8721077 | 0.3851024 |
| mode\_of\_transportTransit |    0.7494133 | 10.8022660 |   0.0693756 | 0.9448202 |
| mode\_of\_transportWalking |  \-1.3287860 | 11.3753639 | \-0.1168126 | 0.9072275 |

``` r
fit_all_bayes_transp <- brm(arrival ~ distance_km + mode_of_transport, data = clean_survey_all_days, iter = 5000, cores = -1)
```

    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 3.6e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.36 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.504885 seconds (Warm-up)
    ## Chain 1:                0.271601 seconds (Sampling)
    ## Chain 1:                0.776486 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 1.3e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.473089 seconds (Warm-up)
    ## Chain 2:                0.256448 seconds (Sampling)
    ## Chain 2:                0.729537 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 1.2e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.12 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.452204 seconds (Warm-up)
    ## Chain 3:                0.246807 seconds (Sampling)
    ## Chain 3:                0.699011 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 1.4e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.14 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.438234 seconds (Warm-up)
    ## Chain 4:                0.266682 seconds (Sampling)
    ## Chain 4:                0.704916 seconds (Total)
    ## Chain 4:

``` r
# Returns the 95% estimates
fit_all_bayes_transp %>%
    gather_draws(b_Intercept, b_distance_km, sigma) %>%
    median_qi() %>%
    kable()
```

| .variable       |      .value |      .lower |     .upper | .width | .point | .interval |
| :-------------- | ----------: | ----------: | ---------: | -----: | :----- | :-------- |
| b\_distance\_km | \-0.8106711 |  \-1.718153 |  0.0963934 |   0.95 | median | qi        |
| b\_Intercept    |   6.9902241 | \-13.732438 | 27.4157229 |   0.95 | median | qi        |
| sigma           |  28.0287801 |   24.722315 | 32.1747158 |   0.95 | median | qi        |

``` r
fit_all_bayes_transp %>%
    gather_draws(b_Intercept, b_distance_km) %>%
    ggplot(aes(.value)) +
    geom_density() +
    facet_wrap(~ .variable, scales = 'free') +
    labs(x = 'Value',
         title = 'Bayesian All Days Model w Confounder')
```

![](survey_analysis_arrival_time_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### Distance and Arrival Time (Monday & Wednesday)

``` r
fit_mw_transp <- lm(mw_arrival ~ distance_km + mode_of_transport, data = clean_survey_sep_days)

tidy(fit_mw_transp) %>% 
    kable()
```

| term                       |    estimate |  std.error |   statistic |   p.value |
| :------------------------- | ----------: | ---------: | ----------: | --------: |
| (Intercept)                |   7.3754923 | 13.5262569 |   0.5452722 | 0.5879432 |
| distance\_km               | \-0.6875821 |  0.6001157 | \-1.1457492 | 0.2572468 |
| mode\_of\_transportDriving | \-7.7143475 | 17.4378990 | \-0.4423897 | 0.6600777 |
| mode\_of\_transportTransit | \-2.2253062 | 14.1293843 | \-0.1574949 | 0.8754767 |
| mode\_of\_transportWalking |   0.2710767 | 14.8789974 |   0.0182187 | 0.9855354 |

``` r
fit_mw_bayes_transp <- brm(mw_arrival ~ distance_km + mode_of_transport, data = clean_survey_sep_days, iter = 5000, cores = -1)
```

    ## 
    ## SAMPLING FOR MODEL '8383dc356debc299916ea59c2351681d' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 3.3e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.33 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.37259 seconds (Warm-up)
    ## Chain 1:                0.20827 seconds (Sampling)
    ## Chain 1:                0.58086 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '8383dc356debc299916ea59c2351681d' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 2.1e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.21 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.32559 seconds (Warm-up)
    ## Chain 2:                0.184298 seconds (Sampling)
    ## Chain 2:                0.509888 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '8383dc356debc299916ea59c2351681d' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 1e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.360704 seconds (Warm-up)
    ## Chain 3:                0.193971 seconds (Sampling)
    ## Chain 3:                0.554675 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '8383dc356debc299916ea59c2351681d' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 1.1e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.11 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.341822 seconds (Warm-up)
    ## Chain 4:                0.189786 seconds (Sampling)
    ## Chain 4:                0.531608 seconds (Total)
    ## Chain 4:

``` r
fit_mw_bayes_transp %>%
    gather_draws(b_Intercept, b_distance_km, sigma) %>%
    median_qi() %>%
    kable()
```

| .variable       |      .value |      .lower |     .upper | .width | .point | .interval |
| :-------------- | ----------: | ----------: | ---------: | -----: | :----- | :-------- |
| b\_distance\_km | \-0.6893717 |  \-1.895912 |  0.4881487 |   0.95 | median | qi        |
| b\_Intercept    |   7.7905653 | \-18.766596 | 34.1716285 |   0.95 | median | qi        |
| sigma           |  25.7713736 |   21.551884 | 31.5258271 |   0.95 | median | qi        |

``` r
fit_mw_bayes_transp %>%
    gather_draws(b_Intercept, b_distance_km) %>%
    ggplot(aes(.value)) +
    geom_density() +
    facet_wrap(~ .variable, scales = 'free') +
    labs(x = 'Value',
         title = 'Bayesian Monday & Wednesday Model w Confounder')
```

![](survey_analysis_arrival_time_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### Distance and Arrival Time (Tuesday and Thursdays)

``` r
fit_tt_transp <- lm(tt_arrival ~ distance_km + mode_of_transport, data = clean_survey_sep_days)

tidy(fit_tt_transp) %>% 
    kable()
```

| term                       |     estimate |  std.error |   statistic |   p.value |
| :------------------------- | -----------: | ---------: | ----------: | --------: |
| (Intercept)                |    6.7487456 | 16.1665213 |   0.4174519 | 0.6781001 |
| distance\_km               |  \-0.9164576 |  0.7172556 | \-1.2777281 | 0.2071309 |
| mode\_of\_transportDriving | \-15.5390222 | 20.8416984 | \-0.7455737 | 0.4593476 |
| mode\_of\_transportTransit |    3.7241328 | 16.8873765 |   0.2205276 | 0.8263407 |
| mode\_of\_transportWalking |  \-2.9286487 | 17.7833107 | \-0.1646852 | 0.8698427 |

``` r
fit_tt_bayes_transp <- brm(tt_arrival ~ distance_km + mode_of_transport, data = clean_survey_sep_days, iter = 5000, cores = -1)
```

    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 3.8e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.38 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.356949 seconds (Warm-up)
    ## Chain 1:                0.190586 seconds (Sampling)
    ## Chain 1:                0.547535 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 1e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.380756 seconds (Warm-up)
    ## Chain 2:                0.198286 seconds (Sampling)
    ## Chain 2:                0.579042 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 1.1e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.11 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.345077 seconds (Warm-up)
    ## Chain 3:                0.201534 seconds (Sampling)
    ## Chain 3:                0.546611 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '39b50f67d47eaaf0a7b8a729349e5ded' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 1e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.371126 seconds (Warm-up)
    ## Chain 4:                0.202915 seconds (Sampling)
    ## Chain 4:                0.574041 seconds (Total)
    ## Chain 4:

``` r
fit_tt_bayes_transp %>%
    gather_draws(b_Intercept, b_distance_km, sigma) %>%
    median_qi() %>%
    kable()
```

| .variable       |      .value |      .lower |     .upper | .width | .point | .interval |
| :-------------- | ----------: | ----------: | ---------: | -----: | :----- | :-------- |
| b\_distance\_km | \-0.9039387 |  \-2.302891 |  0.4958573 |   0.95 | median | qi        |
| b\_Intercept    |   6.4467648 | \-25.227833 | 37.7701462 |   0.95 | median | qi        |
| sigma           |  30.7463665 |   25.725151 | 37.6172060 |   0.95 | median | qi        |

``` r
fit_tt_bayes_transp %>%
    gather_draws(b_Intercept, b_distance_km) %>%
    ggplot(aes(.value)) +
    geom_density() +
    facet_wrap(~ .variable, scales = 'free') +
    labs(x = 'Value',
         title = 'Bayesian Tuesday & Thursday Model w Confounder')
```

![](survey_analysis_arrival_time_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

# Discussion of the results

# Discussion of the survey/study design