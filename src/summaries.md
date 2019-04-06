Untitled
================

``` r
summary(clean_survey_all_days)
```

    ##   distance_km        arrival         mode_of_transport
    ##  Min.   : 0.700   Min.   :-90.0000   Cycling: 8       
    ##  1st Qu.: 1.350   1st Qu.: -7.0000   Driving:14       
    ##  Median : 6.850   Median :  0.0000   Transit:58       
    ##  Mean   : 7.995   Mean   :  0.7946   Walking:32       
    ##  3rd Qu.:10.400   3rd Qu.:  5.0000                    
    ##  Max.   :44.000   Max.   :120.0000

``` r
# summary(clean_survey_all_days) %>% knitr::kable()
```

``` r
# clean_survey_all_days %>% 
#   select(.data$mode_of_transport, .data$distance_km, .data$arrival) %>% 
#   summary_table(.)
```

``` r
clean_survey_all_days %>%
  group_by(.data$mode_of_transport) %>%
  summary_table(.)
```

    ## 
    ## 
    ## |                          |mode_of_transport: Cycling (N = 8) |mode_of_transport: Driving (N = 14) |mode_of_transport: Transit (N = 58) |mode_of_transport: Walking (N = 32) |
    ## |:-------------------------|:----------------------------------|:-----------------------------------|:-----------------------------------|:-----------------------------------|
    ## |**distance_km**           |&nbsp;&nbsp;                       |&nbsp;&nbsp;                        |&nbsp;&nbsp;                        |&nbsp;&nbsp;                        |
    ## |&nbsp;&nbsp; minimum      |1.50                               |9.10                                |2.00                                |0.70                                |
    ## |&nbsp;&nbsp; median (IQR) |5.05 (3.08, 7.97)                  |15.00 (12.75, 18.75)                |8.10 (6.60, 10.10)                  |1.00 (0.97, 1.02)                   |
    ## |&nbsp;&nbsp; mean (sd)    |6.00 &plusmn; 4.38                 |16.13 &plusmn; 5.53                 |10.15 &plusmn; 7.54                 |1.03 &plusmn; 0.22                  |
    ## |&nbsp;&nbsp; maximum      |12.40                              |26.80                               |44.00                               |1.60                                |
    ## |**arrival**               |&nbsp;&nbsp;                       |&nbsp;&nbsp;                        |&nbsp;&nbsp;                        |&nbsp;&nbsp;                        |
    ## |&nbsp;&nbsp; minimum      |-7.00                              |-90.00                              |-85.00                              |-60.00                              |
    ## |&nbsp;&nbsp; median (IQR) |-2.50 (-5.00, 0.50)                |15.00 (0.00, 48.75)                 |0.00 (-7.00, 5.00)                  |-3.00 (-10.00, 3.00)                |
    ## |&nbsp;&nbsp; mean (sd)    |-2.25 &plusmn; 3.37                |17.50 &plusmn; 60.47                |0.33 &plusmn; 23.97                 |-4.91 &plusmn; 15.15                |
    ## |&nbsp;&nbsp; maximum      |2.00                               |120.00                              |75.00                               |25.00                               |
    ## |**mode_of_transport**     |&nbsp;&nbsp;                       |&nbsp;&nbsp;                        |&nbsp;&nbsp;                        |&nbsp;&nbsp;                        |
    ## |&nbsp;&nbsp; Cycling      |8 (100)                            |0 (0)                               |0 (0)                               |0 (0)                               |
    ## |&nbsp;&nbsp; Driving      |0 (0)                              |14 (100)                            |0 (0)                               |0 (0)                               |
    ## |&nbsp;&nbsp; Transit      |0 (0)                              |0 (0)                               |58 (100)                            |0 (0)                               |
    ## |&nbsp;&nbsp; Walking      |0 (0)                              |0 (0)                               |0 (0)                               |32 (100)                            |

``` r
# clean_survey_all_days %>% 
#   group_by(.data$mode_of_transport) %>% 
#   summary_table(.) %>% 
#   knitr::kable()
```
