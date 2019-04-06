Summaries
================

``` r
summary(clean_survey_sep_days)
```

    ##   distance_km       mw_arrival         tt_arrival      mode_of_transport
    ##  Min.   : 0.700   Min.   :-90.0000   Min.   :-90.000   Cycling: 4       
    ##  1st Qu.: 1.350   1st Qu.:-10.0000   1st Qu.: -5.000   Driving: 7       
    ##  Median : 6.850   Median : -1.5000   Median :  0.000   Transit:29       
    ##  Mean   : 7.995   Mean   :  0.1607   Mean   :  1.429   Walking:16       
    ##  3rd Qu.:10.400   3rd Qu.:  5.0000   3rd Qu.:  5.000                    
    ##  Max.   :44.000   Max.   : 90.0000   Max.   :120.000

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
# by mode of transit
by(clean_survey_all_days, clean_survey_all_days$mode_of_transport, summary)
```

    ## clean_survey_all_days$mode_of_transport: Cycling
    ##   distance_km        arrival      mode_of_transport
    ##  Min.   : 1.500   Min.   :-7.00   Cycling:8        
    ##  1st Qu.: 3.075   1st Qu.:-5.00   Driving:0        
    ##  Median : 5.050   Median :-2.50   Transit:0        
    ##  Mean   : 6.000   Mean   :-2.25   Walking:0        
    ##  3rd Qu.: 7.975   3rd Qu.: 0.50                    
    ##  Max.   :12.400   Max.   : 2.00                    
    ## -------------------------------------------------------- 
    ## clean_survey_all_days$mode_of_transport: Driving
    ##   distance_km       arrival       mode_of_transport
    ##  Min.   : 9.10   Min.   :-90.00   Cycling: 0       
    ##  1st Qu.:12.75   1st Qu.:  0.00   Driving:14       
    ##  Median :15.00   Median : 15.00   Transit: 0       
    ##  Mean   :16.13   Mean   : 17.50   Walking: 0       
    ##  3rd Qu.:18.75   3rd Qu.: 48.75                    
    ##  Max.   :26.80   Max.   :120.00                    
    ## -------------------------------------------------------- 
    ## clean_survey_all_days$mode_of_transport: Transit
    ##   distance_km       arrival         mode_of_transport
    ##  Min.   : 2.00   Min.   :-85.0000   Cycling: 0       
    ##  1st Qu.: 6.60   1st Qu.: -7.0000   Driving: 0       
    ##  Median : 8.10   Median :  0.0000   Transit:58       
    ##  Mean   :10.15   Mean   :  0.3276   Walking: 0       
    ##  3rd Qu.:10.10   3rd Qu.:  5.0000                    
    ##  Max.   :44.00   Max.   : 75.0000                    
    ## -------------------------------------------------------- 
    ## clean_survey_all_days$mode_of_transport: Walking
    ##   distance_km       arrival        mode_of_transport
    ##  Min.   :0.700   Min.   :-60.000   Cycling: 0       
    ##  1st Qu.:0.975   1st Qu.:-10.000   Driving: 0       
    ##  Median :1.000   Median : -3.000   Transit: 0       
    ##  Mean   :1.031   Mean   : -4.906   Walking:32       
    ##  3rd Qu.:1.025   3rd Qu.:  3.000                    
    ##  Max.   :1.600   Max.   : 25.000
