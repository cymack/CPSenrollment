Raw Facts, 2018
================
Charlotte Mack
2019-02-22

A set of comparisons for the CPS Enrollment data, to be visualized or arranged in tables elsewhere. At date, "first" year is 2006 and "last" year is 2018.

``` r
options(digits = 2)
```

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0       ✔ purrr   0.3.0  
    ## ✔ tibble  2.0.1       ✔ dplyr   0.8.0.1
    ## ✔ tidyr   0.8.2       ✔ stringr 1.4.0  
    ## ✔ readr   1.3.1       ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
enrollment_all_hs <- read_rds("./enrollment_all_hs.Rds")
```

``` r
# Filter first and this year from data
first_last <- enrollment_all_hs %>% filter(year %in% c(2006, 2018))
```

``` r
# Counts of schools, total and by governance
first_last %>% 
    group_by(year) %>%  
    count()
```

    ## # A tibble: 2 x 2
    ## # Groups:   year [2]
    ##    year     n
    ##   <dbl> <int>
    ## 1  2006   126
    ## 2  2018   183

``` r
first_last %>% 
    group_by(year, govern) %>%  
    count()
```

    ## # A tibble: 5 x 3
    ## # Groups:   year, govern [5]
    ##    year govern      n
    ##   <dbl> <chr>   <int>
    ## 1  2006 charter    11
    ## 2  2006 regular   115
    ## 3  2018 charter    48
    ## 4  2018 other      38
    ## 5  2018 regular    97

``` r
# Enrollments, total and by governance
first_last %>%  
    group_by(year) %>%  
    summarize(sum(total_hs))
```

    ## # A tibble: 2 x 2
    ##    year `sum(total_hs)`
    ##   <dbl>           <dbl>
    ## 1  2006          109132
    ## 2  2018          107345

``` r
first_last %>%  
    group_by(year, govern) %>%  
    summarize(sum(total_hs))
```

    ## # A tibble: 5 x 3
    ## # Groups:   year [2]
    ##    year govern  `sum(total_hs)`
    ##   <dbl> <chr>             <dbl>
    ## 1  2006 charter            5689
    ## 2  2006 regular          103443
    ## 3  2018 charter           23790
    ## 4  2018 other              8134
    ## 5  2018 regular           75421

``` r
# Largest and smallest enrollments overall and by governance
# 

# Largest overall:
first_last %>% 
    filter(year == 2006) %>%  
    arrange(desc(total_hs)) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 126 x 3
    ##    govern  common_name          total_hs
    ##    <chr>   <chr>                   <dbl>
    ##  1 regular Lane Tech HS             4278
    ##  2 regular Kelly HS                 3155
    ##  3 regular Curie HS                 3062
    ##  4 charter Youth Connctns Chrtr     2507
    ##  5 regular Schurz HS                2417
    ##  6 regular Taft HS                  2416
    ##  7 regular Farragut HS              2389
    ##  8 regular Clemente HS              2384
    ##  9 regular Lincoln Park HS          2249
    ## 10 regular Steinmetz HS             2157
    ## # … with 116 more rows

``` r
first_last %>% 
    filter(year == 2018) %>%  
    arrange(desc(total_hs)) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 183 x 3
    ##    govern  common_name     total_hs
    ##    <chr>   <chr>              <dbl>
    ##  1 regular Lane Tech HS        4190
    ##  2 regular Taft HS             3101
    ##  3 regular Curie HS            2916
    ##  4 regular Lincoln Park HS     2101
    ##  5 regular Young Magnet HS     1915
    ##  6 regular Kelly HS            1891
    ##  7 regular Jones CPHS          1865
    ##  8 regular Hubbard HS          1777
    ##  9 regular Schurz HS           1739
    ## 10 regular Juarez HS           1734
    ## # … with 173 more rows

``` r
# Smallest overall:
first_last %>% 
    filter(year == 2006) %>%  
    arrange(total_hs) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 126 x 3
    ##    govern  common_name         total_hs
    ##    <chr>   <chr>                  <dbl>
    ##  1 regular Lindblom CPHS             53
    ##  2 regular Big Pict HS - Yards       77
    ##  3 regular Bowen HS                  77
    ##  4 charter Chgo Math/Sci Chtr        77
    ##  5 regular Fenger AA HS              81
    ##  6 regular Dugan Alt HS              82
    ##  7 regular Big Pict HS - Metro       84
    ##  8 regular DuSable HS                86
    ##  9 regular IDOC/Healy South HS       89
    ## 10 regular Spry Comm Links HS        93
    ## # … with 116 more rows

``` r
first_last %>% 
    filter(year == 2018) %>%  
    arrange(total_hs) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 183 x 3
    ##    govern  common_name              total_hs
    ##    <chr>   <chr>                       <dbl>
    ##  1 other   Camelot Safe                    6
    ##  2 regular Simpson HS                     38
    ##  3 charter Collegiate                     40
    ##  4 other   Instituto Lozano               85
    ##  5 regular TEAM Englewood HS              92
    ##  6 regular Hope CPHS                      95
    ##  7 other   YCCS-Holistic                  95
    ##  8 charter Foundations                    97
    ##  9 regular Douglass HS                   101
    ## 10 regular Peace & Education Alt HS      103
    ## # … with 173 more rows

``` r
# Largest of regular governance:
first_last %>% 
    filter(year == 2006 & govern == "regular") %>%  
    arrange(desc(total_hs)) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 115 x 3
    ##    govern  common_name     total_hs
    ##    <chr>   <chr>              <dbl>
    ##  1 regular Lane Tech HS        4278
    ##  2 regular Kelly HS            3155
    ##  3 regular Curie HS            3062
    ##  4 regular Schurz HS           2417
    ##  5 regular Taft HS             2416
    ##  6 regular Farragut HS         2389
    ##  7 regular Clemente HS         2384
    ##  8 regular Lincoln Park HS     2249
    ##  9 regular Steinmetz HS        2157
    ## 10 regular Bogan Tech HS       2119
    ## # … with 105 more rows

``` r
first_last %>% 
    filter(year == 2018 & govern == "regular") %>%  
    arrange(desc(total_hs)) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 97 x 3
    ##    govern  common_name     total_hs
    ##    <chr>   <chr>              <dbl>
    ##  1 regular Lane Tech HS        4190
    ##  2 regular Taft HS             3101
    ##  3 regular Curie HS            2916
    ##  4 regular Lincoln Park HS     2101
    ##  5 regular Young Magnet HS     1915
    ##  6 regular Kelly HS            1891
    ##  7 regular Jones CPHS          1865
    ##  8 regular Hubbard HS          1777
    ##  9 regular Schurz HS           1739
    ## 10 regular Juarez HS           1734
    ## # … with 87 more rows

``` r
# Smallest of regular governance:
first_last %>% 
    filter(year == 2006 & govern == "regular") %>%  
    arrange(total_hs) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 115 x 3
    ##    govern  common_name         total_hs
    ##    <chr>   <chr>                  <dbl>
    ##  1 regular Lindblom CPHS             53
    ##  2 regular Big Pict HS - Yards       77
    ##  3 regular Bowen HS                  77
    ##  4 regular Fenger AA HS              81
    ##  5 regular Dugan Alt HS              82
    ##  6 regular Big Pict HS - Metro       84
    ##  7 regular DuSable HS                86
    ##  8 regular IDOC/Healy South HS       89
    ##  9 regular Spry Comm Links HS        93
    ## 10 regular Las Casas HS              93
    ## # … with 105 more rows

``` r
first_last %>% 
    filter(year == 2018 & govern == "regular") %>%  
    arrange(total_hs) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 97 x 3
    ##    govern  common_name              total_hs
    ##    <chr>   <chr>                       <dbl>
    ##  1 regular Simpson HS                     38
    ##  2 regular TEAM Englewood HS              92
    ##  3 regular Hope CPHS                      95
    ##  4 regular Douglass HS                   101
    ##  5 regular Peace & Education Alt HS      103
    ##  6 regular Manley HS                     114
    ##  7 regular Graham, R. Trng CTR           117
    ##  8 regular Robeson HS                    128
    ##  9 regular Spry Comm Links HS            129
    ## 10 regular Harper HS                     134
    ## # … with 87 more rows

``` r
# Largest of charter governance:
first_last %>% 
    filter(year == 2006 & govern == "charter") %>%  
    arrange(desc(total_hs)) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 11 x 3
    ##    govern  common_name               total_hs
    ##    <chr>   <chr>                        <dbl>
    ##  1 charter Youth Connctns Chrtr          2507
    ##  2 charter Chgo Intl Chrt - Bucktown      897
    ##  3 charter Noble St. Chrtr HS             480
    ##  4 charter North Lawndale Chrtr           389
    ##  5 charter ASPIRA Chrtr - Ramirez         341
    ##  6 charter ACE Tech Chtr HS               272
    ##  7 charter Yng Women Ldrshp Chrt          236
    ##  8 charter ACT Chtr HS                    188
    ##  9 charter Perspectives Chrtr H           186
    ## 10 charter Shabazz Charter                116
    ## 11 charter Chgo Math/Sci Chtr              77

``` r
first_last %>% 
    filter(year == 2018 & govern == "charter") %>%  
    arrange(desc(total_hs)) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 48 x 3
    ##    govern  common_name                total_hs
    ##    <chr>   <chr>                         <dbl>
    ##  1 charter NSC-Bulls                      1137
    ##  2 charter Noble-ITW                      1019
    ##  3 charter NSC-Pritzker                    986
    ##  4 charter NSC-Muchin                      959
    ##  5 charter NSC-UIC                         935
    ##  6 charter Chgo Intl Chrt - Northtown      909
    ##  7 charter NSC-Englewood                   802
    ##  8 charter NSC-Comer                       782
    ##  9 charter Instituto Health                750
    ## 10 charter Intrinsic                       679
    ## # … with 38 more rows

``` r
# Smallest of charter governance:
first_last %>% 
    filter(year == 2006 & govern == "charter") %>%  
    arrange(total_hs) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 11 x 3
    ##    govern  common_name               total_hs
    ##    <chr>   <chr>                        <dbl>
    ##  1 charter Chgo Math/Sci Chtr              77
    ##  2 charter Shabazz Charter                116
    ##  3 charter Perspectives Chrtr H           186
    ##  4 charter ACT Chtr HS                    188
    ##  5 charter Yng Women Ldrshp Chrt          236
    ##  6 charter ACE Tech Chtr HS               272
    ##  7 charter ASPIRA Chrtr - Ramirez         341
    ##  8 charter North Lawndale Chrtr           389
    ##  9 charter Noble St. Chrtr HS             480
    ## 10 charter Chgo Intl Chrt - Bucktown      897
    ## 11 charter Youth Connctns Chrtr          2507

``` r
first_last %>% 
    filter(year == 2018 & govern == "charter") %>%  
    arrange(total_hs) %>%  
    select(govern, common_name, total_hs)
```

    ## # A tibble: 48 x 3
    ##    govern  common_name           total_hs
    ##    <chr>   <chr>                    <dbl>
    ##  1 charter Collegiate                  40
    ##  2 charter Foundations                 97
    ##  3 charter Horizon-Southwest          110
    ##  4 charter Chicago Virtual Chtr       181
    ##  5 charter UNO-Rogers Park            182
    ##  6 charter Yng Women Ldrshp Chrt      193
    ##  7 charter Urban Prep-E Garfield      213
    ##  8 charter Perspectives Chrtr HS      260
    ##  9 charter CIC-Quest North            262
    ## 10 charter Urban Prep Chtr HS         263
    ## # … with 38 more rows

``` r
# Using anti-joins, determine which schools (by school_id) left the database
# during the period and which ones entered:
first <- first_last %>% filter(year == 2006)
this <- first_last %>% filter(year == 2018)

# School IDs present in 2006 but not in 2018:
changes_f <- anti_join(first, this, by = "school_id")
changes_f %>% glimpse()
```

    ## Observations: 39
    ## Variables: 10
    ## $ govern      <chr> "regular", "regular", "regular", "regular", "regular…
    ## $ school_id   <chr> "609696", "610377", "610370", "609763", "610378", "6…
    ## $ common_name <chr> "Austin HS", "Senn AA HS", "Best Practice HS", "Coll…
    ## $ year        <dbl> 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006…
    ## $ total       <dbl> 580, 142, 374, 852, 320, 1038, 163, 250, 499, 563, 2…
    ## $ total_hs    <dbl> 580, 142, 374, 852, 320, 998, 163, 250, 499, 509, 21…
    ## $ g09         <dbl> NA, 55, 109, 329, 205, 319, 37, 158, NA, 238, 119, N…
    ## $ g10         <dbl> NA, 87, 77, 226, 115, 240, 126, 92, NA, 155, 93, 343…
    ## $ g11         <dbl> 367, NA, 105, 157, NA, 277, NA, NA, 376, 50, NA, 182…
    ## $ g12         <dbl> 213, NA, 83, 140, NA, 162, NA, NA, 123, 66, NA, 112,…

``` r
# School IDs not present in 2006 but appearing in 2018:
changes_t <- anti_join(this, first, by = "school_id")
changes_t %>% glimpse()
```

    ## Observations: 96
    ## Variables: 10
    ## $ govern      <chr> "regular", "regular", "regular", "regular", "regular…
    ## $ school_id   <chr> "610564", "610518", "610524", "610513", "610529", "6…
    ## $ common_name <chr> "Disney", "VOISE", "Alcott", "Air Force", "Ogden", "…
    ## $ year        <dbl> 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018…
    ## $ total       <dbl> 711, 219, 311, 262, 884, 487, 231, 900, 92, 649, 260…
    ## $ total_hs    <dbl> 498, 219, 311, 262, 529, 487, 231, 900, 92, 649, 260…
    ## $ g09         <dbl> 163, 50, 87, 33, 222, 77, 70, 260, 10, 208, 67, 346,…
    ## $ g10         <dbl> 135, 60, 89, 74, 137, 139, 44, 241, 21, 174, 69, 275…
    ## $ g11         <dbl> 97, 45, 68, 77, 80, 139, 55, 224, 24, 142, 52, 274, …
    ## $ g12         <dbl> 103, 64, 67, 78, 90, 132, 62, 175, 37, 125, 72, 295,…

``` r
# Form a data subset having only schools represented in both years, by id, and
# use this to calculate enrollment changes, i.e. absolute, percent, and
# annualized rate, over the period.

deltas <- first_last %>% 
    filter(!(school_id %in% changes_f$school_id) & 
               !(school_id %in% changes_t$school_id)) %>% 
    mutate(delta = ifelse(year == 2018, 
                          total_hs - lag(total_hs, 
                                         1, 
                                         order_by = school_id), 
                          NA), 
           delta_pct_by_100 = delta/lag(total_hs, 
                                 1, 
                                 order_by = school_id),
           delta_rate = ((1 + delta_pct_by_100)^(1/12)) - 1) %>% 
    filter(year == 2018)




# Highest growth over period:
deltas %>%  
    arrange(desc(delta)) %>%  
    select(common_name, total_hs, delta) %>%  
    head(10)
```

    ## # A tibble: 10 x 3
    ##    common_name         total_hs delta
    ##    <chr>                  <dbl> <dbl>
    ##  1 Jones CPHS              1865  1131
    ##  2 Lindblom MSHS           1090   976
    ##  3 Westinghouse II         1189   734
    ##  4 Taft HS                 3101   685
    ##  5 North-Grand HS           958   450
    ##  6 Rickover Naval HS        553   443
    ##  7 Phoenix Mil Acad HS      531   358
    ##  8 Infinity HS              418   318
    ##  9 Chicago Acad HS          542   291
    ## 10 Chgo Math/Sci Chtr       337   260

``` r
# Greatest decline over period:
deltas %>%  
    arrange(delta) %>%  
    select(common_name, total_hs, delta) %>%  
    head(87)
```

    ## # A tibble: 87 x 3
    ##    common_name    total_hs delta
    ##    <chr>             <dbl> <dbl>
    ##  1 Farragut HS         713 -1676
    ##  2 Clemente HS         754 -1630
    ##  3 Julian HS           516 -1435
    ##  4 Kelvyn Park HS      322 -1372
    ##  5 Bogan Tech HS       781 -1338
    ##  6 Kelly HS           1891 -1264
    ##  7 Tilden HS           250 -1179
    ##  8 Gage Park HS        328 -1173
    ##  9 Dunbar Voc HS       457 -1170
    ## 10 Chicago Voc HS      880 -1150
    ## # … with 77 more rows

``` r
# Highest growth over period by percent:
deltas %>%  
    arrange(desc(delta_pct_by_100)) %>%  
    select(common_name, delta, delta_pct_by_100) %>%  
    head(10)
```

    ## # A tibble: 10 x 3
    ##    common_name         delta delta_pct_by_100
    ##    <chr>               <dbl>            <dbl>
    ##  1 Lindblom MSHS         976             8.56
    ##  2 Rickover Naval HS     443             4.03
    ##  3 Chgo Math/Sci Chtr    260             3.38
    ##  4 Infinity HS           318             3.18
    ##  5 World Language HS     248             2.38
    ##  6 Phoenix Mil Acad HS   358             2.07
    ##  7 Sch Of Soc Just HS    186             1.86
    ##  8 Westinghouse II       734             1.61
    ##  9 Bronzeville HS        196             1.56
    ## 10 Jones CPHS           1131             1.54

``` r
# Greatest decline over period by percent:
deltas %>%  
    arrange(delta_pct_by_100) %>%  
    select(common_name, delta, delta_pct_by_100) %>%  
    head(25)
```

    ## # A tibble: 25 x 3
    ##    common_name    delta delta_pct_by_100
    ##    <chr>          <dbl>            <dbl>
    ##  1 Robeson HS     -1130           -0.898
    ##  2 Harper HS      -1127           -0.894
    ##  3 Hope CPHS       -774           -0.891
    ##  4 Manley HS       -848           -0.881
    ##  5 Hirsch HS       -761           -0.847
    ##  6 Tilden HS      -1179           -0.825
    ##  7 Kelvyn Park HS -1372           -0.810
    ##  8 Simpson HS      -142           -0.789
    ##  9 Corliss HS     -1117           -0.783
    ## 10 Gage Park HS   -1173           -0.781
    ## # … with 15 more rows

``` r
# Fastest rate of growth over period, annualized:
deltas %>%  
    arrange(desc(delta_rate)) %>%  
    select(common_name, delta_pct_by_100, delta_rate) %>%  
    head(10)
```

    ## # A tibble: 10 x 3
    ##    common_name         delta_pct_by_100 delta_rate
    ##    <chr>                          <dbl>      <dbl>
    ##  1 Lindblom MSHS                   8.56     0.207 
    ##  2 Rickover Naval HS               4.03     0.144 
    ##  3 Chgo Math/Sci Chtr              3.38     0.131 
    ##  4 Infinity HS                     3.18     0.127 
    ##  5 World Language HS               2.38     0.107 
    ##  6 Phoenix Mil Acad HS             2.07     0.0980
    ##  7 Sch Of Soc Just HS              1.86     0.0915
    ##  8 Westinghouse II                 1.61     0.0833
    ##  9 Bronzeville HS                  1.56     0.0813
    ## 10 Jones CPHS                      1.54     0.0808

``` r
# Fastest rate of decline over period, annualized
deltas %>%  
    arrange(delta_rate) %>%  
    select(common_name, delta_pct_by_100, delta_rate) %>%  
    head(25)
```

    ## # A tibble: 25 x 3
    ##    common_name    delta_pct_by_100 delta_rate
    ##    <chr>                     <dbl>      <dbl>
    ##  1 Robeson HS               -0.898     -0.173
    ##  2 Harper HS                -0.894     -0.170
    ##  3 Hope CPHS                -0.891     -0.168
    ##  4 Manley HS                -0.881     -0.163
    ##  5 Hirsch HS                -0.847     -0.145
    ##  6 Tilden HS                -0.825     -0.135
    ##  7 Kelvyn Park HS           -0.810     -0.129
    ##  8 Simpson HS               -0.789     -0.122
    ##  9 Corliss HS               -0.783     -0.119
    ## 10 Gage Park HS             -0.781     -0.119
    ## # … with 15 more rows

``` r
# Prepare a complete case data.frame using school_id as the only key variable:
both <- first_last %>%  
    filter(!(school_id %in% changes_f$school_id) &
               !(school_id %in% changes_t$school_id)) %>%  
    select(govern, school_id, year, total_hs)
# NB: There are three school_id values that apply to more than one common_name.
# This causes spurious NA to appear when the spread operation above is applied
# to a both data.frame that includes the common_name variable. The three schools
# are Westinghouse/Westinghouse II (609693), Perspectives Charter (400064) which
# is probably a typo error in the database, and Dugan/Peace and Education
# (610386). One way around this until I decide whether to alter the main
# database is to use the deltas data.frame to correct the names and then take
# the ranks:

ranked <- both %>% 
    mutate(common_name = deltas$common_name[match(school_id,
                                                  deltas$school_id)]) %>% 
    spread(year, total_hs, drop = TRUE) %>% 
    rename(hs_06 = `2006`, hs_18 = `2018`) %>% 
    mutate(rank_06 = trunc(rank(-hs_06, ties.method = "min")), 
           rank_18 = trunc(rank(-hs_18, ties.method = "min")))

ranked %>%  
    arrange(rank_06) %>%  
    select(govern, common_name, rank_06, rank_18) #  %>% print(15)
```

    ## # A tibble: 87 x 4
    ##    govern  common_name     rank_06 rank_18
    ##    <chr>   <chr>             <dbl>   <dbl>
    ##  1 regular Lane Tech HS          1       1
    ##  2 regular Kelly HS              2       6
    ##  3 regular Curie HS              3       3
    ##  4 regular Schurz HS             4       9
    ##  5 regular Taft HS               5       2
    ##  6 regular Farragut HS           6      38
    ##  7 regular Clemente HS           7      35
    ##  8 regular Lincoln Park HS       8       4
    ##  9 regular Steinmetz HS          9      20
    ## 10 regular Bogan Tech HS        10      34
    ## # … with 77 more rows

``` r
ranked %>%  
    arrange(desc(rank_06)) %>%  
    select(govern, common_name, rank_06, rank_18) #  %>% print(15)
```

    ## # A tibble: 87 x 4
    ##    govern  common_name              rank_06 rank_18
    ##    <chr>   <chr>                      <dbl>   <dbl>
    ##  1 charter Chgo Math/Sci Chtr            87      56
    ##  2 regular Peace & Education Alt HS      86      84
    ##  3 regular Spry Comm Links HS            85      80
    ##  4 regular Sch Of Soc Just HS            82      64
    ##  5 regular Infinity HS                   82      49
    ##  6 regular Multicultural Arts HS         82      70
    ##  7 regular World Language HS             81      54
    ##  8 regular Rickover Naval HS             80      42
    ##  9 regular Lindblom MSHS                 79      24
    ## 10 regular Uplift Community HS           78      69
    ## # … with 77 more rows

Use dumbbells with color aesthetic for year, arrow connecting 06 to 18, for a portion of the data --- maybe above the median --- or in panels for the entire set. Write up the factsheet as a demonstration of things one can do with data reshaping and grammar of graphics.
