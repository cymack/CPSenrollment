Reshape Your Data to the  Grammar of Your Graphics
========================================================
author: Charlotte Mack
date: April 23, 2019
autosize: true


A Tidy Dataset
========================================================


```
# A tibble: 6 x 3
  common_name       year total_hs
  <chr>            <dbl>    <dbl>
1 ACE Tech Chtr HS  2006      272
2 ACE Tech Chtr HS  2018      315
3 Amundsen HS       2006     1500
4 Amundsen HS       2018     1226
5 Bogan Tech HS     2006     2119
6 Bogan Tech HS     2018      781
```

```
# A tibble: 2 x 2
# Groups:   year [2]
   year     n
  <dbl> <int>
1  2006    84
2  2018    84
```

Standard plots are easy with tidy data
========================================================

```r
talk_dat %>% ggplot(aes(x = total_hs, y = ..density..)) +
    geom_density() + geom_histogram() + facet_wrap(~ year) 
```

![plot of chunk by-year distribution plots](new-figure/by-year distribution plots-1.png)

Un-tidying for a Side-by-side View
========================================================


```r
ranked <- talk_dat %>% spread(year, total_hs, drop = TRUE) %>% 
    rename(hs_06 = `2006`, 
           hs_18 = `2018`) %>% 
    mutate(rank_06 = trunc(rank(-hs_06, ties.method = "min")), 
           rank_18 = trunc(rank(-hs_18, ties.method = "min"))) %>%
    arrange(rank_06) %>%
    select(-c(school_id, hs_06, hs_18))

ranked
```

```
# A tibble: 84 x 4
   govern  common_name     rank_06 rank_18
   <chr>   <chr>             <dbl>   <dbl>
 1 regular Lane Tech HS          1       1
 2 regular Kelly HS              2       6
 3 regular Curie HS              3       3
 4 regular Schurz HS             4       9
 5 regular Taft HS               5       2
 6 regular Farragut HS           6      37
 7 regular Clemente HS           7      34
 8 regular Lincoln Park HS       8       4
 9 regular Steinmetz HS          9      20
10 regular Bogan Tech HS        10      33
# … with 74 more rows
```

Re-tidying to Plot a Side-by-side View
========================================================

<small>

```r
ranked %>% filter(rank_06 <= 15)  %>% 
    gather(key = rank_yr,  value = ranking, -common_name)
```
</small>
![plot of chunk unnamed-chunk-2](new-figure/unnamed-chunk-2-1.png)
