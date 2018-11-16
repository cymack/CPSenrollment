A CPS Enrollment Dataset
================
Charlotte Mack
November 16, 2018

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.7
    ## ✔ tidyr   0.8.2     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
enrollment <- read_rds("./enrollment_all_hs.Rds")
enrollment %>% glimpse()
```

    ## Observations: 1,914
    ## Variables: 10
    ## $ govern      <fct> regular, regular, regular, regular, regular, regul...
    ## $ school_id   <int> 610245, 609695, 609696, 610402, 609708, 609716, 60...
    ## $ common_name <chr> "Douglass HS", "Amundsen HS", "Austin HS", "DeVry ...
    ## $ year        <dbl> 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 20...
    ## $ total       <int> 737, 1500, 580, 223, 1738, 1694, 1439, 4278, 1936,...
    ## $ total_hs    <dbl> 384, 1500, 580, 223, 1738, 1694, 1439, 4278, 1936,...
    ## $ g09         <int> 215, 435, NA, NA, 606, 510, 513, 1103, 578, 254, 2...
    ## $ g10         <int> 119, 438, NA, NA, 504, 419, 331, 945, 546, 254, 30...
    ## $ g11         <int> 50, 361, 367, 107, 380, 371, 291, 1230, 403, NA, 2...
    ## $ g12         <int> NA, 266, 213, 116, 248, 394, 304, 1000, 409, NA, 2...
