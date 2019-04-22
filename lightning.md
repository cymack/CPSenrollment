Lightning Talk
================
Charlotte Mack
April 21, 2019

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(ggplot2)
library(readr)
library(forcats)
```

The purpose of this talk is to encourage R users to learn to reshape data with R tools, and to think in terms of the shape or structure of data when forming analytical objects. My example will show how moving easily from a tidy structure to a spread-type structure can even make it possible to see interesting patterns in tabulated data, while visualizing those patterns might call for shaping the data back to a tidy form, to have the necessary aesthetic mappings to render the ideas.

The example data is an extract of enrollment data for high schools in the Chicago Public Schools district. The full set is available at [my CPSenrollment github site](https://github.com/cymack/CPSenrollment). Here, we just use records from September of 2006 and 2018, the first and last periods of the whole series. Only schools that have records for both of those years are used.

``` r
talk_dat <- read_csv("talk_dat.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   govern = col_character(),
    ##   school_id = col_double(),
    ##   common_name = col_character(),
    ##   year = col_double(),
    ##   total_hs = col_double()
    ## )

``` r
talk_dat %>% 
    arrange(school_id) %>%
    head()
```

    ## # A tibble: 6 x 5
    ##   govern  school_id common_name         year total_hs
    ##   <chr>       <dbl> <chr>              <dbl>    <dbl>
    ## 1 charter    400010 ACE Tech Chtr HS    2006      272
    ## 2 charter    400010 ACE Tech Chtr HS    2018      315
    ## 3 charter    400035 Chgo Math/Sci Chtr  2006       77
    ## 4 charter    400035 Chgo Math/Sci Chtr  2018      337
    ## 5 charter    400051 Noble St. Chrtr HS  2006      480
    ## 6 charter    400051 Noble St. Chrtr HS  2018      651

``` r
talk_dat %>% 
    group_by(year) %>% 
    count()
```

    ## # A tibble: 2 x 2
    ## # Groups:   year [2]
    ##    year     n
    ##   <dbl> <int>
    ## 1  2006    84
    ## 2  2018    84

This data set is in tidy form: each row records an observation, each column a variable, and of course every observed unit is of the same type, a school (RDS 149). In this structure we can easily use base R commands such as head(), tail(), and summary(), or tidyverse commands such as dplyr::glimpse() to get an overview of what the data.frame contains. Whether we use base::plot(), ggplot2::ggplot(), or some other graphing engine, it is straightforward to generate any number of standard graphs to visualize features of the set as a whole, or to compare differences among the units with facetted plots. The structure of the data in this form fits the structure required by our tools quite well.

``` r
talk_dat %>% ggplot(aes(x = total_hs, y = ..density..)) +
    geom_density(size = 1, alpha = 0.5) +
    geom_histogram(alpha = 0.5, bins = 20) +
    facet_wrap(~ year) +
    labs(x = "Total High School Enrollment")
```

![](lightning_files/figure-markdown_github/by-year%20distribution%20plots-1.png)

It sometimes happens, however, that the tidy structure causes us to run around unnecessarily to get a grasp on some matter of interest. A longitudinal data set such as the schools data here poses such challenges. Sometimes we do not want to emphasize each observation, as we get from the tidy records. Instead, we want to change what we regard as the unit of observation from say, one school at one year, to one school over all the years in the survey.

It is possible, of course to do this data reshaping in a spreadsheet, by cutting and transpose pasting. Please, do not ever do this! It is horrifyingly error-prone, and consumes mass quantities of time even for relatively smaller data tables such as the present one. As even Hadley Wickham himself has admitted, the vocabulary and mechanics of spreading and gathering are a little tough. However, learning them is productive confusion as opposed to the other kind.

When one is comfortable with reshaping, useful things are possible. Again returning to longitudinal data, one often wants to make a static comparison between the units at two different dates: How do the schools rank by their enrollments at the beginning and end of the survey? One can of course filter two lists, after creating the ranking variables, and run around looking at them separately. Or, having become comfortable with data spreading, one can generate a side-by-side table.

``` r
ranked <- talk_dat %>% spread(year, total_hs, drop = TRUE) %>% 
    rename(hs_06 = `2006`, 
           hs_18 = `2018`) %>% 
    mutate(rank_06 = trunc(rank(-hs_06, ties.method = "min")), 
           rank_18 = trunc(rank(-hs_18, ties.method = "min"))) %>%
    arrange(rank_06) %>%
    select(-school_id)

ranked
```

    ## # A tibble: 84 x 6
    ##    govern  common_name     hs_06 hs_18 rank_06 rank_18
    ##    <chr>   <chr>           <dbl> <dbl>   <dbl>   <dbl>
    ##  1 regular Lane Tech HS     4278  4190       1       1
    ##  2 regular Kelly HS         3155  1891       2       6
    ##  3 regular Curie HS         3062  2916       3       3
    ##  4 regular Schurz HS        2417  1739       4       9
    ##  5 regular Taft HS          2416  3101       5       2
    ##  6 regular Farragut HS      2389   713       6      37
    ##  7 regular Clemente HS      2384   754       7      34
    ##  8 regular Lincoln Park HS  2249  2101       8       4
    ##  9 regular Steinmetz HS     2157  1278       9      20
    ## 10 regular Bogan Tech HS    2119   781      10      33
    ## # … with 74 more rows

This table is not essentially tidy, as it was generated from a spread, untidy data structure which has observations for different years that are treated as variables in each of its observation. But, scanning down the table shows some interesting movement of ranks by a number of schools, with some of the largest at the beginning of the period now appearing in the middle or lower quintiles. We saw earlier that average school sizes have shrunk as well, so these schools have experienced eye-opening losses in enrollment.

How to make this point even clearer? A visualization, of course. I decided to try a simple graph in which each school had a line with a dot in one color for its 2006 rank, one in another color for 2018, and an arrow connecting them to emphasize the direction in which the rank moved. I wanted to do this with as few geom layers as possible, but getting the arrow pointing in consistently the proper direction with just one command was the clincher that required the data be restructured back into tidy form.

``` r
ranked %>% filter(rank_06 <= 15)  %>% 
    mutate(common_name = 
               fct_reorder(common_name, rank_06, median)) %>%
    select(-starts_with("hs")) %>% 
    gather(key = rank_yr, 
           value = ranking, 
           -govern,
           -common_name) %>% 
    ggplot(aes(x = fct_rev(common_name), 
               y = ranking, 
               color = rank_yr)) + 
    geom_point(position = position_jitter(width = 0, height = 0.6)) +
    coord_flip() + 
    geom_path(aes(group = common_name), 
              lineend = "butt", 
              linejoin = "mitre", 
              arrow = grid::arrow(angle = 15, 
                                  length = unit(.2, "cm"), 
                                  ends = "last", 
                                  type = "closed"))
```

![](lightning_files/figure-markdown_github/re-tidy%20and%20plot-1.png)

Now there can be an aesthetic mapping within the gathered variable rank\_yr to color the dots appropriately within one geom\_point() layer. This also creates a consistent beginning and end for the arrow specification in geom\_path(). There might be other ways to do this, even within ggplot2, but this way seems very fluid, and only at the cost of learning to recognize data structure problems, and learning to be comfortable with correcting them with a programmatic reshape command.
