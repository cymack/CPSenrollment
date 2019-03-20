A CPS Enrollment Dataset
================
Charlotte Mack
November 16, 2018

Last updated March 20, 2019.
NB: This document is under construction! Comments are welcome.

This repository contains exploratory analysis and visualizations of enrollment data that were extracted from Chicago Public Schools (CPS) public records. The prepared data are available at this repository in Rds and csv formats; at present there are only high school data, with elementary school data forthcoming. The sets span the school years from 2006--2007 through 2017--2018, and will be periodically updated.

A glimpse of the high schools data:

    ## Observations: 2,097
    ## Variables: 10
    ## $ govern      <chr> "regular", "regular", "regular", "regular", "regular…
    ## $ school_id   <chr> "610245", "609695", "609696", "610402", "609708", "6…
    ## $ common_name <chr> "Douglass HS", "Amundsen HS", "Austin HS", "DeVry Ad…
    ## $ year        <dbl> 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006…
    ## $ total       <dbl> 737, 1500, 580, 223, 1738, 1694, 1439, 4278, 1936, 5…
    ## $ total_hs    <dbl> 384, 1500, 580, 223, 1738, 1694, 1439, 4278, 1936, 5…
    ## $ g09         <dbl> 215, 435, NA, NA, 606, 510, 513, 1103, 578, 254, 262…
    ## $ g10         <dbl> 119, 438, NA, NA, 504, 419, 331, 945, 546, 254, 305,…
    ## $ g11         <dbl> 50, 361, 367, 107, 380, 371, 291, 1230, 403, NA, 267…
    ## $ g12         <dbl> NA, 266, 213, 116, 248, 394, 304, 1000, 409, NA, 208…

Number of schools represented in the data: 248

Number of schools for which there are data for all years in the period:

    ## [1] 83

Missing observations could be a key feature of the CPS enrollment dataset in some applications, because many of them are not generated randomly but as a result of the entry and exit of schools from the CPS roster. These processes often result in grades being phased in or out at the transitioning school. The next few exhibits concern missing points on some or all data where the school does have a record for the year. The case not considered at the moment is truncation when a school does not exist for some years --a form of hidden missing observations so to speak.

Aggregate numbers of missing observations, by grade: ![](CPSenrollment_files/figure-markdown_github/missing_grade-1.png)

Missing grade observations by year, counts:

![](CPSenrollment_files/figure-markdown_github/_miss%20by%20year-1.png)

Missing grade observations by year, as a fraction of all for that year: ![](CPSenrollment_files/figure-markdown_github/_miss%20shares%20by%20year-1.png)

Of 270 incomplete records occuring in 2006 through 2012, 55 records or 20 percent of the subset are provided by schools in a particular track of the CPS, called Academic Achievement High Schools. These were smaller institutions about which more information would be desirable. In general they only enrolled students in grades 9 and 10, though Chicago Vocational AA, Tilden AA, and Crane AA had instances of grade 11 or 12 enrollment. The program was apparently discontinued after the 2012--2013 school year.

Boxplot overview of high school enrollment distribution by year: ![](CPSenrollment_files/figure-markdown_github/boxplot-1.png)

Notes
=====

The original CPS data are available in a series of spreadsheets at [CPS website.](http://www.cps.edu/SchoolData/Pages/SchoolData.aspx)[1] Downloadable data files that I have prepared are in this repository with Rds and csv extensions. The Rds files, which are used in R language programming, may have some type designations that are not in the csv files, but there should be no other difference.

[1] Membership data are under the heading "Demographics."
