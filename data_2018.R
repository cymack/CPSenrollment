# This protocol creates an R-ready dataset of CPS school enrollment data for the
# school year 2018-2019, starting from the spreadsheet published by CPS at
# https://www.cps.edu/SchoolData/Pages/SchoolData.aspx. The protocol is a more
# compact version of the methods used in [data.R](), which covers preparation of
# all years from 2006-2007 through 2017-2018.

library(tidyverse)

# I assume the file in csv form is locally present, until I get a working RCurl
# command. If xls version is available, the same arguments can be used inside
# readxl::read_xls().

tbl.enrollment2018 <- read_csv("./Demographics_20thDay_2018.csv", 
                               skip=2, 
                               col_names = c("school_id",
                                             "school_name",
                                             "network",
                                             "total",
                                             "pe",
                                             "pk",
                                             "k",
                                             "g01",
                                             "g02",
                                             "g03",
                                             "g04",
                                             "g05",
                                             "g06",
                                             "g07",
                                             "g08",
                                             "g09",
                                             "g10",
                                             "g11",
                                             "g12"))
glimpse(tbl.enrollment2018)

# Immediately add a year variable, lest some disaster overtake us:

tbl.enrollment2018 <- mutate(tbl.enrollment2018, year=2018)
glimpse(tbl.enrollment2018)

# Made some type conversions:
tbl.enrollment2018 <- tbl.enrollment2018 %>% mutate(school_id = as.character(school_id))
tbl.enrollment2018 <- tbl.enrollment2018 %>% mutate_if(is.double, as.integer)
 

# Create a factor variable 'grades' to indicate schools that are high schools.
# The others will be classed as elementary schools for the moment; they are to
# be omitted from this project. This method assigns "high school" to any school
# having enrollment in any of the high school grades, so schools that either are
# phasing in or out, or schools with unconventional grade offerings, are
# included. 
# NB: In the 2018 listing, zeros rather than NA are used by CPS for missing grades.
tbl.enrollment2018 <- tbl.enrollment2018 %>% 
    mutate(grades = ifelse(g09 == 0 & g10 == 0 & g11 == 0 & g12 == 0,
                           "elementary",
                           "high school"
                           ))
tbl.enrollment2018 <- tbl.enrollment2018 %>% mutate(grades = as.factor(grades))
glimpse(tbl.enrollment2018)

# Checking whether the assignment seems ok:
#   select(tbl.enrollment2018, school_name, grades)





# There are only 4 "Contract" network schools in 2018, so these are included
# with the "Options" network in the "other" encoding for govern. The nested
# ifelse statements were discussed in Matloff, Art of R Programming.
tbl.enrollment2018 <- tbl.enrollment2018 %>% 
    mutate(govern = ifelse(network == "Charter", "charter",
                           ifelse(network == "Options" | 
                                      network == "Contract", 
                                  "other", "regular")))
glimpse(tbl.enrollment2018)

# Filtering and dropping unneeded variables to create the high school data set
# for 2018:
# 
tbl.enrollment2018_hs <- tbl.enrollment2018 %>% 
    filter(grades == "high school")
glimpse(tbl.enrollment2018_hs)

tbl.enrollment2018_hs <- tbl.enrollment2018_hs %>% 
    select( govern, school_id, school_name, year, total, g09, g10, g11, g12 )
glimpse(tbl.enrollment2018_hs)

summary(tbl.enrollment2018_hs)

# Identify the g09:g12 "NA" schools.
# This serves as a check on the validity of the "grades" classification of high
# and elementary schools. Also if any schools are in phase-in/phase-out periods
# they should be detected here.
# Schools having grade 9 missing:
tbl.na_g09_2018 <- tbl.enrollment2018_hs %>% 
    filter(g09 == 0) %>% 
    select(school_name)
tbl.na_g09_2018

# Schools having grade 10 missing:
tbl.na_g10_2018 <-  tbl.enrollment2018_hs %>% 
    filter(g10 == 0) %>% 
    select(school_name)
tbl.na_g10_2018

# Schools having grade 11 missing:
tbl.na_g11_2018 <-  tbl.enrollment2018_hs %>% 
    filter(g11 == 0) %>% 
    select(school_name)
tbl.na_g11_2018

# Schools having grade 12 missing:
tbl.na_g12_2018 <-  tbl.enrollment2018_hs %>% 
    filter(g12 == 0) %>% 
    select(school_name)
tbl.na_g12_2018

# Schools having enrollment only in grade 9 for 2018-19 year:
tbl.na_g101112_2018 <- tbl.enrollment2018_hs %>% 
    filter(g10 == 0 & g11 == 0 & g12 == 0) %>% 
    select(school_name, school_id)
tbl.na_g101112_2018

# Visits to the schools' websites show that Leland and Sayre schools are
# elementary schools by structure, having barely a handful of 9th graders, so
# they will be reclassified and dropped from the high school set. Chicago
# Collegiate Charter appears to be phasing in, and is set to have at least the
# first two high school grades, so it will be kept in the high school set.

tbl.drop2018 <- tbl.na_g101112_2018 %>% 
    filter(str_detect(school_name, "Leland") | 
               str_detect(school_name, "Sayre")  ) %>% 
    select(school_id)

tbl.enrollment2018_hs <- filter(tbl.enrollment2018_hs, 
                                !(school_id %in% tbl.drop2018$school_id) )

tbl.na_g091011_2018 <- tbl.enrollment2018_hs %>% 
    filter(g09 == 0 & g10 == 0 & g11 == 0) %>% 
    select(school_name, school_id)
tbl.na_g091011_2018

# There are no high schools (by the criterion here) having only grade 12
# enrollment, which would be possible phaseout schools.
tbl.enrollment2018_hs %>% 
    filter(is.na(g09) & is.na(g10) & is.na(g11) & !is.na(g12)) %>% 
    select(school_id, school_name)


# Correcting the grades observations for those schools in the main 2018 dataset:
tbl.enrollment2018 <- tbl.enrollment2018 %>% 
    mutate( grades  = ifelse( str_detect(school_name, "Leland") | 
                                  str_detect(school_name, "Sayre"), 
                              "elementary" , grades))

# The code block that is commented out below is not necessary for the 2018 data,
# as this time CPS has zeroed out all grades for us. (NB: They do not always do
# this!)
    # # Create a total_hs variable:
    # # Mutate auxilliary variables x09:x12 <- g09:g12
    # # ifelse( is.na(x09:x12), 0, x09:x12 )
    # # Total high school is vector sum of x09:x12
    # # Drop x09:x12
    # 
    # tbl.enrollment2018_hs <- tbl.enrollment2018_hs %>% 
    #     mutate(x09 = ifelse(is.na(g09), 0, g09 ), 
    #            x10 = ifelse(is.na(g10), 0, g10 ), 
    #            x11 = ifelse(is.na(g11), 0, g11 ), 
    #            x12 = ifelse(is.na(g12), 0, g12 ))

tbl.enrollment2018_hs <- tbl.enrollment2018_hs %>% 
    mutate( total_hs = g09 + g10 + g11 + g12 )  %>%
    select(govern, 
           school_id, 
           school_name, 
           year, 
           total, 
           total_hs, 
           g09, 
           g10, 
           g11, 
           g12)
glimpse(tbl.enrollment2018_hs)

# Common names are created for each school, in order to have a shorter and
# uniform reference name for tables and visualizations. These come by matching
# with common names already in the database for each school id, and creating new
# ones for cases with no match. The procedure follows that found in clean.R.

# Get the schools from the reference year, in this case 2017, by loading the
# current all-school data and filtering:
enrollment_2017 <- read_csv("enrollment_all_hs.csv") %>% 
        filter(year == 2017)

# Create common_names with as many matches from 2017 as found:
tbl.enrollment2018_hs <- tbl.enrollment2018_hs %>% 
    mutate(common_name = 
               enrollment_2017$common_name[match(school_id, enrollment_2017$school_id)])

# Counting the NAs generated, and identifying them:
tbl.enrollment2018_hs %>% count(is.na(common_name))

tbl.enrollment2018_hs %>% 
    filter(is.na(common_name)) %>% 
    select(school_id, school_name)

# This year there was only one new high school, Chicago Collegiate Charter
# School. The name Collegiate will be given to it:
tbl.enrollment2018_hs <- tbl.enrollment2018_hs %>%
    mutate(common_name = ifelse(school_id == "400161", "Collegiate", common_name))

# Replacing school_name by common_name:
tbl.enrollment2018_hs <- tbl.enrollment2018_hs %>% 
    select(govern, 
           school_id, 
           common_name, 
           year, 
           total, 
           total_hs, 
           g09, 
           g10, 
           g11, 
           g12)
glimpse(tbl.enrollment2018_hs)

# Search for missing observations The effect of using 0 codings in the
# recent data should be apparent.

# First, assign some handy indicator variables for missing observations, then
# sum them by year:
enrollment <- enrollment_all_hs %>% 
    mutate_at(vars(g09:g12), funs(miss = is.na(.)))

missing_by_yr <- enrollment %>% 
    select(year, g09_miss:g12_miss) %>% 
    group_by(year) %>% 
    summarize_at(vars(g09_miss:g12_miss), 
                      funs(sum(.)))
# Plot them in a facetted chart of missing by grade for each year:
missing_by_yr %>% 
         gather(key = feature, value = n, -year) %>% 
         mutate(feature = str_remove(feature, "_miss")) %>%
         ggplot(aes(x = fct_inorder(feature), y = n)) +
         geom_bar(stat = "identity", width = .5) + 
         labs(y = "Number",
              x = "Grade") +
         coord_flip() +
         facet_wrap(~year)

# Sure enough, nothing for 2018. The zero entries in principle are a mixture of
# true zero counts and counts that for whatever reason were not submitted.
# They can be recoded to NA, the better to see any patterns:
enrollment_all_hs %>% 
    filter(year == 2018) %>% 
    select(7:10) %>% 
    transmute_all(funs(ifelse(. == 0, NA, .))) %>% 
    DataExplorer::profile_missing() %>% 
    ggplot(aes(x = fct_inorder(feature), 
               y = num_missing)) + 
    geom_bar(stat = "identity", 
             width = .5) + 
    labs(y = "Number", 
         x = "Grade") + 
    coord_flip()

# There are very few missing overall in 2018, with by far the most occuring in
# 9th grade counts. Whatever coding we impose on the data will be wrong, but for
# consistency with the existing data set, I will recode the zeros in 2018 as NA.
# Since the only zero entries are in g09:g12 variables, mutate_all with a
# conditional function should work.

enrollment_all_hs <- enrollment_all_hs %>% 
    mutate_all(funs(ifelse(. == 0, NA, .)))

# Aggregated missing:
enrollment_all_hs %>% 
    select(7:10) %>% 
    DataExplorer::profile_missing() %>% 
    ggplot(aes(x = fct_inorder(feature), 
               y = num_missing)) + 
    geom_bar(stat = "identity", 
             width = .5) + 
    labs(y = "Number", 
         x = "Grade") + 
    coord_flip()

# Facet wrap:
enrollment <- enrollment_all_hs %>% 
    mutate_at(vars(g09:g12), funs(miss = is.na(.)))

missing_by_yr <- enrollment %>% 
    select(year, g09_miss:g12_miss) %>% 
    group_by(year) %>% 
    summarize_at(vars(g09_miss:g12_miss), 
                 funs(sum(.)))

missing_by_yr %>% 
    gather(key = feature, value = n, -year) %>% 
    mutate(feature = str_remove(feature, "_miss")) %>%
    ggplot(aes(x = fct_inorder(feature), y = n)) +
    geom_bar(stat = "identity", width = .5) + 
    labs(y = "Number",
         x = "Grade") +
    coord_flip() +
    facet_wrap(~year)


# Uncomment as needed:
# Two datasets worth saving:
# write_rds(tbl.enrollment2018, "./output/Rds/enrollment2018.Rds")
# write_rds(tbl.enrollment2018_hs, "./output/Rds/enrollment2018_hs.Rds")

# Create the final updated set for all years by 
enrollment_all_hs <- rbind(enrollment_all_hs, 
                           tbl.enrollment2018_hs)
