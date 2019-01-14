#
#
#
# Preparing Chicago Public Schools datasets on
# enrollment and graduation and dropout rates
# for analysis in R.
#
# Data are Excel spreadsheets downloaded from
# the CPS website at http://www.cps.edu/SchoolData/Pages/SchoolData.aspx
# 
#
# Enrollment data are in a series of spreadsheets,
# one for each year, covering all the Chicago
# Public Schools, including charter schools. There
# are many inconsistencies in the formatting. The
# general plan will be to use one file, the most
# recent, to establish a tidy format that includes
# the necessary data, then apply that form to 
# the other files in turn, and finally to bind
# or join the assemblage into one dataframe or
# tibble.
#
#
# The work will be done using tidyverse commands
# wherever possible. Also, the forcats and stringr
# packages, which are not in the tidyverse core,
# are going to be needed.
#

library(tidyverse)
library(forcats)
library(stringr)
library(magrittr)

sink("data_cps.out", split=TRUE)
#
#
#
# The xlsx file for the 2016-7 academic year was
# saved in csv format, after reformatting the numbers
# so that no 000s separator was in the file.
# This file will be loaded using read_csv().
#
# I use similar procedures for the remaining original data spreadsheets. Note,
# however, that each one has peculiarities.
# 
# 
# The basic protocol is to read in the csv file, assigning uniform variable
# names; check the results using glimpse(); insert new variables for year,
# grades, and govern; repeat as necessary for subsets of the original table,
# e.g. regular and charter subsets; check again using glimpse(), dim(), head(),
# tail(), and summary(); bind any subsets using rbind() or bind_rows(), and
# check head() and tail() again; filter to create a high school-only dataset; 
# select the needed variables, generally govern, school_id, common_name (or
# school_name), year, total, g09:g12; add the total_hs variable; find largest
# and smallest using total_hs, recheck dim() and summary(), and count govern
# subgroups.


################################################################
#                                                              #
#	Data for 2017                                                #
#                                                              #
################################################################


tbl.enrollment2017 <- read_csv("./Demographics_20thDay_2017.csv", 
                               skip=2, 
                               col_names = c("network",
                                             "school_id",
                                             "school_name",
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
glimpse(tbl.enrollment2017)

# Immediately add a year variable, lest some disaster overtake us:

tbl.enrollment2017 <- mutate(tbl.enrollment2017, year=2017)
glimpse(tbl.enrollment2017)
#
# Create a factor variable 'grades' to indicate schools that are high schools.
# The others will be classed as elementary schools for the moment; they are to
# be omitted from this project. This method assigns "high school" to any school
# having enrollment in any of the high school grades, so schools that either are
# phasing in or out, or schools with unconventional grade offerings, are
# included.
tbl.enrollment2017 <- tbl.enrollment2017 %>% 
  mutate(grades = ifelse(!is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                         "high school", 
                         "elementary" ))
glimpse(tbl.enrollment2017)

# Checking whether the assignment seems ok:
select(tbl.enrollment2017, school_name, grades)





# There are only 4 "Contract" network schools in 2017, so these are included
# with the "Options" network in the "other" encoding for govern. The nested
# ifelse statements were discussed in Matloff, Art of R Programming.
tbl.enrollment2017 <- tbl.enrollment2017 %>% 
  mutate(govern = ifelse(network == "Charter", "charter", 
                         ifelse(network == "Options" | network == "Contract", 
                                "other", "regular")))
glimpse(tbl.enrollment2017)

# Filtering and dropping unneeded variables to create the high school data set
# for 2017:
# 
tbl.enrollment2017_hs <- tbl.enrollment2017 %>% 
  filter(grades == "high school")
glimpse(tbl.enrollment2017_hs)

tbl.enrollment2017_hs <- tbl.enrollment2017_hs %>% 
  select( govern, school_id, school_name, year, total, g09, g10, g11, g12 )
glimpse(tbl.enrollment2017_hs)

summary(tbl.enrollment2017_hs)

# Identify the g09:g12 "NA" schools.
# Schools having grade 9 missing:
tbl.enrollment2017_hs %>% 
  filter(is.na(g09)) %>% 
  select(school_name)

# Schools having grade 10 missing:
tbl.na_g10_2017 <-  tbl.enrollment2017_hs %>% 
  filter(is.na(g10)) %>% 
  select(school_name)

# Schools having grade 11 missing:
tbl.na_g11_2017 <-  tbl.enrollment2017_hs %>% 
  filter(is.na(g11)) %>% 
  select(school_name)

# Schools having grade 12 missing:
tbl.na_g12_2017 <-  tbl.enrollment2017_hs %>% 
  filter(is.na(g12)) %>% 
  select(school_name)

# Schools having enrollment only in grade 9 for 2016-17 year:
tbl.na_g101112_2017 <- tbl.enrollment2017_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(school_name, school_id)

# Visits to the schools' websites show that Keller and Providence Englewood are 
# elementary schools by structure, so they will be dropped from the set. The
# others are phasing in to full high schools several years hence.

tbl.drop2017 <- tbl.na_g101112_2017 %>% 
  filter(str_detect(school_name, "Keller") | 
           str_detect(school_name, "Providence")  ) %>% 
  select(school_id)

tbl.enrollment2017_hs <- filter(tbl.enrollment2017_hs, 
                                !(school_id %in% tbl.drop2017$school_id) )

# There are no high schools (by the criterion here) having only grade 12
# enrollment, i.e. possible phaseout schools.
tbl.enrollment2017_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11) & !is.na(g12)) %>% 
  select(school_id, school_name)


# Correcting the grades observations for those schools in the main 2017 dataset:
tbl.enrollment2017 <- tbl.enrollment2017 %>% 
  mutate( grades  = ifelse( str_detect(school_name, "Keller") | 
                              str_detect(school_name, "Providence"), 
                            "elementary" , grades))



# The largest and smallest schools by total enrollment:

tbl.enrollment2017_hs %>% 
  select( govern, school_name, total, year) %>% 
  arrange(desc(total)) 

tbl.enrollment2017_hs %>% 
  select( govern, school_name, total, year) %>% 
  arrange(total) 


# Create a total_hs variable:
# Mutate auxilliary variables x09:x12 <- g09:g12
# ifelse( is.na(x09:x12), 0, x09:x12 )
# Total high school is vector sum of x09:x12
# Drop x09:x12

tbl.enrollment2017_hs <- tbl.enrollment2017_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09 ), 
         x10 = ifelse(is.na(g10), 0, g10 ), 
         x11 = ifelse(is.na(g11), 0, g11 ), 
         x12 = ifelse(is.na(g12), 0, g12 ))

tbl.enrollment2017_hs <- tbl.enrollment2017_hs %>% 
  mutate( total_hs = x09 + x10 + x11 + x12 ) %>%
  select( -x09:-x12 )
glimpse(tbl.enrollment2017_hs)

# Once again, largest and smallest. Removing lower grades changes the order
# slightly:
tbl.enrollment2017_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(desc(total_hs))

tbl.enrollment2017_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(total_hs)

# Uncomment as needed:
# Two datasets worth saving:
# write_rds(tbl.enrollment2017, "./output/Rds/enrollment2017.Rds")
# write_rds(tbl.enrollment2017_hs, "./output/Rds/enrollment2017_hs.Rds")

# Examine largest and smallest again. Count
# schools by govern type. Calculate aggregate enrollment, average per school, 
# per cent enrollment, etc., by type. 

# A count of schools having enrollments less than 250:
tbl.enrollment2017_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(total_hs) %>% 
  filter(total_hs <=250) %>% 
  group_by(govern) %>% 
  count()



# ##  Will investigate treating schools larger that 2000 (6) and smaller than
# 100 (3) as outliers ##  in some graphs such as histograms.
# 
# # If we simply omit the outlier observations, this is the histogram we get:
tbl.enrollment2017_hs %>% 
  filter( total < 2000) %>% 
  ggplot(., aes(x=total) ) + 
  geom_histogram(color="black", bins=15) +
  facet_wrap(~govern)

# The above is more illuminating with the addition of facet-wrap(~govern)


################################################################
#                                                              #
#	Data for 2006                                                #
#                                                              #
################################################################


# Next, reading in the 2006 data in order to start bracketting the analysis
# period as soon as possible. Notice that the variable set has a number of
# differences, so cannot simply paste the earlier read statement or make it a
# function without allowing for this:

tbl.enrollment2006 <- read_csv("./membership_20th_day_2006.csv", 
                               skip=1, 
                               n_max = 603,
                               col_names = c("area",
                                             "unit",
                                             "school_id",
                                             "common_name",
                                             "headstart",
                                             "other_pk",
                                             "state_pk",
                                             "pk_sped",
                                             "fullday_k",
                                             "halfday_k",
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
                                             "g12",
                                             "total"))
glimpse(tbl.enrollment2006)


# Confirming that the read cutoff of 603 records was correct:
tail(tbl.enrollment2006)


# Creating the year  and govern variables ... as a factor ...
tbl.enrollment2006 <- mutate(tbl.enrollment2006, year=2006, govern="regular")
glimpse(tbl.enrollment2006)


# Reminder: all the schools in this subset are "regular" governance.

# Reading in and transforming the charter portion of the CPS 2006 spreadsheet: 
# Note that in 2006, the only governance category levels in the data are
# "regular" and "charter". The "other" levels, contract etc., only appear in
# later years.

tbl.enrollment2006_chtr <- read_csv("./membership_20th_day_2006.csv", 
                                    skip=607, 
                                    n_max=22,
                                    col_names = c("area",
                                                  "unit",
                                                  "school_id",
                                                  "common_name",
                                                  "headstart",
                                                  "other_pk",
                                                  "state_pk",
                                                  "pk_sped",
                                                  "fullday_k",
                                                  "halfday_k",
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
                                                  "g12",
                                                  "total"))
tail(tbl.enrollment2006_chtr)

# Add year and govern:
tbl.enrollment2006_chtr <- tbl.enrollment2006_chtr %>%
  mutate(year=2006, govern="charter")
glimpse(tbl.enrollment2006_chtr)

head(tbl.enrollment2006_chtr)
tail(tbl.enrollment2006_chtr)

# Here, the two parts of the 2006 roster are combined:
tbl.enrollment2006 <- bind_rows(tbl.enrollment2006, tbl.enrollment2006_chtr)
glimpse(tbl.enrollment2006)


head(tbl.enrollment2006)
tail(tbl.enrollment2006)


tbl.enrollment2006 <- tbl.enrollment2006 %>% 
  mutate(grades = ifelse( !is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                          "high school", "elementary" ) )
glimpse(tbl.enrollment2006)

# First cut at a high school data frame:
tbl.enrollment2006_hs <- tbl.enrollment2006 %>% 
  filter(grades == "high school") %>% 
  select(govern, school_id, common_name, year, total, g09, g10, g11, g12)
glimpse(tbl.enrollment2006_hs)
head(tbl.enrollment2006_hs)
tail(tbl.enrollment2006_hs)

####### Check for and correct misclassified cases like Keller in 2017.

# Identify the g09:g12 "NA" schools.
# Schools having grade 9 missing:
tbl.enrollment2006_hs %>% 
  filter(is.na(g09)) %>% 
  select(common_name)

# Schools having grade 10 missing:
tbl.enrollment2006_hs %>% 
  filter(is.na(g10)) %>% 
  select(common_name)

# Schools having grade 11 missing:
tbl.enrollment2006_hs %>% 
  filter(is.na(g11)) %>% 
  select(common_name)

# Schools having grade 12 missing:
tbl.enrollment2006_hs %>% 
  filter(is.na(g12)) %>% 
  select(common_name)

# Many schools during this time were phasing in and out under Small Schools
# Initiative and similar programs.

# Schools having enrollment only in grade 9 for 2005-06 year:
tbl.na_g101112_2006 <- tbl.enrollment2006_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(common_name, school_id)

# Schools having enrollment only in grade 12 for 2005-06 year:
tbl.na_g091011_2006 <- tbl.enrollment2006_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11)) %>%
  select(common_name, school_id)

# There do not appear to be any classification issues this time.

# Create a total_hs variable:
# Mutate auxilliary variables x09:x12 <- g09:g12
# ifelse( is.na(x09:x12), 0, x09:x12 )
# Total high school is vector sum of x09:x12
# Drop x09:x12

tbl.enrollment2006_hs <- tbl.enrollment2006_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09 ),  
         x10 = ifelse(is.na(g10), 0, g10 ),  
         x11 = ifelse(is.na(g11), 0, g11 ),  
         x12 = ifelse(is.na(g12), 0, g12 ))

tbl.enrollment2006_hs <- tbl.enrollment2006_hs %>% 
  mutate( total_hs = x09 + x10 + x11 + x12 ) %>%
  select( -x09:-x12 )
glimpse(tbl.enrollment2006_hs)

# A check:
tbl.enrollment2006_hs %>%  
  mutate(diff = total - total_hs) %>% 
  select(common_name, diff)
View(tbl.enrollment2006_hs)

# Data summarized:
summary(tbl.enrollment2006_hs)


# Largest and smallest high school enrollments:
tbl.enrollment2006_hs %>% 
  select( govern, common_name, total_hs, year) %>% arrange(desc(total_hs))

tbl.enrollment2006_hs %>% 
  select( govern, common_name, total_hs, year) %>% arrange(total_hs)




# Remark: Youth Connections Charter is treated as a unified school in 2006, but 
# it actually has its enrollment spread over a number of sites throughout the
# city. In 2017 there were 19 YCC locations. In that year's file, they are
# treated as separate. Also, at some point YCC was classed as a Options school
# rather than a Charter, though it retains "Charter" as part of its name.
# 
# Chicago International Charter data for 2006 includes all elementary grades. 
# This school also has multiple locations throughout the city, with 14
# identifiable in the 2017 spreadsheet.


# Using View, we see that 13 schools had a 2006 enrollment of at least 2000
# students. In addition to the top ten, they were Bogan Tech, Morgan Park, and
# CVS. Hyde Park, Julian, Mather, and Young were over 1900.

tbl.enrollment2006_hs %>% 
  select( govern, common_name, total_hs, year) %>% 
  arrange(total_hs) %>% 
  View()


# Eight additional schools were under 300 enrollment in 2006. 
# Three of these schools were charter schools.
#
# The low enrollment for Lindblom is due to the re-opening of the school in the
# 2005-2006 school year, following major renovation of its plant; see Wikipedia,
# "Robert Lindblom..." The slow rebuilding of enrollment at Lindblom suggests
# that reputational factors including the view of the school's neighborhood
# could be relevant.
#
# Bowen HS also reformed, at the beginning of the 2012 school year. Before then,
# it was divided into four separate schools.


# Uncomment as needed:
# Writing the 2006 data to disk:
# write_rds(tbl.enrollment2006, "./output/Rds/enrollment2006.Rds")
# write_rds(tbl.enrollment2006_hs, "./output/Rds/enrollment2006_hs.Rds")





################################################################
#                                                              #
#	Data for 2007                                                #
#                                                              #
################################################################



# Loadiing 2007 data. Its format resembles the 2006 file:
tbl.enrollment2007 <- read_csv("./membership_20th_day_2007.csv", 
                               skip=1, 
                               n_max = 636,
                               col_names = c("area",
                                             "unit",
                                             "school_id",
                                             "common_name",
                                             "headstart",
                                             "other_pk",
                                             "state_pk",
                                             "pk_sped",
                                             "fullday_k",
                                             "halfday_k",
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
                                             "g12",
                                             "total"))
tail(tbl.enrollment2007)
glimpse(tbl.enrollment2007)


# Creating the essential new variables:
tbl.enrollment2007 <- mutate(tbl.enrollment2007, year=2007, govern="regular")
glimpse(tbl.enrollment2007)

# Loading the 2007 charter data:
tbl.enrollment2007_chtr <- read_csv("./membership_20th_day_2007.csv", 
                                    skip=640, 
                                    n_max=46 ,
                                    col_names = c("area",
                                                  "unit",
                                                  "school_id",
                                                  "common_name",
                                                  "headstart",
                                                  "other_pk",
                                                  "state_pk",
                                                  "pk_sped",
                                                  "fullday_k",
                                                  "halfday_k",
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
                                                  "g12",
                                                  "total"))
tbl.enrollment2007_chtr %>% 
  tail()

tbl.enrollment2007_chtr <- tbl.enrollment2007_chtr %>% 
  mutate(year=2007, govern="charter")
glimpse(tbl.enrollment2007_chtr)


tbl.enrollment2007 <- bind_rows(tbl.enrollment2007, tbl.enrollment2007_chtr)

# Checks:
tbl.enrollment2007 %>% head()
tbl.enrollment2007 %>% tail()
tbl.enrollment2007 %>% dim()

# Creating the grades variable:
tbl.enrollment2007 <- tbl.enrollment2007 %>% 
  mutate(grades = 
           ifelse(!is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                   "high school", "elementary"))
glimpse(tbl.enrollment2007)

# Filtering to keep "high school" observations, then selecting only the
# needed variables:
tbl.enrollment2007_hs <- tbl.enrollment2007 %>% filter(grades == "high school")
print(tbl.enrollment2007_hs) # Observations: 131

# The variable network is omitted here, it was not used until 2012.
tbl.enrollment2007_hs <- tbl.enrollment2007_hs %>% 
  select(govern, school_id, common_name, year, total, g09, g10, g11, g12)
glimpse(tbl.enrollment2007_hs) 
dim(tbl.enrollment2007_hs)
summary( tbl.enrollment2007_hs )

# Identify the g09:g12 "NA" schools.
#
# Schools having grade 9 missing:
tbl.enrollment2007_hs %>% 
  filter(is.na(g09)) %>% 
  select(common_name)

# Schools having grade 10 missing:
tbl.enrollment2007_hs %>% 
  filter(is.na(g10)) %>% 
  select(common_name)

# Schools having grade 11 missing:
tbl.enrollment2007_hs %>% 
  filter(is.na(g11)) %>% 
  select(common_name)

# Schools having grade 12 missing:
tbl.enrollment2007_hs %>% 
  filter(is.na(g12)) %>% 
  select(common_name) %>% 
  print(n=100)

# Schools having enrollment only in grade 9 for 2006-07 year:
tbl.na_g101112_2007 <- tbl.enrollment2007_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(common_name, school_id)


# Schools having enrollment only in grade 12 for 2006-07 year:
tbl.na_g091011_2007 <- tbl.enrollment2007_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11)) %>%
  select(common_name, school_id)

# Adding a total_hs variable to the datasets, and rechecking the largest and
# smallest: Create a total_hs variable: Mutate auxilliary variables x09:x12 <-
# g09:g12 ifelse( is.na(x09:x12), 0, x09:x12 ) Total high school is vector sum
# of x09:x12 Drop x09:x12

tbl.enrollment2007_hs <- tbl.enrollment2007_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09),  
         x10 = ifelse(is.na(g10), 0, g10),  
         x11 = ifelse(is.na(g11), 0, g11),  
         x12 = ifelse(is.na(g12), 0, g12))

tbl.enrollment2007_hs <- tbl.enrollment2007_hs %>% 
  mutate(total_hs = x09 + x10 + x11 + x12) %>%
  select( -x09:-x12)
glimpse(tbl.enrollment2007_hs)

# Largest and smallest high school enrollments:
tbl.enrollment2007_hs %>% 
  select(govern, common_name, total_hs, year) %>% 
  arrange(desc(total_hs))

tbl.enrollment2007_hs %>% 
  select(govern, common_name, total_hs, year) %>% 
  arrange(total_hs)

# Summary stats:
summary(tbl.enrollment2007_hs)

# Uncomment as needed:
# Writing the 2007 data to disk:
# write_rds(tbl.enrollment2007, "./output/Rds/enrollment2007.Rds")
# write_rds(tbl.enrollment2007_hs, "./output/Rds/enrollment2007_hs.Rds")










################################################################
#                                                              #
#	Data for 2008                                                #
#                                                              #
################################################################



# Loading the 2008 main data:
tbl.enrollment2008 <- read_csv("./membership_20th_day_2008.csv", 
                               skip=1, 
                               n_max=632,
                               col_names = c("area",
                                             "unit",
                                             "school_id",
                                             "common_name",
                                             "pe",
                                             "pk",
                                             "fullday_k",
                                             "halfday_k",
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
                                             "g12",
                                             "total"))


head(tbl.enrollment2008)
tail(tbl.enrollment2008)
glimpse(tbl.enrollment2008)

# Creating the year and govern variables:
tbl.enrollment2008 <- mutate(tbl.enrollment2008, year=2008,  govern="regular")

glimpse(tbl.enrollment2008)


# Loading the 2008 charter data:
tbl.enrollment2008_chtr <- read_csv("./membership_20th_day_2008.csv",
                                    skip=636, 
                                    n_max=60,
                                    col_names = c("area",
                                                  "unit",
                                                  "school_id",
                                                  "common_name",
                                                  "pe",
                                                  "pk",
                                                  "fullday_k",
                                                  "halfday_k",
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
                                                  "g12",
                                                  "total"))

head(tbl.enrollment2008_chtr)
tail(tbl.enrollment2008_chtr)

glimpse(tbl.enrollment2008_chtr)

# Creating the year and govern variables:
tbl.enrollment2008_chtr <- tbl.enrollment2008_chtr %>% 
  mutate(year=2008, govern="charter")
glimpse(tbl.enrollment2008_chtr)


# Binding the two parts of the 2008 roster together:
tbl.enrollment2008 <- bind_rows(tbl.enrollment2008, tbl.enrollment2008_chtr)
head(tbl.enrollment2008)
tail(tbl.enrollment2008)
glimpse(tbl.enrollment2008)

# Creating the grades variable:
tbl.enrollment2008 <- tbl.enrollment2008 %>% 
  mutate(grades = ifelse( !is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                          "high school", 
                          "elementary" ))
glimpse(tbl.enrollment2008)



# Filtering to keep "high school" observations, then selecting only the
# needed variables:
tbl.enrollment2008_hs <- tbl.enrollment2008 %>% 
  filter(grades == "high school")
glimpse(tbl.enrollment2008_hs)
print(tbl.enrollment2008_hs) 

# The variable network is omitted here, it was not used until 2012.
tbl.enrollment2008_hs <- tbl.enrollment2008_hs %>% 
  select(govern, school_id, common_name, year, total, g09, g10, g11, g12)
glimpse(tbl.enrollment2008_hs)

dim(tbl.enrollment2008_hs)  
summary(tbl.enrollment2008_hs)





# There are 2 schools that have no school_id in this dataset:
# 
filter(tbl.enrollment2008_hs, is.na(school_id))

# Identify the g09:g12 "NA" schools.
# Schools having grade 9 missing:
tbl.enrollment2008_hs %>% 
  filter(is.na(g09)) %>% 
  select(common_name)

tbl.enrollment2008_hs %>% 
  filter(is.na(g10)) %>% 
  select(common_name)

tbl.enrollment2008_hs %>% 
  filter(is.na(g11)) %>% 
  select(common_name)

tbl.enrollment2008_hs %>% 
  filter(is.na(g12)) %>% 
  select(common_name)



# Schools having enrollment only in grade 9:
tbl.na_g101112_2008 <- tbl.enrollment2008_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(common_name, school_id)
tbl.na_g101112_2008

# Schools having enrollment only in grade 12:
tbl.na_g091011_2008 <- tbl.enrollment2008_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11)) %>%
  select(common_name, school_id)
tbl.na_g091011_2008

# Adding a total_hs variable to the datasets, and checking the largest and
# smallest: 
# Create a total_hs variable: 
# Mutate auxilliary variables x09:x12 <- g09:g12 
# ifelse( is.na(x09:x12), 0, x09:x12 ) 
# Total high school is vector sum of x09:x12 
# Drop x09:x12

tbl.enrollment2008_hs <- tbl.enrollment2008_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09),  
         x10 = ifelse(is.na(g10), 0, g10),  
         x11 = ifelse(is.na(g11), 0, g11),  
         x12 = ifelse(is.na(g12), 0, g12))

tbl.enrollment2008_hs <- tbl.enrollment2008_hs %>% 
  mutate(total_hs = x09 + x10 + x11 + x12) %>%
  select( -x09:-x12)
glimpse(tbl.enrollment2008_hs)

# West Pullman school has 1 enrolled in g09, so it will be dropped from the high
# school sample as an elementary school.
tbl.enrollment2008_hs %>% 
  filter( str_detect(common_name, "Pullman"))
tbl.drop2008 <- tbl.enrollment2008_hs %>% 
  filter( str_detect(common_name, "Pullman"))
tbl.enrollment2008_hs <- tbl.enrollment2008_hs %>% 
  filter( !(school_id %in% tbl.drop2008$school_id) )
dim(tbl.enrollment2008_hs)    # 142  10

# Largest and smallest high school enrollments:
tbl.enrollment2008_hs %>% 
  select( govern, common_name, total_hs, year) %>% 
  arrange(desc(total_hs))


tbl.enrollment2008_hs %>% 
  select( govern, common_name, total_hs, year) %>% 
  arrange(total_hs)



# Summary stats:
summary(tbl.enrollment2008_hs)

# Uncomment as needed:
# Writing the 2008 data to disk:
# write_rds(tbl.enrollment2008, "./output/Rds/enrollment2008.Rds")
# write_rds(tbl.enrollment2008_hs, "./output/Rds/enrollment2008_hs.Rds")




################################################################
#                                                              #
#	Data for 2009                                                #
#                                                              #
################################################################




# Loading the 2009 main data:
tbl.enrollment2009 <- read_csv("./membership_20th_day_2009.csv", 
                               skip=1, 
                               n_max=588,
                               col_names = c("area",
                                             "unit",
                                             "school_id",
                                             "school_name",
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
                                             "g12",
                                             "k",
                                             "pe",
                                             "pk",
                                             "total"))


head(tbl.enrollment2009)
tail(tbl.enrollment2009)
glimpse(tbl.enrollment2009)

# Creating the year and govern variables:
tbl.enrollment2009 <- mutate(tbl.enrollment2009, year=2009, govern="regular")

glimpse(tbl.enrollment2009)


# Loading the 2009 charter data:
tbl.enrollment2009_chtr <- read_csv("./membership_20th_day_2009.csv", 
                                    skip=592, 
                                    n_max=78,
                                    col_names = c("area",
                                                  "unit",
                                                  "school_id",
                                                  "school_name",
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
                                                  "g12",
                                                  "k",
                                                  "pe",
                                                  "pk",
                                                  "total"))

head(tbl.enrollment2009_chtr)
tail(tbl.enrollment2009_chtr)

glimpse(tbl.enrollment2009_chtr)

# Creating the year and govern variables:
tbl.enrollment2009_chtr <- mutate(tbl.enrollment2009_chtr, 
                                  year=2009,  
                                  govern="charter")
glimpse(tbl.enrollment2009_chtr)


# Binding the two parts of the 2009 roster together:
tbl.enrollment2009 <- bind_rows(tbl.enrollment2009, tbl.enrollment2009_chtr)
head(tbl.enrollment2009)
tail(tbl.enrollment2009)
glimpse(tbl.enrollment2009)

# Creating the grades variable:
tbl.enrollment2009 <- tbl.enrollment2009 %>% 
  mutate(grades = 
           ifelse( !is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                   "high school", 
                   "elementary"))
glimpse(tbl.enrollment2009)



# Filtering to keep "high school" observations, then selecting only the
# needed variables:
tbl.enrollment2009_hs <- tbl.enrollment2009 %>% 
  filter(grades == "high school")
glimpse(tbl.enrollment2009_hs)
print(tbl.enrollment2009_hs)

# The variable network is omitted here, it was not used until 2012.
tbl.enrollment2009_hs <- tbl.enrollment2009_hs %>% 
  select(govern, school_id, school_name, year, total, g09, g10, g11, g12)
glimpse(tbl.enrollment2009_hs)

dim(tbl.enrollment2009_hs)  
summary(tbl.enrollment2009_hs)

# Identify the g09:g12 "NA" schools.
# Schools having grade 9, 10, 11, or 12 missing:
tbl.enrollment2009_hs %>% filter(is.na(g09)) %>% select(school_name)

tbl.enrollment2009_hs %>% filter(is.na(g10)) %>% select(school_name)

tbl.enrollment2009_hs %>% filter(is.na(g11)) %>% select(school_name)

tbl.enrollment2009_hs %>% filter(is.na(g12)) %>% select(school_name)



# Schools having enrollment only in grade 9:
tbl.na_g101112_2009 <- tbl.enrollment2009_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(school_name, school_id)
tbl.na_g101112_2009

# Schools having enrollment only in grade 12:
tbl.na_g091011_2009 <- tbl.enrollment2009_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11)) %>%
  select(school_name, school_id)
tbl.na_g091011_2009

# Adding a total_hs variable to the datasets, and rechecking 
# the largest and smallest:
# Create a total_hs variable:
# Mutate auxilliary variables x09:x12 <- g09:g12
# ifelse( is.na(x09:x12), 0, x09:x12 )
# Total high school is vector sum of x09:x12
# Drop x09:x12

tbl.enrollment2009_hs <- tbl.enrollment2009_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09),  
         x10 = ifelse(is.na(g10), 0, g10),  
         x11 = ifelse(is.na(g11), 0, g11),  
         x12 = ifelse(is.na(g12), 0, g12))
glimpse(tbl.enrollment2009_hs)

tbl.enrollment2009_hs <- tbl.enrollment2009_hs %>% 
  mutate(total_hs = x09 + x10 + x11 + x12) %>%
  select(-x09:-x12)
glimpse(tbl.enrollment2009_hs)


# Largest and smallest high school enrollments:
tbl.enrollment2009_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(desc(total_hs))

tbl.enrollment2009_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(total_hs)



# Summary stats:
summary(tbl.enrollment2009_hs)


## Uncomment as needed:
# Writing the 2009 data to disk:
# write_rds(tbl.enrollment2009, "./output/Rds/enrollment2009.Rds")
# write_rds(tbl.enrollment2009_hs, "./output/Rds/enrollment2009_hs.Rds")










################################################################
#                                                              #
#	Data for 2010                                                #
#                                                              #
################################################################




# Loading the 2010 main data:
tbl.enrollment2010 <- read_csv("./membership_20th_day_2010.csv", 
                               skip=1, 
                               n_max=633,
                               col_names = c("area",
                                             "school_id",
                                             "unit",
                                             "school_name",
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
                                             "g12",
                                             "total"))


head(tbl.enrollment2010)
tail(tbl.enrollment2010)
glimpse(tbl.enrollment2010)

# Creating the year and govern variables:
tbl.enrollment2010 <- mutate(tbl.enrollment2010, 
                             year=2010, 
                             govern="regular")

glimpse(tbl.enrollment2010)


# Loading the 2010 charter data:
tbl.enrollment2010_chtr <- read_csv("./membership_20th_day_2010.csv", 
                                    skip=636, 
                                    n_max=60,
                                    col_names = c("area",
                                                  "school_id",
                                                  "unit",
                                                  "school_name",
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
                                                  "g12",
                                                  "total"))

head(tbl.enrollment2010_chtr)
tail(tbl.enrollment2010_chtr)

glimpse(tbl.enrollment2010_chtr)

# Creating the year and govern variables:
tbl.enrollment2010_chtr <- mutate(tbl.enrollment2010_chtr, 
                                  year=2010,  
                                  govern="charter")
glimpse(tbl.enrollment2010_chtr)


# Binding the two parts of the 2010 roster together:
tbl.enrollment2010 <- bind_rows(tbl.enrollment2010, tbl.enrollment2010_chtr)
head(tbl.enrollment2010)
tail(tbl.enrollment2010)
glimpse(tbl.enrollment2010)

# Creating the grades variable:
tbl.enrollment2010 <- tbl.enrollment2010 %>% 
  mutate(grades = 
           ifelse(!is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                   "high school", 
                   "elementary"))
glimpse(tbl.enrollment2010)



# Filtering to keep "high school" observations, then selecting only the
# needed variables:
tbl.enrollment2010_hs <- tbl.enrollment2010 %>% 
  filter(grades == "high school")
print(tbl.enrollment2010_hs)

# The variable network is omitted here, it was not used until 2012.
tbl.enrollment2010_hs <- tbl.enrollment2010_hs %>% 
  select(govern, school_id, school_name, year, total, g09, g10, g11, g12 )
glimpse(tbl.enrollment2010_hs)

summary(tbl.enrollment2010_hs)

# Identify the g09:g12 "NA" schools.
# Schools having grade 9, 10, 11, or 12 missing:
tbl.enrollment2010_hs %>% 
  filter(is.na(g09)) %>% 
  select(school_name)
tbl.enrollment2010_hs %>% 
  filter(is.na(g10)) %>% 
  select(school_name)
tbl.enrollment2010_hs %>% 
  filter(is.na(g11)) %>% 
  select(school_name) %>% 
  print(n=100)
tbl.enrollment2010_hs %>% 
  filter(is.na(g12)) %>% 
  select(school_name) %>% 
  print(n=100)


# Schools having enrollment only in grade 9:
tbl.na_g101112_2010 <- tbl.enrollment2010_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(school_name, school_id)
tbl.na_g101112_2010 

# Schools having enrollment only in grade 12:
tbl.na_g091011_2010 <- tbl.enrollment2010_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11)) %>%
  select(school_name, school_id)
tbl.na_g091011_2010


######### Mis-classified elementary schools
## Use similar as needed:
tbl.drop2010 <- tbl.na_g101112_2010 %>% 
  filter(str_detect(school_name, "Nathaniel Pope") | 
           str_detect(school_name, "David Kohn") | 
           str_detect(school_name, "Burnside")  ) %>% 
  select(school_id)
tbl.drop2010

#> # Burnside, Kohn, and Pope are elementary schools. As of 2017 only Burnside
# is still open.
tbl.enrollment2010_hs <- 
  filter(tbl.enrollment2010_hs, !(school_id %in% tbl.drop2010$school_id))
# Correcting the grades observations for those schools in the main 2010 dataset:
tbl.enrollment2010 <- tbl.enrollment2010 %>% 
     mutate(grades = 
              ifelse( school_id %in% tbl.drop2010$school_id, 
                      "elementary", 
                      grades))

dim(tbl.enrollment2010_hs)


# Adding a total_hs variable to the datasets, and rechecking 
# the largest and smallest:
# Create a total_hs variable:
# Mutate auxilliary variables x09:x12 <- g09:g12
# ifelse( is.na(x09:x12), 0, x09:x12 )
# Total high school is vector sum of x09:x12
# Drop x09:x12

tbl.enrollment2010_hs <- tbl.enrollment2010_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09 ),  
         x10 = ifelse(is.na(g10), 0, g10 ),  
         x11 = ifelse(is.na(g11), 0, g11 ),  
         x12 = ifelse(is.na(g12), 0, g12 ))
glimpse(tbl.enrollment2010_hs)

tbl.enrollment2010_hs <- tbl.enrollment2010_hs %>% 
  mutate( total_hs = x09 + x10 + x11 + x12 ) %>%
  select( -x09:-x12 )
glimpse(tbl.enrollment2010_hs)


# Largest and smallest high school enrollments:
tbl.enrollment2010_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(desc(total_hs))
tbl.enrollment2010_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(total_hs)

# Summary stats:
summary(tbl.enrollment2010_hs)


# Uncomment as needed:
# Writing the 2010 data to disk:
# write_rds(tbl.enrollment2010, "./output/Rds/enrollment2010.Rds")
# write_rds(tbl.enrollment2010_hs, "./output/Rds/enrollment2010_hs.Rds")










################################################################
#                                                              #
#	Data for 2011                                                #
#                                                              #
################################################################




# Loading the 2011 main data:
tbl.enrollment2011 <- read_csv("./membership_20th_day_2011.csv", 
                               skip=1, 
                               n_max=633,
                               col_names = c("area",
                                             "school_id",
                                             "unit",
                                             "school_name",
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
                                             "g12",
                                             "99",
                                             "total"))


head(tbl.enrollment2011)
tail(tbl.enrollment2011)
glimpse(tbl.enrollment2011)

# Creating the year and govern variables:
tbl.enrollment2011 <- mutate(tbl.enrollment2011, year=2011,  govern="regular")

glimpse(tbl.enrollment2011)


# Loading the 2011 charter data:
tbl.enrollment2011_chtr <- read_csv("./membership_20th_day_2011.csv", 
                                    skip=636, 
                                    n_max=60,
                                    col_names = c("area",
                                                  "school_id",
                                                  "unit",
                                                  "school_name",
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
                                                  "g12",
                                                  "99",
                                                  "total"))

head(tbl.enrollment2011_chtr)
tail(tbl.enrollment2011_chtr)

glimpse(tbl.enrollment2011_chtr)

# Creating the year and govern variables:
tbl.enrollment2011_chtr <- tbl.enrollment2011_chtr %>% 
  mutate(year=2011,  govern="charter")
glimpse(tbl.enrollment2011_chtr)


# Binding the two parts of the 2011 roster together:
tbl.enrollment2011 <- bind_rows(tbl.enrollment2011, tbl.enrollment2011_chtr)
head(tbl.enrollment2011)
tail(tbl.enrollment2011)
glimpse(tbl.enrollment2011)

# Creating the grades variable:
tbl.enrollment2011 <- tbl.enrollment2011 %>% 
  mutate(grades = ifelse(!is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                          "high school", 
                          "elementary"))
glimpse(tbl.enrollment2011)



# Filtering to keep "high school" observations, then selecting only the
# needed variables:
tbl.enrollment2011_hs <- tbl.enrollment2011 %>% filter(grades == "high school")
glimpse(tbl.enrollment2011_hs)
print(tbl.enrollment2011_hs)

# The variable network is omitted here, it was not used until 2012.
tbl.enrollment2011_hs <- tbl.enrollment2011_hs %>% 
  select(govern, school_id, school_name, year, total, g09, g10, g11, g12)
glimpse(tbl.enrollment2011_hs)

summary(tbl.enrollment2011_hs)

# Identify the g09:g12 "NA" schools.
tbl.enrollment2011_hs %>% filter(is.na(g09)) %>% select(school_name)
tbl.enrollment2011_hs %>% filter(is.na(g10)) %>% select(school_name)
tbl.enrollment2011_hs %>% filter(is.na(g11)) %>% select(school_name)
tbl.enrollment2011_hs %>% filter(is.na(g12)) %>% select(school_name)


# Schools having enrollment only in grade 9:
tbl.na_g101112_2011 <- tbl.enrollment2011_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(school_name, school_id)
tbl.na_g101112_2011

# Schools having enrollment only in grade 12:
tbl.na_g091011_2011 <- tbl.enrollment2011_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11)) %>%
  select(school_name, school_id)
tbl.na_g091011_2011

(tbl.drop2011 <- tbl.na_g101112_2011 %>% 
    filter(str_detect(school_name, "Goldblatt") | 
             str_detect(school_name, "Clara Barton") | 
             str_detect(school_name, "Perspectives Charter")) %>% 
    select(school_id) )
#> tbl.drop2011
tbl.enrollment2011_hs <- 
  filter(tbl.enrollment2011_hs, !(school_id %in% tbl.drop2011$school_id))
#> # Correcting the grades observations for those schools in the main 2011 dataset:
dim(tbl.enrollment2011_hs)

tbl.enrollment2011 <- tbl.enrollment2011 %>% 
    mutate(grades  = ifelse( school_id %in% tbl.drop2011$school_id, 
                             "elementary", 
                             grades))



# Adding a total_hs variable to the datasets, and rechecking 
# the largest and smallest:
# Create a total_hs variable:
# Mutate auxilliary variables x09:x12 <- g09:g12
# ifelse( is.na(x09:x12), 0, x09:x12 )
# Total high school is vector sum of x09:x12
# Drop x09:x12

tbl.enrollment2011_hs <- tbl.enrollment2011_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09 ),  
         x10 = ifelse(is.na(g10), 0, g10 ),  
         x11 = ifelse(is.na(g11), 0, g11 ),  
         x12 = ifelse(is.na(g12), 0, g12 ))
glimpse(tbl.enrollment2011_hs)

tbl.enrollment2011_hs <- tbl.enrollment2011_hs %>% 
  mutate( total_hs = x09 + x10 + x11 + x12 ) %>%
  select(-x09:-x12)
glimpse(tbl.enrollment2011_hs)


# Largest and smallest high school enrollments:
tbl.enrollment2011_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(desc(total_hs))


tbl.enrollment2011_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(total_hs)



# Summary stats:
summary(tbl.enrollment2011_hs)


# Uncomment as needed:
# Writing the 2011 data to disk:
# write_rds(tbl.enrollment2011, "./output/Rds/enrollment2011.Rds")
# write_rds(tbl.enrollment2011_hs, "./output/Rds/enrollment2011_hs.Rds")








################################################################
#                                                              #
#	Data for 2012                                                #
#                                                              #
################################################################




# Loading the 2012 main data:
tbl.enrollment2012 <- read_csv("./membership_20th_day_2012.csv", 
                               skip=1, 
                               n_max=633,
                               col_names = c("network",
                                             "school_id",
                                             "unit",
                                             "school_name",
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
                                             "g12",
                                             "total"))


head(tbl.enrollment2012)
tail(tbl.enrollment2012)
glimpse(tbl.enrollment2012)

# Creating the year and govern variables:
tbl.enrollment2012 <- mutate(tbl.enrollment2012, year=2012,  govern="regular")

glimpse(tbl.enrollment2012)


# Loading the 2012 charter data:
tbl.enrollment2012_chtr <- read_csv("./membership_20th_day_2012.csv", 
                                    skip=636, 
                                    n_max=60, 
                                    col_names = c("network",
                                                  "school_id",
                                                  "unit",
                                                  "school_name",
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
                                                  "g12",
                                                  "total"))

head(tbl.enrollment2012_chtr)
tail(tbl.enrollment2012_chtr)

glimpse(tbl.enrollment2012_chtr)

# Creating the year and govern variables:
tbl.enrollment2012_chtr <- tbl.enrollment2012_chtr %>% 
  mutate(year=2012,  govern="charter")
glimpse(tbl.enrollment2012_chtr)


# Binding the two parts of the 2012 roster together:
tbl.enrollment2012 <- bind_rows(tbl.enrollment2012, tbl.enrollment2012_chtr)
head(tbl.enrollment2012)
tail(tbl.enrollment2012)
glimpse(tbl.enrollment2012)

# Creating the grades variable:
tbl.enrollment2012 <- tbl.enrollment2012 %>% 
  mutate(grades = ifelse( !is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                          "high school", 
                          "elementary" ))
glimpse(tbl.enrollment2012)



# Filtering to keep "high school" observations, then selecting only the
# needed variables:
tbl.enrollment2012_hs <- tbl.enrollment2012 %>% 
  filter(grades == "high school")
glimpse(tbl.enrollment2012_hs)
print(tbl.enrollment2012_hs)

# The variable network is omitted here, it was not used until 2012.
tbl.enrollment2012_hs <- tbl.enrollment2012_hs %>% 
  select(govern, school_id, school_name, year, total, g09, g10, g11, g12 )
glimpse(tbl.enrollment2012_hs)
summary(tbl.enrollment2012_hs)

# Identify the g09:g12 "NA" schools.
# Schools having grade 9 (10, 11, 12) missing:
tbl.enrollment2012_hs %>% filter(is.na(g09)) %>% select(school_name)
tbl.enrollment2012_hs %>% filter(is.na(g10)) %>% select(school_name)
tbl.enrollment2012_hs %>% filter(is.na(g11)) %>% select(school_name)
tbl.enrollment2012_hs %>% filter(is.na(g12)) %>% select(school_name)


# Schools having enrollment only in grade 9:
tbl.na_g101112_2012 <- tbl.enrollment2012_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(school_name, school_id)
tbl.na_g101112_2012

# Schools having enrollment only in grade 12:
tbl.na_g091011_2012 <- tbl.enrollment2012_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11)) %>%
  select(school_name, school_id)
tbl.na_g091011_2012

# Missclassified: James Weldon Johnson, Ida B. Wells, and Paul Revere are
# elementary schools.
(tbl.drop2012 <- tbl.na_g101112_2012 %>% 
    filter(str_detect(school_name, "Weldon") | 
             str_detect(school_name, "Ida B Wells") | 
             str_detect(school_name, "Revere")) %>% 
    select(school_id) )
# Correcting the hs data:
tbl.enrollment2012_hs <- 
  filter(tbl.enrollment2012_hs, !(school_id %in% tbl.drop2012$school_id))
# Correcting the grades observations for those schools in the main 2012 dataset:
tbl.enrollment2012 <- tbl.enrollment2012 %>% 
   mutate( grades  = ifelse(school_id %in% tbl.drop2012$school_id, 
                            "elementary", 
                            grades))
dim(tbl.enrollment2012_hs)


# Adding a total_hs variable to the datasets, and rechecking 
# the largest and smallest:
# Create a total_hs variable:
# Mutate auxilliary variables x09:x12 <- g09:g12
# ifelse( is.na(x09:x12), 0, x09:x12 )
# Total high school is vector sum of x09:x12
# Drop x09:x12

tbl.enrollment2012_hs <- tbl.enrollment2012_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09 ),  
         x10 = ifelse(is.na(g10), 0, g10 ),  
         x11 = ifelse(is.na(g11), 0, g11 ),  
         x12 = ifelse(is.na(g12), 0, g12 ))
glimpse(tbl.enrollment2012_hs)

tbl.enrollment2012_hs <- tbl.enrollment2012_hs %>% 
  mutate( total_hs = x09 + x10 + x11 + x12 ) %>%
  select(-x09:-x12)
glimpse(tbl.enrollment2012_hs)


# Largest and smallest high school enrollments:
tbl.enrollment2012_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(desc(total_hs))


tbl.enrollment2012_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(total_hs)

# Summary stats:
summary(tbl.enrollment2012_hs)


# Uncomment as needed:
# Writing the 2012 data to disk:
# write_rds(tbl.enrollment2012, "./output/Rds/enrollment2012.Rds")
# write_rds(tbl.enrollment2012_hs, "./output/Rds/enrollment2012_hs.Rds")




################################################################
#                                                              #
#	Data for 2013                                                #
#                                                              #
################################################################



#Loading the 2013 main data: ## NB: This file came in from excel with a utf-16
#encoding. This was unparsable by read_csv(), but the problem was fixed by
#changing the encoding in the vim editor.
tbl.enrollment2013 <- read_csv("./enrollment_20th_day_2013.csv", 
                               skip=1, 
                               n_max=578, 
                               col_names = c("network",
                                             "school_id",
                                             "school_name",
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
                                             "g12",
                                             "total"))


head(tbl.enrollment2013)
tail(tbl.enrollment2013)
glimpse(tbl.enrollment2013)


# Creating the year and govern variables:
tbl.enrollment2013 <- mutate(tbl.enrollment2013, year=2013,  govern="regular")

glimpse(tbl.enrollment2013)


# Loading the 2013 charter data:
tbl.enrollment2013_chtr <- read_csv("./enrollment_20th_day_2013.csv", 
                                    skip=582, 
                                    n_max=103, 
                                    col_names = c("network",
                                                  "school_id",
                                                  "school_name",
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
                                                  "g12",
                                                  "total"))

head(tbl.enrollment2013_chtr)
tail(tbl.enrollment2013_chtr)

glimpse(tbl.enrollment2013_chtr)

# Creating the year and govern variables:
tbl.enrollment2013_chtr <- tbl.enrollment2013_chtr %>% 
  mutate(year=2013,  govern="charter")
glimpse(tbl.enrollment2013_chtr)


# Binding the two parts of the 2013 roster together:
tbl.enrollment2013 <- bind_rows(tbl.enrollment2013, tbl.enrollment2013_chtr)
head(tbl.enrollment2013)
tail(tbl.enrollment2013)
glimpse(tbl.enrollment2013)

# Creating the grades variable:
tbl.enrollment2013 <- tbl.enrollment2013 %>% 
  mutate(grades = ifelse( !is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                          "high school", 
                          "elementary" ))
glimpse(tbl.enrollment2013)



# Filtering to keep "high school" observations, then selecting only the
# needed variables:
tbl.enrollment2013_hs <- tbl.enrollment2013 %>% filter(grades == "high school")
glimpse(tbl.enrollment2013_hs) 
print(tbl.enrollment2013_hs) 

# The variable network is omitted here, it was not used until 2013.
tbl.enrollment2013_hs <- tbl.enrollment2013_hs %>% 
  select(govern, school_id, school_name, year, total, g09, g10, g11, g12 )
glimpse(tbl.enrollment2013_hs)

dim(tbl.enrollment2013_hs)  
summary(tbl.enrollment2013_hs)

# Identify the g09:g12 "NA" schools.
# Schools having grade 9 (10, 11, 12) missing:
tbl.enrollment2013_hs %>% filter(is.na(g09)) %>% select(school_name)
tbl.enrollment2013_hs %>% filter(is.na(g10)) %>% select(school_name)
tbl.enrollment2013_hs %>% filter(is.na(g11)) %>% select(school_name)
tbl.enrollment2013_hs %>% filter(is.na(g12)) %>% select(school_name)


# Schools having enrollment only in grade 9:
tbl.na_g101112_2013 <- tbl.enrollment2013_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(school_name, school_id)
tbl.na_g101112_2013

# Schools having enrollment only in grade 12:
tbl.na_g091011_2013 <- tbl.enrollment2013_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11)) %>%
  select(school_name, school_id)
tbl.na_g091011_2013

###### Adapt and use as needed:
# Missclassified: Lawndale Elementary and Eli Whitney are elementary schools.
(tbl.drop2013 <- tbl.na_g101112_2013 %>% 
    filter(str_detect(school_name, "Lawndale Elementary") | 
             str_detect(school_name, "Eli Whitney")) %>% 
    select(school_id) )
# Correcting the hs data:
tbl.enrollment2013_hs <- 
  filter(tbl.enrollment2013_hs, !(school_id %in% tbl.drop2013$school_id))
# Correcting the grades observations for those schools in the main 2013 dataset:
tbl.enrollment2013 <- tbl.enrollment2013 %>% 
   mutate(grades  = 
            ifelse(school_id %in% tbl.drop2013$school_id, "elementary", grades))
dim(tbl.enrollment2013_hs)





# Adding a total_hs variable to the datasets, and rechecking 
# the largest and smallest:
# Create a total_hs variable:
# Mutate auxilliary variables x09:x12 <- g09:g12
# ifelse( is.na(x09:x12), 0, x09:x12 )
# Total high school is vector sum of x09:x12
# Drop x09:x12

tbl.enrollment2013_hs <- tbl.enrollment2013_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09 ),  
         x10 = ifelse(is.na(g10), 0, g10 ),  
         x11 = ifelse(is.na(g11), 0, g11 ),  
         x12 = ifelse(is.na(g12), 0, g12 ))
glimpse(tbl.enrollment2013_hs)

tbl.enrollment2013_hs <- tbl.enrollment2013_hs %>% 
  mutate( total_hs = x09 + x10 + x11 + x12 ) %>%
  select( -x09:-x12)
glimpse(tbl.enrollment2013_hs)


# Largest and smallest high school enrollments:
tbl.enrollment2013_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(desc(total_hs))


tbl.enrollment2013_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(total_hs)



# Summary stats:
summary(tbl.enrollment2013_hs)


# Uncomment as needed:
# Writing the 2013 data to disk:
# write_rds(tbl.enrollment2013, "./output/Rds/enrollment2013.Rds")
# write_rds(tbl.enrollment2013_hs, "./output/Rds/enrollment2013_hs.Rds")




################################################################
#                                                              #
#	Data for 2014                                                #
#                                                              #
################################################################




# Loading the 2014 main data:
tbl.enrollment2014 <- read_csv("./enrollment_20th_day_2014.csv", 
                               skip=1, 
                               n_max=539, 
                               col_names = c("network",
                                             "school_id",
                                             "school_name",
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
                                             "g12",
                                             "total"))


head(tbl.enrollment2014)
tail(tbl.enrollment2014)


# Creating the year and govern variables:
tbl.enrollment2014 <- mutate(tbl.enrollment2014, year=2014,  govern="regular")
glimpse(tbl.enrollment2014)

# Loading the 2014 charter data:
tbl.enrollment2014_chtr <- read_csv("./enrollment_20th_day_2014.csv", 
                                    skip=543, 
                                    n_max=133, 
                                    col_names = c("network",
                                                  "school_id",
                                                  "school_name",
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
                                                  "g12",
                                                  "total"))

head(tbl.enrollment2014_chtr)

tail(tbl.enrollment2014_chtr)


# Creating the year and govern variables:
tbl.enrollment2014_chtr <- tbl.enrollment2014_chtr %>% 
  mutate(year=2014,  govern="charter")
glimpse(tbl.enrollment2014_chtr)

# Binding the two parts of the 2014 roster together:
tbl.enrollment2014 <- bind_rows(tbl.enrollment2014, tbl.enrollment2014_chtr)
head(tbl.enrollment2014)
tail(tbl.enrollment2014)
glimpse(tbl.enrollment2014)

# Creating the grades variable:
tbl.enrollment2014 <- tbl.enrollment2014 %>% 
  mutate(grades = ifelse( !is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                          "high school", 
                          "elementary"))
glimpse(tbl.enrollment2014)

# Filtering to keep "high school" observations, then selecting only the
# needed variables:
tbl.enrollment2014_hs <- tbl.enrollment2014 %>% filter(grades == "high school")

glimpse(tbl.enrollment2014_hs) # Observations: 
# print(tbl.enrollment2014_hs) # Observations: 185

# The variable network is omitted here, it was not used until 2014.
tbl.enrollment2014_hs <- tbl.enrollment2014_hs %>% 
  select(govern, school_id, school_name, year, total, g09, g10, g11, g12)
glimpse(tbl.enrollment2014_hs)
dim(tbl.enrollment2014_hs)
summary(tbl.enrollment2014_hs)


# Identify the g09:g12 "NA" schools.
# Schools having grade 9 missing:
tbl.enrollment2014_hs %>% filter(is.na(g09)) %>% select(school_name) %>% 
  print(n=100)
tbl.enrollment2014_hs %>% filter(is.na(g10)) %>% select(school_name)
tbl.enrollment2014_hs %>% filter(is.na(g11)) %>% select(school_name)
tbl.enrollment2014_hs %>% filter(is.na(g12)) %>% select(school_name)
# Schools having enrollment only in grade 9:
tbl.na_g101112_2014 <- tbl.enrollment2014_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(school_name, school_id)
tbl.na_g101112_2014
# Schools having enrollment only in grade 12:
tbl.na_g091011_2014 <- tbl.enrollment2014_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11)) %>%
  select(school_name, school_id)
tbl.na_g091011_2014

# Adding a total_hs variable to the datasets, and rechecking 
# the largest and smallest:
# Create a total_hs variable:
# Mutate auxilliary variables x09:x12 <- g09:g12
# ifelse( is.na(x09:x12), 0, x09:x12 )
# Total high school is vector sum of x09:x12
# Drop x09:x12

tbl.enrollment2014_hs <- tbl.enrollment2014_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09 ),  
         x10 = ifelse(is.na(g10), 0, g10 ), 
         x11 = ifelse(is.na(g11), 0, g11 ), 
         x12 = ifelse(is.na(g12), 0, g12 ))

tbl.enrollment2014_hs <- tbl.enrollment2014_hs %>% 
  mutate( total_hs = x09 + x10 + x11 + x12 ) %>%
  select( -x09:-x12)
glimpse(tbl.enrollment2014_hs)


# Largest and smallest high school enrollments:
tbl.enrollment2014_hs %>% 
  select(govern, school_name, total_hs, year) %>% 
  arrange(desc(total_hs))

tbl.enrollment2014_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(total_hs)

# Summary stats:
summary(tbl.enrollment2014_hs)

# Uncomment as needed.
# Writing the 2014 data to disk:
# write_rds(tbl.enrollment2014, "./output/Rds/enrollment2014.Rds")
# write_rds(tbl.enrollment2014_hs, "./output/Rds/enrollment2014_hs.Rds")



################################################################
#                                                              #
#	Data for 2015                                                #
#                                                              #
################################################################




# Loading the 2015 main data:
tbl.enrollment2015 <- read_csv("./enrollment_20th_day_2015.csv", 
                               skip=1, 
                               n_max=537, 
                               col_names = c("network",
                                             "school_id",
                                             "school_name",
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
                                             "g12",
                                             "total"))


head(tbl.enrollment2015)
tail(tbl.enrollment2015)
glimpse(tbl.enrollment2015)


# Creating the year and govern variables:
tbl.enrollment2015 <- mutate(tbl.enrollment2015, year=2015,  govern="regular")



# Loading the 2015 charter data:
tbl.enrollment2015_chtr <- read_csv("./enrollment_20th_day_2015.csv", 
                                    skip=541, 
                                    n_max=141, 
                                    col_names = c("network",
                                                  "school_id",
                                                  "school_name",
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
                                                  "g12",
                                                  "total"))

head(tbl.enrollment2015_chtr)
tail(tbl.enrollment2015_chtr)

# Creating the year and govern variables:
tbl.enrollment2015_chtr <- tbl.enrollment2015_chtr %>% 
  mutate(year=2015,  govern="charter")

# Binding the two parts of the 2015 roster together:
tbl.enrollment2015 <- bind_rows(tbl.enrollment2015, tbl.enrollment2015_chtr)
head(tbl.enrollment2015)
tail(tbl.enrollment2015)
glimpse(tbl.enrollment2015)

# Creating the grades variable:
tbl.enrollment2015 <- tbl.enrollment2015 %>% 
  mutate(grades = ifelse( !is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                          "high school", 
                          "elementary" ))
glimpse(tbl.enrollment2015)


# Filtering to keep "high school" observations, then selecting only the
# needed variables:
tbl.enrollment2015_hs <- tbl.enrollment2015 %>% filter(grades == "high school")

# print(tbl.enrollment2015_hs) # Observations:

# The variable network is omitted here, it was not used until 2015.
tbl.enrollment2015_hs <- tbl.enrollment2015_hs %>% 
  select(govern, school_id, school_name, year, total, g09, g10, g11, g12 )
glimpse(tbl.enrollment2015_hs) # Observations: 

dim(tbl.enrollment2015_hs)
summary(tbl.enrollment2015_hs)


# Identify the g09:g12 "NA" schools.
# Schools having grade 9 missing:
tbl.enrollment2015_hs %>% 
  filter(is.na(g09)) %>% 
  select(school_name) %>% 
  print(n=100)

tbl.enrollment2015_hs %>% filter(is.na(g10)) %>% select(school_name)

tbl.enrollment2015_hs %>% filter(is.na(g11)) %>% select(school_name)

tbl.enrollment2015_hs %>% filter(is.na(g12)) %>% select(school_name)

# Schools having enrollment only in grade 9:
tbl.na_g101112_2015 <- tbl.enrollment2015_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(school_name, school_id)
tbl.na_g101112_2015

# Schools having enrollment only in grade 12:
tbl.na_g091011_2015 <- tbl.enrollment2015_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11)) %>%
  select(school_name, school_id)
tbl.na_g091011_2015




# Adding a total_hs variable to the datasets, and rechecking 
# the largest and smallest:
# Create a total_hs variable:
# Mutate auxilliary variables x09:x12 <- g09:g12
# ifelse( is.na(x09:x12), 0, x09:x12 )
# Total high school is vector sum of x09:x12
# Drop x09:x12

tbl.enrollment2015_hs <- tbl.enrollment2015_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09 ),  
         x10 = ifelse(is.na(g10), 0, g10 ),  
         x11 = ifelse(is.na(g11), 0, g11 ),  
         x12 = ifelse(is.na(g12), 0, g12 ))


tbl.enrollment2015_hs <- tbl.enrollment2015_hs %>% 
  mutate( total_hs = x09 + x10 + x11 + x12 ) %>%
  select(-x09:-x12)
glimpse(tbl.enrollment2015_hs)



# Largest and smallest high school enrollments:
tbl.enrollment2015_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(desc(total_hs))
tbl.enrollment2015_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(total_hs)

# Summary stats:
summary(tbl.enrollment2015_hs)


# Uncomment as needed:
# Writing the 2015 data to disk:
# write_rds(tbl.enrollment2015, "./output/Rds/enrollment2015.Rds")
# write_rds(tbl.enrollment2015_hs, "./output/Rds/enrollment2015_hs.Rds")



################################################################
#                                                              #
#	Data for 2016                                                #
#                                                              #
################################################################





# Loading the 2016 main data:
tbl.enrollment2016 <- read_csv("./enrollment_20th_day_2016.csv", 
                               skip=1, 
                               n_max=567, 
                               col_names = c("network",
                                             "school_id",
                                             "school_name",
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
                                             "g12",
                                             "total"))


head(tbl.enrollment2016)

tail(tbl.enrollment2016)

glimpse(tbl.enrollment2016)
# Creating the year and govern variables:
tbl.enrollment2016 <- mutate(tbl.enrollment2016, year=2016,  govern="regular")

glimpse(tbl.enrollment2016)

# Loading the 2016 charter data:
tbl.enrollment2016_chtr <- read_csv("./enrollment_20th_day_2016.csv", 
                                    skip=571, 
                                    n_max=111, 
                                    col_names = c("network",
                                    "school_id",
                                    "school_name",
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
                                    "g12",
                                    "total"))

head(tbl.enrollment2016_chtr)
tail(tbl.enrollment2016_chtr)

glimpse(tbl.enrollment2016_chtr)

# Creating the year and govern variables:
tbl.enrollment2016_chtr <- tbl.enrollment2016_chtr %>% 
  mutate(year=2016,  govern="charter")

# Binding the two parts of the 2016 roster together:
tbl.enrollment2016 <- bind_rows(tbl.enrollment2016, tbl.enrollment2016_chtr)
head(tbl.enrollment2016)
tail(tbl.enrollment2016)
glimpse(tbl.enrollment2016)

# Creating the grades variable:
tbl.enrollment2016 <- tbl.enrollment2016 %>% 
  mutate(grades = ifelse( !is.na(g09) | !is.na(g10) | !is.na(g11) | !is.na(g12), 
                          "high school", 
                          "elementary" ))
glimpse(tbl.enrollment2016)


# Filtering to keep "high school" observations, then selecting only the
# needed variables:
tbl.enrollment2016_hs <- tbl.enrollment2016 %>% filter(grades == "high school")
glimpse(tbl.enrollment2016_hs)
# print(tbl.enrollment2016_hs)


# The variable network is omitted here, it was not used until 2016.
tbl.enrollment2016_hs <- tbl.enrollment2016_hs %>% 
  select(govern, school_id, school_name, year, total, g09, g10, g11, g12)
glimpse(tbl.enrollment2016_hs)

dim(tbl.enrollment2016_hs)
summary(tbl.enrollment2016_hs)

# Identify the g09:g12 "NA" schools.
# Schools having grades 9, 10, 11, or 12 missing:
tbl.enrollment2016_hs %>% 
  filter(is.na(g09)) %>% 
  select(school_name) %>% 
  print(n=100)

tbl.enrollment2016_hs %>% 
  filter(is.na(g10)) %>% 
  select(school_name)

tbl.enrollment2016_hs %>% 
  filter(is.na(g11)) %>% 
  select(school_name)

tbl.enrollment2016_hs %>% 
  filter(is.na(g12)) %>% 
  select(school_name)



# Schools having enrollment only in grade 9:
tbl.na_g101112_2016 <- tbl.enrollment2016_hs %>% 
  filter(is.na(g10) & is.na(g11) & is.na(g12)) %>% 
  select(school_name, school_id)
tbl.na_g101112_2016


# Schools having enrollment only in grade 12:
tbl.na_g091011_2016 <- tbl.enrollment2016_hs %>% 
  filter(is.na(g09) & is.na(g10) & is.na(g11)) %>% 
  select(school_name, school_id)
tbl.na_g091011_2016


# Adding a total_hs variable to the datasets, and rechecking 
# the largest and smallest.
# The general procedure:
# Create a total_hs variable;
# Mutate auxilliary variables x09:x12 <- g09:g12
# ifelse( is.na(x09:x12), 0, x09:x12 )
# Total high school is vector sum of x09:x12
# Drop x09:x12

tbl.enrollment2016_hs <- tbl.enrollment2016_hs %>% 
  mutate(x09 = ifelse(is.na(g09), 0, g09 ),  
         x10 = ifelse(is.na(g10), 0, g10 ),  
         x11 = ifelse(is.na(g11), 0, g11 ),  
         x12 = ifelse(is.na(g12), 0, g12 ))

tbl.enrollment2016_hs <- tbl.enrollment2016_hs %>% 
  mutate( total_hs = x09 + x10 + x11 + x12 ) %>%
  select(-x09:-x12)
glimpse(tbl.enrollment2016_hs)

# Largest and smallest high school enrollments:
tbl.enrollment2016_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(desc(total_hs))

tbl.enrollment2016_hs %>% 
  select( govern, school_name, total_hs, year) %>% 
  arrange(total_hs)

# Summary stats:
summary(tbl.enrollment2016_hs)

# Uncomment as needed:
# Writing the 2016 data to disk:
# write_rds(tbl.enrollment2016, "./output/Rds/enrollment2016.Rds")
# write_rds(tbl.enrollment2016_hs, "./output/Rds/enrollment2016_hs.Rds")


          ################################################
          #                                              #
          #    Conclusion                                #
          #                                              #
          ################################################

# This completes the importation of the Chicago Public Schools high school 
# enrollment data into R format. Each annual file has been written to an Rds
# file for future use. Some remaining data cleaning tasks will be done in the
# script clean.R.






