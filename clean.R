# Continue the preparation of CPS enrollment data for analysis, by
# creating uniform nicknames for the schools, and binding the separate
# files into a master data set.

# Use/modify this to direct output:
# sink("data_cps.out", split=TRUE)


library(tidyverse)
library(forcats)
library(stringr)
library(magrittr)

# Loading the Rds files of high school data that were written in the
# data.R procedure:

##	## This can wait.
##	## Per Wickham's rule about pasting no more than twice, this job calls
##	## for a function. The stages are to get a list of the files to be
##	## loaded, extract the primary part of the file names to use as handles
##	## for the loaded data, and use read_rds to load each file in turn into
##	## its handle. So,
##	## list.files()
##	> sets <- list.files("./output/Rds/", pattern="*_hs.Rds")
##	> sets %>% str_split("\\.") %>% class()
##	[1] "list"
##	> sets %>% str_split("\\.")
##	[[1]]
##	[1] "enrollment2006_hs" "Rds"              
##	
##	[[2]]
##	[1] "enrollment2007_hs" "Rds"              
##	
##	[[3]]
##	[1] "enrollment2008_hs" "Rds"              
##	
##	[...]
##	
##	Convert the list sets to a dataframe:
##	> df.x <- plyr::ldply(x, data.frame)
##	> df.x
##	              X..i..
##	1  enrollment2006_hs
##	2                Rds
##	3  enrollment2007_hs
##	4                Rds
##	5  enrollment2008_hs
##	6                Rds
##	[...]
##	23 enrollment2017_hs
##	24               Rds
##	
##	Filter out the Rds and give the result a name:
##	df.handles <- filter(df.x, !str_detect(X..i.., "Rds"))
##	
##	Give the df variable a better name, and clean up:
##	
##	df.handles$handle <- df.handles$X..i..
##	df.handles <- select(df.handles, handle)
##	## 
##	enrollment2006_hs <-  read_rds("./output/Rds/enrollment2006_hs.Rds")
##	
##	Now we want something like handle <- read_rds("handle.Rds")
##	Be sure to include a path argument!!!

# The first set that needs common_names constructed is 2009. The idea is to take
# the most recent set of common_name, from 2008, and match that by school_id,
# then manually add the ones not present in the earlier year. The procedure
# comes from RDS p.179, which actually is a digression in a discussion of
# mutating joins. I would have used a mutating join here, but the result was
# very untidy. It is less work for now to do it this way. This command correctly
# identified the matches from the 2008 data and inserted them into a new 
# variable in the 2009 set, with NA values for schools not in 2009:

tbl.enrollment2008_hs <-  read_rds("./output/Rds/enrollment2008_hs.Rds")
tbl.enrollment2009_hs <-  read_rds("./output/Rds/enrollment2009_hs.Rds")

tbl.enrollment2009_hs <- 
    tbl.enrollment2009_hs %>% 
    mutate(common_name = 
             tbl.enrollment2008_hs$common_name[match(school_id, tbl.enrollment2008_hs$school_id)])

# Counting the NAs generated, and identifying them:
tbl.enrollment2009_hs %>% count(is.na(common_name))

tbl.enrollment2009_hs %>% 
  filter(is.na(common_name)) %>% 
  select(school_id, school_name)


# This exercise has revealed another missclassified school, UCC-Woodson,
# which is a middle school. It has been dropped from the 2009 data, and
# should be dropped from any other hs list where it is found, and the
# combined list for each year corrected. It is probably best to do this
# after all years are bound together. The corrected tbl.*2009 shows only
# 8 missing common_names to be added.

# Entries needed common_name:
tbl.needs2009 <- tbl.enrollment2009_hs %>% filter(is.na(common_name)) %>%
  filter(!is.na(school_id)) %>% 
  select(school_id, school_name)
tbl.needs2009

# New common_name for 2009:
tbl.new2009 <- c("VOISE", 
                 "Orr", 
                 "Power House", 
                 "NSC-Comer", 
                 "NSC-UIC", 
                 "Perspectives-MS",
                 "Woodson South",
                 "UNO-Archer Hghts", 
                 "Community Services W")
tbl.needs2009 <- tbl.needs2009 %>% mutate(common_name = tbl.new2009)

tbl.enrollment2009_hs <- tbl.enrollment2009_hs %>% 
  mutate(common_name = ifelse(is.na(common_name), 
    tbl.needs2009$common_name[match(school_id, tbl.needs2009$school_id)], 
    common_name))
tbl.enrollment2009_hs %>% count(is.na(common_name))

# Names for 2010 .................................................
tbl.enrollment2010_hs <-  read_rds("./output/Rds/enrollment2010_hs.Rds")
head(tbl.enrollment2010_hs)
tbl.enrollment2010_hs <- tbl.enrollment2010_hs %>% 
  mutate(common_name = 
           tbl.enrollment2009_hs$common_name[match(school_id, tbl.enrollment2009_hs$school_id)])	 %>%
  filter(!is.na(school_id))

# Counting the NAs generated, and identifying them:
tbl.enrollment2010_hs %>% count(is.na(common_name))

# Identifying entries that need a common_name created for them:
tbl.needs2010 <- tbl.enrollment2010_hs %>% 
  filter( is.na(common_name) ) %>%
  select(school_id, school_name)
print(tbl.needs2010, n=100)


# Common_names for the schools in the needs tibble:
tbl.new2010 <- c("Alcott", 
                 "Mason", 
                 "Westinghouse II", 
                 "VOISE",
                 "Air Force", 
                 "Orr",
                 "Ogden", 
                 "Power House",
                 "NSC-Bulls", 
                 "NSC-Comer",
                 "NSC-Muchin", 
                 "NSC-UIC",
                 "Perspectives-Calumet ΙI",
                 "Perspectives-IIT",
                 "UNO Charter-Garcia",
                 "Advanced Tech", 
                 "Chicago Arts", 
                 "Talent", 
                 "Community Services West",
                 "EPIC", 
                 "Urban Prep-E Garfield" )

# Inserting the new names into the needs tibble as a new variable
tbl.needs2010 <- tbl.needs2010 %>% mutate(common_name = tbl.new2010)
print(tbl.needs2010, n=25)

# Recoding the NAs in the 2010 dataset with the new common_names:
(tbl.enrollment2010_hs <- tbl.enrollment2010_hs %>% 
    mutate(common_name = 
             ifelse(is.na(common_name), 
             tbl.needs2010$common_name[match(school_id, 
             tbl.needs2010$school_id)], common_name)))

# A check. There should be no NAs for common_name remaining.
tbl.enrollment2010_hs %>% count(is.na(common_name))
tbl.enrollment2010_hs %>% glimpse()


# Names for 2011 .................................................
tbl.enrollment2011_hs <-  read_rds("./output/Rds/enrollment2011_hs.Rds")
head(tbl.enrollment2011_hs)

# Filtering out the entries having no school_id, and matching common_name and
# school_id to the previous year:
tbl.enrollment2011_hs <- tbl.enrollment2011_hs %>%
  filter(!is.na(school_id)) %>%
  mutate(common_name = 
           tbl.enrollment2010_hs$common_name[match(school_id, tbl.enrollment2010_hs$school_id)])	

# Counting the NAs generated, and identifying them:
tbl.enrollment2011_hs %>% count(is.na(common_name))

# Identifying entries that need a common_name created for them:
tbl.needs2011 <- tbl.enrollment2011_hs %>% 
  filter(is.na(common_name)) %>% 
  select(school_id, school_name)
print(tbl.needs2011, n=100)


# Common_names for the schools in the needs tibble:
tbl.new2011 <- c("Solorio", 
                 "CIC-Hawkins", 
                 "Instituto Health", 
                 "NSC-Pritzker", 
                 "NSC-Rauner", 
                 "NSC-Englewood", 
                 "Urban Prep-SShore")

# Inserting the new names into the needs tibble as a new variable
( tbl.needs2011 <- tbl.needs2011 %>% mutate(common_name = tbl.new2011) )

# Recoding the NAs in the 2011 dataset with the new common_names:
tbl.enrollment2011_hs <- tbl.enrollment2011_hs %>% 
    mutate(common_name = 
             ifelse(is.na(common_name), 
             tbl.needs2011$common_name[match(school_id, tbl.needs2011$school_id)], 
             common_name)) 
# Check that no entries with NA for common_name are left:
tbl.enrollment2011_hs %>% count(is.na(common_name))


# Names for 2012 .................................................
tbl.enrollment2012_hs <-  read_rds("./output/Rds/enrollment2012_hs.Rds")
head(tbl.enrollment2012_hs)
tbl.enrollment2012_hs <- tbl.enrollment2012_hs %>% 
  filter(!is.na(school_id)) %>%
  mutate(common_name = 
           tbl.enrollment2011_hs$common_name[match(school_id, tbl.enrollment2011_hs$school_id)])	

# Counting the NAs generated, and identifying them:
tbl.enrollment2012_hs %>% count(is.na(common_name))

# Identifying entries that need a common_name created for them:
tbl.needs2012 <- tbl.enrollment2012_hs %>% 
  filter(is.na(common_name)) %>% 
  select(school_id, school_name)
print(tbl.needs2012, n=100)


# Common_names for the schools in the needs tibble:
tbl.new2012 <- c("Banner South", 
                 "Summers", 
                 "Banner North", 
                 "Milburn", 
                 "South Shore ICP", 
                 "Pathways", 
                 "Banner West", 
                 "Prologue-Johnston")

# Inserting the new names into the needs tibble as a new variable
(tbl.needs2012 <- tbl.needs2012 %>% 
    mutate( common_name = tbl.new2012))

# Recoding the NAs in the 2012 dataset with the new common_names:
tbl.enrollment2012_hs <- tbl.enrollment2012_hs %>% 
    mutate(common_name = 
             ifelse(is.na(common_name), 
                    tbl.needs2012$common_name[match(school_id, tbl.needs2012$school_id)], 
                    common_name)) 
# Check that no entries with NA for common_name are left:
tbl.enrollment2012_hs %>% count(is.na(common_name))


# Names for 2013 .................................................
tbl.enrollment2013_hs <-  read_rds("./output/Rds/enrollment2013_hs.Rds")
head(tbl.enrollment2013_hs)
tbl.enrollment2013_hs <- 
  tbl.enrollment2013_hs %>% 
  filter(!is.na(school_id)) %>%
  mutate(common_name = 
           tbl.enrollment2012_hs$common_name[match(school_id, tbl.enrollment2012_hs$school_id)])	

# Counting the NAs generated, and identifying them:
tbl.enrollment2013_hs %>% count(is.na(common_name))

# Identifying entries that need a common_name created for them:
tbl.needs2013 <- tbl.enrollment2013_hs %>% filter(is.na(common_name)) %>%
     select(school_id, school_name)
print(tbl.needs2013, n=100)


# Common_names for the schools in the needs tibble:
tbl.new2013 <- c("Goode", 
                 "Amandla", 
                 "Catalyst-Maria", 
                 "Excel", 
                 "CIC-Quest North", 
                 "Instituto Justice", 
                 "Legal Prep", 
                 "NSC-Pritzker", 
                 "NSC-Purple", 
                 "NSC-Silver")

# Inserting the new names into the needs tibble as a new variable
(tbl.needs2013 <- tbl.needs2013 %>% mutate( common_name = tbl.new2013))

# Recoding the NAs in the 2013 dataset with the new common_names:
tbl.enrollment2013_hs <- tbl.enrollment2013_hs %>% 
    mutate( common_name = ifelse(is.na(common_name), 
                                 tbl.needs2013$common_name[match(school_id, 
                                                                 tbl.needs2013$school_id)], 
                                 common_name)) 
# Check that no entries with NA for common_name are left:
tbl.enrollment2013_hs %>% count(is.na(common_name))


# Names for 2014 .................................................
tbl.enrollment2014_hs <-  read_rds("./output/Rds/enrollment2014_hs.Rds")
head(tbl.enrollment2014_hs)
tbl.enrollment2014_hs <- 
  tbl.enrollment2014_hs %>% 
  filter(!is.na(school_id)) %>%
  mutate(common_name = 
           tbl.enrollment2013_hs$common_name[match(school_id, 
                                                   tbl.enrollment2013_hs$school_id)])	

# Counting the NAs generated, and identifying them:
tbl.enrollment2014_hs %>% count(is.na(common_name))

# Identifying entries that need a common_name created for them:
tbl.needs2014 <- tbl.enrollment2014_hs %>% 
  filter(is.na(common_name)) %>% 
  select(school_id, school_name)
print(tbl.needs2014, n=100)

# Common_names for the schools in the needs tibble:
tbl.new2014 <- c("Disney", 
                 "Crane Med Prep", 
                 "BotY IB", 
                 "Camelot Safe", 
                 "Excel-Englewood", 
                 "Little Black Pearl", 
                 "MJ North Lawndale", 
                 "Ombudsman #1", 
                 "Pathways-Avondale", 
                 "Winnie Mandela", 
                 "Instituto Lozano", 
                 "Intrinsic", 
                 "NSC-Crimson", 
                 "NSC-Orange", 
                 "UNO-Rogers Park", 
                 "UNO-Soccer", 
                 "YCCS-Aspira", 
                 "YCCS-Scholastic", 
                 "YCCS-Ada McKinley", 
                 "YCCS-El Cuarto Año", 
                 "YCCS-Austin", 
                 "YCCS-CCA", 
                 "YCCS-Houston", 
                 "YCCS-Chatham", 
                 "YCCS-CYDI", 
                 "YCCS-Albizu", 
                 "YCCS-Innovations", 
                 "YCCS-Addams", 
                 "YCCS-Latino Youth", 
                 "YCCS-Olive Harvey", 
                 "YCCS-Sullivan", 
                 "YCCS-Truman", 
                 "YCCS-Virtual", 
                 "YCCS-West Town", 
                 "YCCS-Holistic", 
                 "YCCS-Connection" )

# Inserting the new names into the needs tibble as a new variable
(tbl.needs2014 <- tbl.needs2014 %>% mutate( common_name = tbl.new2014))

# Recoding the NAs in the 2014 dataset with the new common_names:
tbl.enrollment2014_hs <- tbl.enrollment2014_hs %>% 
    mutate(common_name = 
             ifelse(is.na(common_name), 
                    tbl.needs2014$common_name[match(school_id, tbl.needs2014$school_id)], 
                    common_name)) 
# Check that no entries with NA for common_name are left:
tbl.enrollment2014_hs %>% count(is.na(common_name))


# Names for 2015 .................................................
tbl.enrollment2015_hs <-  read_rds("./output/Rds/enrollment2015_hs.Rds")
head(tbl.enrollment2015_hs)
tbl.enrollment2015_hs <- 
  tbl.enrollment2015_hs %>% 
  filter(!is.na(school_id)) %>%
  mutate(common_name = 
           tbl.enrollment2014_hs$common_name[match(school_id, 
                                                   tbl.enrollment2014_hs$school_id)])	

# Counting the NAs generated, and identifying them:
tbl.enrollment2015_hs %>% count(is.na(common_name))

# Identifying entries that need a common_name created for them:
tbl.needs2015 <- tbl.enrollment2015_hs %>% 
  filter(is.na(common_name)) %>% 
  select(school_id, school_name)
print(tbl.needs2015, n=100)

# Common_names for the schools in the needs tibble:
tbl.new2015 <- c("Marine-Ames", 
                 "MJ-South Shore", 
                 "MJ-Englewood", 
                 "MJ-Humboldt", 
                 "Ombudsman #2", 
                 "Ombudsman #3", 
                 "Pathways-Brighton", 
                 "NCS-Noble", 
                 "Noble-ITW", 
                 "Excel-Southwest")

# Inserting the new names into the needs tibble as a new variable
(tbl.needs2015 <- tbl.needs2015 %>% mutate( common_name = tbl.new2015))

# Recoding the NAs in the 2015 dataset with the new common_names:
tbl.enrollment2015_hs <- tbl.enrollment2015_hs %>% 
    mutate(common_name = 
             ifelse(is.na(common_name), 
                    tbl.needs2015$common_name[match(school_id, 
                                                    tbl.needs2015$school_id)], 
                    common_name)) 
# Check that no entries with NA for common_name are left:
tbl.enrollment2015_hs %>% count(is.na(common_name))


# Names for 2016 .................................................
tbl.enrollment2016_hs <-  read_rds("./output/Rds/enrollment2016_hs.Rds")
head(tbl.enrollment2016_hs)
tbl.enrollment2016_hs <- 
  tbl.enrollment2016_hs %>% 
  filter(!is.na(school_id)) %>%
  mutate(common_name = 
           tbl.enrollment2015_hs$common_name[match(school_id, 
                                                   tbl.enrollment2015_hs$school_id)])	

# Counting the NAs generated, and identifying them:
tbl.enrollment2016_hs %>% count(is.na(common_name))

# Identifying entries that need a common_name created for them:
tbl.needs2016 <- tbl.enrollment2016_hs %>% 
  filter(is.na(common_name)) %>% 
  select(school_id, school_name)
print(tbl.needs2016, n=100)


# Common_names for the schools in the needs tibble:
tbl.new2016 <- c("Camelot-Garfield", 
                 "Excel-Woodlawn", 
                 "MJ-Brainerd", 
                 "ASPIRA Bus", 
                 "Horizon-Southwest")

# Inserting the new names into the needs tibble as a new variable
(tbl.needs2016 <- tbl.needs2016 %>% mutate( common_name = tbl.new2016))

# Recoding the NAs in the 2016 dataset with the new common_names:
tbl.enrollment2016_hs <- tbl.enrollment2016_hs %>% 
    mutate(common_name = 
             ifelse(is.na(common_name), 
                    tbl.needs2016$common_name[match(school_id, 
                                                    tbl.needs2016$school_id)], 
                    common_name)) 

# Check that no entries with NA for common_name are left:
tbl.enrollment2016_hs %>% count(is.na(common_name))


# Names for 2017 .................................................
tbl.enrollment2017_hs <-  read_rds("./output/Rds/enrollment2017_hs.Rds")
head(tbl.enrollment2017_hs)
tbl.enrollment2017_hs <- 
  tbl.enrollment2017_hs %>% 
 filter(!is.na(school_id)) %>%
  mutate(common_name = 
           tbl.enrollment2016_hs$common_name[match(school_id, 
                                                   tbl.enrollment2016_hs$school_id)])	

# Counting the NAs generated, and identifying them:
tbl.enrollment2017_hs %>% count(is.na(common_name))

# Identifying entries that need a common_name created for them:
tbl.needs2017 <- tbl.enrollment2017_hs %>% 
  filter(is.na(common_name)) %>% 
  select(school_id, school_name)
print(tbl.needs2017, n=100)


# Common_names for the schools in the needs tibble:
tbl.new2017 <- c("Dyett Arts", "Foundations", "Noble Mansueto")

# Inserting the new names into the needs tibble as a new variable
(tbl.needs2017 <- tbl.needs2017 %>% mutate(common_name = tbl.new2017))

# Recoding the NAs in the 2017 dataset with the new common_names:
tbl.enrollment2017_hs <- tbl.enrollment2017_hs %>% 
    mutate(common_name = ifelse(is.na(common_name), 
                                tbl.needs2017$common_name[match(school_id, 
                                                                tbl.needs2017$school_id)], 
                                common_name)) 
# Check that no entries with NA for common_name are left:
tbl.enrollment2017_hs %>% count(is.na(common_name))


# The final columns are to be govern, school_id, common_name, year, total,
# total_hs, g09, g10, g11, g12.

# The 2006 and 2007 data, not used in the cleaning exercise, are loaded here to
# check that their columns conform:
tbl.enrollment2006_hs <-  read_rds("./output/Rds/enrollment2006_hs.Rds")
glimpse(tbl.enrollment2006_hs)

tbl.enrollment2007_hs <-  read_rds("./output/Rds/enrollment2007_hs.Rds")
glimpse(tbl.enrollment2007_hs)

tbl.enrollment2006_hs <- tbl.enrollment2006_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2006_hs %>% glimpse()

tbl.enrollment2007_hs <- tbl.enrollment2007_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2007_hs %>% glimpse()

tbl.enrollment2008_hs <- tbl.enrollment2008_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2008_hs %>% glimpse()

tbl.enrollment2009_hs <- tbl.enrollment2009_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2009_hs %>% glimpse()

tbl.enrollment2010_hs <- tbl.enrollment2010_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2010_hs %>% glimpse()

tbl.enrollment2011_hs <- tbl.enrollment2011_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2011_hs %>% glimpse()

tbl.enrollment2012_hs <- tbl.enrollment2012_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2012_hs %>% glimpse()

tbl.enrollment2013_hs <- tbl.enrollment2013_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2013_hs %>% glimpse()

tbl.enrollment2014_hs <- tbl.enrollment2014_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2014_hs %>% glimpse()

tbl.enrollment2015_hs <- tbl.enrollment2015_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2015_hs %>% glimpse()

tbl.enrollment2016_hs <- tbl.enrollment2016_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2016_hs %>% glimpse()

tbl.enrollment2017_hs <- tbl.enrollment2017_hs %>%
  select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)
tbl.enrollment2017_hs %>% glimpse()



# Writing the cleaned annual data to Rds files:
write_rds(tbl.enrollment2006_hs, "./output/Rds/cleaned/enrollment2006_hs.Rds")
write_rds(tbl.enrollment2007_hs, "./output/Rds/cleaned/enrollment2007_hs.Rds")
write_rds(tbl.enrollment2008_hs, "./output/Rds/cleaned/enrollment2008_hs.Rds")
write_rds(tbl.enrollment2009_hs, "./output/Rds/cleaned/enrollment2009_hs.Rds")
write_rds(tbl.enrollment2010_hs, "./output/Rds/cleaned/enrollment2010_hs.Rds")
write_rds(tbl.enrollment2011_hs, "./output/Rds/cleaned/enrollment2011_hs.Rds")
write_rds(tbl.enrollment2012_hs, "./output/Rds/cleaned/enrollment2012_hs.Rds")
write_rds(tbl.enrollment2013_hs, "./output/Rds/cleaned/enrollment2013_hs.Rds")
write_rds(tbl.enrollment2014_hs, "./output/Rds/cleaned/enrollment2014_hs.Rds")
write_rds(tbl.enrollment2015_hs, "./output/Rds/cleaned/enrollment2015_hs.Rds")
write_rds(tbl.enrollment2016_hs, "./output/Rds/cleaned/enrollment2016_hs.Rds")
write_rds(tbl.enrollment2017_hs, "./output/Rds/cleaned/enrollment2017_hs.Rds")



# Creating the final master file ......................................

# Loading and binding the cleaned annual files: This is from a tip on
# stackoverflow. I tried specifying the filepath in the list.files command,
# which would be superior to changing the wd, but the map_df did not follow the
# path. Will work on this at another time.
setwd("/home/cym/math/R/sessions/cps/output/Rds/cleaned/")
tbl.enrollment_all_hs <-
  list.files(pattern="*.Rds") %>%
  map_df(~read_rds(.))

tbl.enrollment_all_hs
setwd("/home/cym/math/R/sessions/cps/")


# A better column ordering:
tbl.enrollment_all_hs <- tbl.enrollment_all_hs %>% select(govern, school_id, common_name, year, total, total_hs, g09, g10, g11, g12)

# Save this now, though some work still to be done.
write_rds(tbl.enrollment_all_hs, "./output/Rds/cleaned/enrollment_all_hs")


# A count of the number of schools in the survey by school_id:
tbl.enrollment_all_hs %>% select(school_id) %>% unique() %>% dim()


# Factorizing the govern variable to use in frequency tables and graphs, and
# putting the factor levels in frequency order:
tbl.enrollment_all_hs <- tbl.enrollment_all_hs %>%
  mutate(govern = fct_infreq(factor(govern)))

tbl.enrollment_all_hs %>% glimpse()
