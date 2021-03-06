Introducing CPS Enrollment Datasets
================
Charlotte Mack
March 15, 2019

CPSenrollment
=============

Exploring enrollment data from the Chicago Public Schools

This repository contains exploratory analysis and visualizations of enrollment data that were extracted from Chicago Public Schools (CPS) public records. The prepared data are available at this repository in Rds and csv formats; at present there are only high school data, with elementary school data forthcoming. The sets span the school years from 2006--2007 through 2018--2019, and will be periodically updated.

A glimpse of the high schools data:

    ## Warning in gzfile(file, "rb"): cannot open compressed file './
    ## enrollment_all_hs.Rds', probable reason 'No such file or directory'

    ## Error in gzfile(file, "rb"): cannot open the connection

    ## Error in eval(lhs, parent, parent): object 'enrollment_hs' not found

The original CPS data are available in a series of spreadsheets at [CPS website](http://www.cps.edu/SchoolData/Pages/SchoolData.aspx)[1] Downloadable data files that I have prepared are in this repository with Rds and csv extensions, downloadable through the links below. The Rds files, which are used in R language programming, may have some type designations that are not in the csv files, but there should be no other difference.

[Rds file for all CPS high schools, September 2006 to September 2018](https://github.com/cymack/CPSenrollment/blob/master/enrollment_all_hs.Rds)

[csv file for all CPS high schools, September 2006 to September 2018](https://github.com/cymack/CPSenrollment/blob/master/enrollment_all_hs.csv)

Rds files of high school locations as of 2014--2015 school year, [with enrollments from 2016--2017 school year](https://github.com/cymack/CPSenrollment/blob/master/school_loc_merged.2016.Rds), and from [2006--2007 school year](https://github.com/cymack/CPSenrollment/blob/master/school_loc_merged.206.Rds) These data were used to construct the school maps linked below.

A more detailed overview of the all-years enrollment data is in the file [CPSenrollment.md](https://github.com/cymack/CPSenrollment/blob/master/CPSenrollment.md).

[A brief article with maps of District high schools under regular and other governance for the 2006--2007 and 2016--2017 school years is in the file school\_maps.md.](https://github.com/cymack/CPSenrollment/blob/master/school_maps.md)

[The preparation of the data 2006-2017 is described in data.R](https://github.com/cymack/CPSenrollment/blob/master/data.R) and [clean.R](https://github.com/cymack/CPSenrollment/blob/master/clean.R). data.R is a protocol in the form of a script of the first interactive steps that I took in preparing the unified data set. The protocol here takes the data from the original set of annual membership rosters, published in spreadsheets and having various sets and arrangements of variables, to R data.frames of uniform layout. I undertook the project when I was fairly new to R, so it is not as automated as it might be; on the other hand, each year's roster from the Chicago Public Schools has idiosyncrasies that can only be found and worked out manually. clean.R examines missing data more closely and creates a uniform set of names for each school to be used throughout the data.frame.

[The protocol for the 2018-2019 update is in data\_2018.R](https://github.com/cymack/CPSenrollment/blob/master/data_2018.R), for those who want a more compact overview.

Miscellaneous exploratory analysis, mostly in graphs is in [explorechange a subset with dplyr transmute.R](https://github.com/cymack/CPSenrollment/blob/master/explore.R).

[1] Membership data are under the heading "Demographics."
