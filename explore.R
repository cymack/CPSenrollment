# Libraries
library(tidyverse)
library(forcats)
library(ggthemes)
library(lubridate)
library(readr)

# Reading in the enrollment_all_hs data set 
tbl.enrollment_all_hs <- read_rds("/home/cym/math/R/sessions/cps/output/Rds/cleaned/enrollment_all_hs.Rds")

tbl.enrollment_all_hs %>% glimpse()

# Summaries We already know from the checks in data.R that there are outliers at
# both the large and small ends of the high school enrollment range. Therefore
# we expect to use median and range as statistics for centrality and
# variability. Nevertheless, here are overall summary statistics.

tbl.enrollment_all_hs %>% summary()


# Two remaining schools without school_id values were revealed. They are
# Jefferson IDOC/Healy and Jerfferson Ctr/Factory (an apparent spelling error).
# These will be deleted from further analysis pending more information.

tbl.enrollment_all_hs <- tbl.enrollment_all_hs %>%
  filter(!is.na(school_id))

tbl.enrollment_all_hs %>% glimpse()
tbl.enrollment_all_hs %>% summary()


tbl.enrollment_all_hs %>% 
  ggplot(aes(x=total_hs)) + 
  geom_histogram() +
  geom_vline(xintercept = mean(tbl.enrollment_all_hs$total_hs), alpha = 0.5) +
  geom_vline(xintercept = median(tbl.enrollment_all_hs$total_hs), alpha = 0.5) +
  annotate("text", 
           x = 740, 
           y = 345, 
           hjust = 1,
           size = 3, 
           angle = 90, 
           alpha = .5, 
           label = paste("Mean", 
                         "=", 
                         round(mean(tbl.enrollment_all_hs$total_hs)), 
                              sep = " ")) +
  annotate("text", 
           x = 485, 
           y = 370, 
           hjust = 1,
           size = 3, 
           angle = 90, 
           alpha = .5, 
           label = paste("Median", 
                         "=", 
                         round(median(tbl.enrollment_all_hs$total_hs)), 
                              sep = " ")) +
  annotate("text", 
           x = 1750, 
           y = 300,
           hjust = 0,
           size = 4,
           label = "Several perennially large schools \ngreatly affect annual enrollment averages. They include \nLane, Curie, Kelly, and Taft regular public schools, \nand Youth Connections Charter.") +
  labs(x = "Total High School Enrollment",
       y = "Number of Schools",
       title = "Distribution of Schools by Total High School Enrollment, All CPS, 2006 to 2017",
       subtitle = "Data from Chicago Public Schools, April 2017 \n(http://www.cps.edu/SchoolData/Pages/SchoolData.aspx#collapseThree)")

ggsave("./output/graphics/totalhs_histogram_by_school.pdf", 
       width = 7.5, 
       height = 6.0, 
       units = "in")

# Graphs and counts of school types .......................................... 

# What does the dataset say about changes in the number of high schools overall,
# the comparative rates of growth of the major types of schools and the same for
# their enrollments? Which schools have seen the greatest declines during this
# period, and can the decline be attributed mainly to the growth of charter
# schools?

tbl.allschools_years <- tbl.enrollment_all_hs %>% 
  group_by(year) %>% 
  count() 
tbl.allschools_years %>% print(n=20)

# An annual time line graph of number of all high schools:
tbl.allschools_years %>% 
  ggplot(aes(x = ymd(year, truncated = 3), y = n)) + 
  geom_point() +
  geom_line(linetype = "dashed", alpha = .10) +
  annotate("text", 
           x = ymd(2011, truncated = 3), 
           y = 170, 
           size = 4, 
           hjust = 0.80, 
           label = "Fastest growth in number of schools: \nfrom 155 schools in 2013 to 185 in 2014.") +
  labs(x = "Year",
       y = "Number of Schools",
       title = "All CPS High Schools by Year, 2006 through 2017",
       subtitle = "Data from Chicago Public Schools, April 2017 \n(http://www.cps.edu/SchoolData/Pages/SchoolData.aspx#collapseThree)")

ggsave("./output/graphics/allschools_years.pdf",
       width = 6.75,
       height = 4.75,
       units ="in")

# Joint timelines of annual school numbers by governance type.
# Minor complication: in 2017 and only in that year, there are three governance 
# categories, not two. One solution is to create an adjusted n variable, taking
# care to annotate the fact. We can use forcats::fct_collapse.

tbl.x <- 
  tbl.enrollment_all_hs %>% 
  mutate(govern = as_factor(govern)) %>% 
  mutate(govern = fct_collapse(govern, "charter" = c("charter", "other")))

# So in any table or graph derived from tbl.x, the "charter" factor includes
# contract or other schools that in the main data frame are factored separately
# as "other."

tbl.allschools_years_gov <- tbl.x %>% 
  group_by(year, govern) %>% 
  count()
tbl.allschools_years_gov %>% print(n=24)

# Timelines by governance type:
# Number of schools timelines:
tbl.allschools_years_gov %>% 
  ggplot(aes(x = ymd(year, truncated = 3), y = n)) + 
  geom_point(aes(shape = govern)) +
  geom_line(aes(linetype = govern), alpha = .10) +
  annotate("text", 
           x = ymd(2006, truncated = 3), 
           y = 75, 
           size = 4, 
           hjust = 0, 
           label = "Most striking dynamic element \nin number of schools is the growth of charters.") +
  annotate("text",
           x = ymd(2015, truncated = 3),
           y = 125,
           size = 3,
           hjust = 1,
           alpha = 0.5,
           label = "regular") +
  annotate("text",
           x = ymd(2015, truncated = 3),
           y = 60,
           size = 3,
           hjust = 0,
           alpha = 0.5,
           label = "charter") +
  theme(legend.position = "none") +
  labs(x = "Year",
       y = "Number of Schools",
       title = "CPS High Schools by Year and Governance Type, 2006 through 2017",
       subtitle = "Data from Chicago Public Schools, April 2017. In 2017, contract schools are included among charters. \n(http://www.cps.edu/SchoolData/Pages/SchoolData.aspx#collapseThree)")


ggsave("./output/graphics/allschools_years_gov.pdf",
       width = 6.75,
       height = 4.75,
       units ="in")

tbl.typemeans <- 
  tbl.x %>% 
  group_by(govern, year) %>% 
  summarise(mean(total_hs))

colnames(tbl.typemeans) <- (c("govern", "year", "mean_totalhs"))


# Enrollment timelines:
tbl.typemeans %>% 
  ggplot(aes(x = ymd(year, truncated = 3), y = mean_totalhs)) + 
  geom_point(aes(shape = govern)) +
  geom_line(aes(linetype = govern), alpha = .10) +
  annotate("text", 
           x = ymd(2006, truncated = 3), 
           y = 700, 
           size = 4, 
           hjust = 0, 
           label = "Average enrollment cycles \nwithout trend for charter schools, but \nwith downward trend for regular schools.") +
  annotate("text",
           x = ymd(2015, truncated = 3),
           y = 650,
           size = 3,
           hjust = 1,
           alpha = 0.5,
           label = "regular") +
  annotate("text",
           x = ymd(2015, truncated = 3),
           y = 450,
           size = 3,
           hjust = 0,
           alpha = 0.5,
           label = "charter") +
  theme(legend.position = "none") +
  labs(x = "Year",
       y = "Mean High School Enrollment",
       title = "Enrollment in CPS High Schools by Year and Governance Type, 2006 through 2017",
       subtitle = "Data from Chicago Public Schools, April 2017. In 2017, contract schools are included among charters. \n(http://www.cps.edu/SchoolData/Pages/SchoolData.aspx#collapseThree)")



ggsave("./output/graphics/enrollments_years_gov.pdf",
       width = 7.5,
       height = 4.75,
       units ="in")


#####
# Facet of school counts, bar chart (corresponding to group by (year, govern)):
grf.hs_by_type_dodged <- 
  ggplot(data=tbl.enrollment_all_hs, aes(x=govern)) + 
  geom_bar() + 
  facet_wrap(~year)

ggsave("./output/graphics/hs_by_type_dodged.pdf")

# Enrollment boxplots by year. These are minimalist Tufte-style boxplots, using
# a geom provided by the package ggthemes.

tbl.enrollment_all_hs %>% 
  ggplot(aes(x=as.factor(year), y=total_hs)) + 
  theme_tufte(ticks=F) + 
  geom_tufteboxplot(median.type = "line", 
                    whisker.type = "line", 
                    hoffset = 0, 
                    width = 3) + 
  labs(title = "Annual CPS High School Enrollment Distributions", 
       x ="Year", 
       y ="Total High School Enrollment per School") + 
  coord_flip()

ggsave("./output/graphics/total_boxes.pdf")

## The fill colors in the next two graphs do not convert well to monochrome printing.
# Theme adjustments are necessary. 
# A filled bar graph showing school count, annual:
grf.hs_by_type_count <- 
  tbl.enrollment_all_hs %>%  
  mutate(govern = fct_rev(fct_infreq(factor(govern)))) %>%
  ggplot(aes(x=as.factor(year), fill=govern)) +
  geom_bar() +
  labs(title = "Schools by Type, 2006 to 2017 (stacked)",
       x="Year",
       y="Number of Schools") + 
  scale_fill_grey(start = 0.4) +
  theme(panel.background = element_blank()) +
  coord_flip()

# The graph is saved in pdf by the following command:
ggsave("./output/graphics/hs_by_type_count_gray.pdf", width = 6.00, height = 4.5, units="in")

# A filled bar graph showing school count by proportion, annual:
grf.hs_by_type_prop <- tbl.enrollment_all_hs %>%  
  mutate( govern = fct_rev(fct_infreq(factor(govern)))) %>%
  ggplot(aes(x=as.factor(year), fill=govern)) +
  geom_bar(position="fill") +
  labs(title = "CPS High Schools by Type, 2006 to 2017 (proportions)",
       x ="Year",
       y ="Proportion of Schools") + 
  scale_fill_grey(start = 0.4) +
  theme(panel.background = element_blank()) +
  coord_flip()

ggsave("./output/graphics/hs_by_type_prop.pdf", width = 6.00, height = 4.5, units="in")

# The tibble with reversed govern levels used in the graphs might be a useful
# object, or not: tbl.enrollment_all_hs %>%  mutate( govern = fct_rev(
# fct_infreq( factor(govern) ) ) )








# Graphs of total high school enrollment ..................................

# Facet of school enrollments, bar chart:
grf.hsrolls_by_type_dodged <- 
  tbl.enrollment_all_hs %>% mutate(govern = fct_infreq(govern)) %>%
  ggplot(aes(x=govern, stat=total_hs)) + 
  geom_bar() + 
  facet_wrap(~year) +
  labs(title = "Annual CPS High School Enrollments by School Type", 
       x ="School Type", 
       y ="Enrollment") +
  coord_flip()

ggsave("./output/graphics/grf.hsrolls_by_type_dodged.pdf", 
       width = 6.00, 
       height = 4.5, 
       units = "in")


# Facets of school timelines --- 129 schools having at least 8 observations:
tbl.for_grid <- tbl.enrollment_all_hs %>% 
  group_by(school_id) %>% 
  count() %>% 
  filter(n > 7)

tbl.enrollment_all_hs %>% 
  filter(school_id %in% tbl.for_grid$school_id) %>% 
  ggplot(aes(x= year, y=total_hs)) +
  geom_line() +
  facet_wrap(~school_id) +
  labs(title = "Enrollment Timelines, CPS High Schools Having At Least 8 Observations",
       x = "year",
       y = "High School Enrollment") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave("./output/graphics/total_grid.pdf",
       width = 7.00, 
       height = 8.0, 
       units = "in")

tbl.enrollment_all_hs <- tbl.enrollment_all_hs %>% 
  group_by(common_name) %>% 
  mutate(ths_pct = 100*(total_hs - dplyr::lag(total_hs))/dplyr::lag(total_hs)) %>% 
  ungroup()
#  select(common_name, year, total_hs, ths_pct) 

# Scatter of percent change in total_hs, without color coding:
grf.scatter <- tbl.enrollment_all_hs %>% 
  filter(ths_pct <= 100) %>%  
  ggplot( aes(x=total_hs, y=ths_pct) ) + 
  geom_point()

ggsave("./output/graphics/grf_scatter.pdf")

# Histogram of percent change. A binwidth=5.0 gives 5 percentage points in each 
# bin, for approximately 100 bins with the range (-90, 350). This range trims
# the two observations with values well over 1000 percent, which are two small
# start-up schools.
grf.ths_pct_histogram <-  tbl.enrollment_all_hs %>% 
  ungroup() %>%
  ggplot( aes(x=ths_pct) ) +
  geom_histogram(binwidth=5.0, fill="gray", color="black") + 
  xlim(-90, 350)


# Absolute values of percent change in total_hs:
grf.scatter_abs <- tbl.enrollment_all_hs %>% 
  ungroup() %>% 
  filter(ths_pct <= 100) %>%  
  mutate( abs_ths_pct=abs(ths_pct) ) %>% 
  ggplot( aes(x= total_hs , y=abs_ths_pct ) ) + 
  geom_point()
ggsave("./output/graphics/grf_scatter_abs.pdf")

# Decliners selected from grid search of school timeline graphs:
vec.decliners <- c(609674,
                   609698,
                   609676,
                   609702,
                   609705,
                   609707,
                   609708,
                   609709,
                   609710,
                   609711,
                   609712,
                   609713,
                   609722,
                   609723,
                   609735,
                   609759,
                   609761,
                   609762,
                   609768,
                   609716,
                   609736,
                   609704)
tbl.enrollment_all_hs %>% 
  ungroup() %>% 
  filter( school_id %in% vec.decliners) %>% 
  select(school_id, common_name) %>% 
  print(n=25)
# A tibble: 260 x 2
# school_id    common_name
# <int>          <chr>
#   1    609708     Foreman HS
# 2    609716 Kelvyn Park HS
# 3    609759    Clemente HS
# 4    609702  Crane Tech HS
# 5    609722      Manley HS
# 6    609723    Marshall HS
# 7    609676  Dunbar Voc HS
# 8    609704    Farragut HS
# 9    609735      Tilden HS
# 10    609698  Bogan Tech HS
# 11    609736       Dyett HS
# 12    609709   Gage Park HS
# 13    609711      Harper HS
# 14    609712      Hirsch HS
# 15    609768      Hope CPHS
# 16    609713   Hyde Park HS
# 17    609707     Robeson HS
# 18    609674 Chicago Voc HS
# 19    609761     Corliss HS
# 20    609705     Fenger  HS
# 21    609710      Harlan HS
# 22    609762      Julian HS


tbl.enrollment_all_hs %>% 
  filter(school_id %in% vec.decliners) %>% 
  ggplot( aes(x=as.numeric(year), y=total_hs) ) + 
  geom_line() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  facet_wrap(~school_id)
ggsave("./output/graphics/decliners_grid.pdf")


# Same graph with school name labels instead of id#:
tbl.enrollment_all_hs %>% 
  filter(school_id %in% vec.decliners) %>% 
  ggplot( aes(x=as.numeric(year), y=total_hs) ) + 
  geom_line() + 
  labs(x = "Year", y = "Total High School Enrollment per School") +
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  facet_wrap(~common_name)
ggsave("./output/graphics/decliners_named_grid.pdf")


tbl.enrollment_all_hs <- tbl.enrollment_all_hs %>% 
  mutate(decline = school_id %in% vec.decliners) 
tbl.enrollment_all_hs %>% 
  ggplot( aes(x=total_hs, y=ths_pct, color = decline)) + 
  geom_point()


# By doing the previous graph in layers, the decline observations can be made to
# stand out by making the non-declines more transparent.
tbl.decline_1 <- tbl.enrollment_all_hs %>% filter(abs(ths_pct) <=100  & decline==1)
tbl.decline_0 <- tbl.enrollment_all_hs %>% filter(abs(ths_pct) <=100  &decline==0)

# Can the y-scale be compressed to show more detail in the main data? This says
# "yes."
tbl.enrollment_all_hs %>%  arrange(desc(ths_pct)) %>% select(ths_pct)
# A tibble: 1,913 x 1
# ths_pct
# <dbl>
#   1 9750.0000
# 2 1740.0000
# 3  336.0465
# 4  314.2012
# 5  241.1765


grf.scatter_color <-  tbl.enrollment_all_hs %>% 
  filter( abs(ths_pct) <= 100) %>%  
  ggplot( aes(x=total_hs, y=ths_pct, color=decline)) + 
  geom_point( data = tbl.decline_0, aes(x = total_hs, y = ths_pct), size = 1, alpha = .25) +
  geom_point( data = tbl.decline_1, size = 1) + 
  theme(panel.background = element_rect(fill="white")) + 
  theme(panel.grid.major = element_line(color="gray"), panel.grid.minor = element_line(color="light gray"))
ggsave("./output/graphics/grf_scatter_color.pdf")

grf.scatter_abs_color <-  tbl.enrollment_all_hs %>% 
  filter( abs(ths_pct) <= 100) %>%  
  ggplot( aes(x=total_hs, y=abs(ths_pct), color=decline)) + 
  geom_point( data = tbl.decline_0, aes(x = total_hs, y = abs(ths_pct)), size = 1, alpha = .25) +
  geom_point( data = tbl.decline_1, size = 1) + 
  theme( panel.background = element_rect(fill="white") ) + 
  theme(panel.grid.major = element_line(color="gray"), panel.grid.minor = element_line(color="light gray") )
ggsave("./output/graphics/grf_scatter_abs_color.pdf")

tbl.dec_nondec_avg <- " 
  'decline', 2006, val.1 
  'nondecline', 2006, val.2 
  'decline', 2017, val.3 
  'nondecline', 2017, val.4
"





