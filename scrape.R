library(tidyverse)
library(rvest)


# Student Needs -----------------------------------------------------------

needs_2022 <- readxl::read_xlsx('~/Downloads/selectedpopulations(7).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2021-22')

needs_2021 <- readxl::read_xlsx('~/Downloads/selectedpopulations.xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2020-21')

needs_2020 <- readxl::read_xlsx('~/Downloads/selectedpopulations(1).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2019-20')

needs_2019 <- readxl::read_xlsx('~/Downloads/selectedpopulations(2).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  mutate(year = '2018-19')

needs_2018 <- readxl::read_xlsx('~/Downloads/selectedpopulations(3).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2017-18')

needs_2017 <- readxl::read_xlsx('~/Downloads/selectedpopulations(4).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2016-17')

needs_2016 <- readxl::read_xlsx('~/Downloads/selectedpopulations(5).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2015-16')


student_needs <- bind_rows(needs_2021,needs_2022, needs_2020, needs_2019, 
                           needs_2018, needs_2017, needs_2016)

write_csv(student_needs, 'student_needs.csv')

# Student Diversity -------------------------------------------------------




diversity_2021 <- readxl::read_xlsx('~/Downloads/ClassSizebyRaceEthnicity.xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2020-21')

diversity_2020 <- readxl::read_xlsx('~/Downloads/ClassSizebyRaceEthnicity(1).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2019-20')

diversity_2019 <- readxl::read_xlsx('~/Downloads/ClassSizebyRaceEthnicity(2).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  mutate(year = '2018-19')

diversity_2018 <- readxl::read_xlsx('~/Downloads/ClassSizebyRaceEthnicity(3).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2017-18')

diversity_2017 <- readxl::read_xlsx('~/Downloads/ClassSizebyRaceEthnicity(4).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2016-17')

diversity_2016 <- readxl::read_xlsx('~/Downloads/ClassSizebyRaceEthnicity(5).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2015-16')


student_diversity <- bind_rows(diversity_2021, diversity_2020, diversity_2019, 
                           diversity_2018, diversity_2017, diversity_2016)

write_csv(student_diversity, 'student_diversity.csv')


# Student Mobility --------------------------------------------------------


mobility_2021 <- readxl::read_xlsx('~/Downloads/mobilityrates.xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2020-21')

mobility_2020 <- readxl::read_xlsx('~/Downloads/mobilityrates(1).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2019-20')

mobility_2019 <- readxl::read_xlsx('~/Downloads/mobilityrates(2).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  mutate(year = '2018-19')

mobility_2018 <- readxl::read_xlsx('~/Downloads/mobilityrates(3).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2017-18')

mobility_2017 <- readxl::read_xlsx('~/Downloads/mobilityrates(4).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2016-17')

mobility_2016 <- readxl::read_xlsx('~/Downloads/mobilityrates(5).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2015-16')


student_mobility <- bind_rows(mobility_2021, mobility_2020, mobility_2019, 
                           mobility_2018, mobility_2017, mobility_2016)

write_csv(student_mobility, 'student_mobility.csv')


# Staff Diversity ---------------------------------------------------------




