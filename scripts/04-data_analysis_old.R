#### Preamble ####
# Purpose: Analyzes the clean data from the "data/clean_data" directory
# Author: Sima Shmuylovich
# Date: 15 February 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run "99-import_packages.R" and "02-data_cleaning.R" before running this script.

#### Workspace setup ####
library(tidyverse)
library(dplyr)

#### Analyze data ####
# Import clean data
clean_data <- read_csv("data/clean_data/clean_data.csv", col_names=TRUE, col_types=cols())

# Analyze Gender of Speakers 
gender <- clean_data %>%
  group_by(female) %>%
  summarise(Speakers = n())

gender_by_region <- clean_data %>%
  group_by(region, female) %>%
  summarise(Speakers = n())

gender_over_time <- clean_data %>%
  group_by(semester, female) %>%
  summarise(Speakers = n())

gender_by_region_over_time <- clean_data %>%
  group_by(semester, region, female) %>%
  summarise(Speakers = n())

# Analyze URM of Speakers 
urm <- clean_data %>%
  group_by(urm) %>%
  summarise(Speakers = n())

urm_by_region <- clean_data %>%
  group_by(region, urm) %>%
  summarise(Speakers = n())

urm_over_time <- clean_data %>%
  group_by(semester, urm) %>%
  summarise(Speakers = n())

urm_by_region_over_time <- clean_data %>%
  group_by(semester, region, urm) %>%
  summarise(Speakers = n())

# Analyze Demographic of Speakers 
demographic <- clean_data %>%
  group_by(urm, female) %>%
  summarise(Speakers = n())

demographic_by_region <- clean_data %>%
  group_by(region, urm, female) %>%
  summarise(Speakers = n())

demographic_over_time <- clean_data %>%
  group_by(semester, urm, female) %>%
  summarise(Speakers = n())

demographic_by_region_over_time <- clean_data %>%
  group_by(semester, region, urm, female) %>%
  summarise(Speakers = n())

# Analyze seminars with at least one female
at_least_one_female <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  filter(female==1) %>%
  summarise(Seminars = n())

at_least_one_female_by_region <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  group_by(region) %>%
  filter(female==1) %>%
  summarise(Seminars = n())

at_least_one_female_over_time <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  group_by(semester) %>%
  filter(female==1) %>%
  summarise(Seminars = n())

at_least_one_female_by_region_over_time <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  group_by(semester, region) %>%
  filter(female==1) %>%
  summarise(Seminars = n())

# Analyze seminars with at least one urm
at_least_one_urm <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  filter(urm==1) %>%
  summarise(Seminars = n())

at_least_one_urm_by_region <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  group_by(region) %>%
  filter(urm==1) %>%
  summarise(Seminars = n())

at_least_one_urm_over_time <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  group_by(semester) %>%
  filter(urm==1) %>%
  summarise(Seminars = n())

at_least_one_urm_by_region_over_time <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  group_by(semester, region) %>%
  filter(urm==1) %>%
  summarise(Seminars = n())

# Analyze seminars with at least one urm female
at_least_one_urm_female <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  filter(female==1 & urm==1) %>%
  summarise(Seminars = n())

at_least_one_urm_female_by_region <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  group_by(region) %>%
  filter(female==1 & urm==1) %>%
  summarise(Seminars = n())

at_least_one_urm_female_over_time <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  group_by(semester) %>%
  filter(female==1 & urm==1) %>%
  summarise(Seminars = n())

at_least_one_urm_female_by_region_over_time <- clean_data %>%
  distinct(seminar_id, .keep_all=TRUE) %>%
  group_by(semester, region) %>%
  filter(female==1 & urm==1) %>%
  summarise(Seminars = n())

#### Save data ####
write_csv(gender, "data/analysis_data/gender.csv")
write_csv(gender_by_region, "data/analysis_data/gender_by_region.csv")
write_csv(gender_over_time, "data/analysis_data/gender_over_time.csv")
write_csv(gender_by_region_over_time, "data/analysis_data/gender_by_region_over_time.csv")

write_csv(urm, "data/analysis_data/urm.csv")
write_csv(urm_by_region, "data/analysis_data/urm_by_region.csv")
write_csv(urm_over_time, "data/analysis_data/urm_over_time.csv")
write_csv(urm_by_region_over_time, "data/analysis_data/urm_by_region_over_time.csv")

write_csv(demographic, "data/analysis_data/demographic.csv")
write_csv(demographic_by_region, "data/analysis_data/demographic_by_region.csv")
write_csv(demographic_over_time, "data/analysis_data/demographic_over_time.csv")
write_csv(demographic_by_region, "data/analysis_data/demographic_by_region_over_time.csv")

write_csv(at_least_one_female, "data/analysis_data/at_least_one_female.csv")
write_csv(at_least_one_female_by_region, "data/analysis_data/at_least_one_female_by_region.csv")
write_csv(at_least_one_female_over_time, "data/analysis_data/at_least_one_female_over_time.csv")
write_csv(at_least_one_female_by_region_over_time, "data/analysis_data/at_least_one_female_by_region_over_time.csv")

write_csv(at_least_one_urm, "data/analysis_data/urm.csv")
write_csv(at_least_one_urm_by_region, "data/analysis_data/at_least_one_urm_by_region.csv")
write_csv(at_least_one_urm_over_time, "data/analysis_data/at_least_one_urm_over_time.csv")
write_csv(at_least_one_urm_by_region_over_time, "data/analysis_data/at_least_one_urm_by_region_over_time.csv")

write_csv(at_least_one_urm_female, "data/analysis_data/urm_female.csv")
write_csv(at_least_one_urm_female_by_region, "data/analysis_data/at_least_one_urm_female_by_region.csv")
write_csv(at_least_one_urm_female_over_time, "data/analysis_data/at_least_one_urm_female_over_time.csv")
write_csv(at_least_one_urm_female_by_region, "data/analysis_data/at_least_one_urm_female_by_region_over_time.csv")


