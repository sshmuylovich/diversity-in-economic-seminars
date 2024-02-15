#### Preamble ####
# Purpose: Cleans the raw data from the "data/raw_data" directory provided in the replication package. 
# Author: Sima Shmuylovich
# Date: 15 February 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run "99-import_packages.R" and "02-data_cleaning.R" before running this script.

#### Workspace setup ####
library(tidyverse)
library(dplyr)

#### Clean data ####
raw_data <- read_csv("data/raw_data/raw_data.csv", col_names=TRUE, col_types=cols())

df <-
  raw_data %>%
  # Only select attributes of interest
  select('id', 'semester', 'urm_status', 'census_region') %>%
  # Rename attributes to match convention
  rename(seminar_id = 'id',
         semester = 'semester',
         demographic = 'urm_status',
         region = 'census_region') %>%
  # Split demographic column into two columns, underepresented minority and female
  mutate(
    # Check if 'non-urm' is present in the demographic column, case-insensitive
    urm = ifelse(grepl("non-urm", demographic, ignore.case = TRUE), 1, 0), 
    # Check if 'female' is present in the demographic column, case-insensitive
    female = ifelse(grepl("female", demographic, ignore.case = TRUE), 1, 0),
    # Identify 'URM female' exactly, assuming case-sensitivity
    urm_female = ifelse(tolower(demographic) == "urm female", 1, 0)
  ) %>%
  # Drop demographic column
  select('seminar_id', 'semester', 'urm', 'female', 'urm_female', 'region')

# Analyze Gender of Speakers 
gender <- df %>%
  group_by(female) %>%
  summarise(Speakers = n())

gender_by_region <- df %>%
  group_by(region, female) %>%
  summarise(Speakers = n())

gender_over_time <- df %>%
  group_by(semester, female) %>%
  summarise(Speakers = n())

gender_by_region_over_time <- df %>%
  group_by(semester, region, female) %>%
  summarise(Speakers = n())

# Analyze URM of Speakers 
urm <- df %>%
  group_by(urm) %>%
  summarise(Speakers = n())

urm_by_region <- df %>%
  group_by(region, urm) %>%
  summarise(Speakers = n())

urm_over_time <- df %>%
  group_by(semester, urm) %>%
  summarise(Speakers = n())

urm_by_region_over_time <- df %>%
  group_by(semester, region, urm) %>%
  summarise(Speakers = n())

# Analyze Demographic of Speakers 
demographic <- df %>%
  group_by(urm, female) %>%
  summarise(Speakers = n())

demographic_by_region <- df %>%
  group_by(region, urm, female) %>%
  summarise(Speakers = n())

demographic_over_time <- df %>%
  group_by(semester, urm, female) %>%
  summarise(Speakers = n())

demographic_by_region_over_time <- df %>%
  group_by(semester, region, urm, female) %>%
  summarise(Speakers = n())

# Analyze seminars with at least one female
at_least_one_female <- df %>%
  group_by(seminar_id) %>%
  filter(female==1) %>%
  summarise(Seminars = n())

at_least_one_female_by_region <- df %>%
  group_by(seminar_id, region) %>%
  filter(female==1) %>%
  summarise(Seminars = n())

at_least_one_female_over_time <- df %>%
  group_by(semester, seminar_id) %>%
  filter(female==1) %>%
  summarise(Seminars = n())

at_least_one_female_by_region_over_time <- df %>%
  group_by(semester, seminar_id, region) %>%
  filter(female==1) %>%
  summarise(Seminars = n())

# Analyze seminars with at least one urm
at_least_one_urm <- df %>%
  group_by(seminar_id) %>%
  filter(urm==1) %>%
  summarise(Seminars = n())

at_least_one_urm_by_region <- df %>%
  group_by(seminar_id, region) %>%
  filter(urm==1) %>%
  summarise(Seminars = n())

at_least_one_urm_over_time <- df %>%
  group_by(semester, seminar_id) %>%
  filter(urm==1) %>%
  summarise(Seminars = n())

at_least_one_urm_by_region_over_time <- df %>%
  group_by(semester, seminar_id, region) %>%
  filter(urm==1) %>%
  summarise(Seminars = n())

# Analyze seminars with at least one urm female
at_least_one_urm_female <- df %>%
  group_by(seminar_id) %>%
  filter(urm_female==1) %>%
  summarise(Seminars = n())

at_least_one_urm_female_by_region <- df %>%
  group_by(seminar_id, region) %>%
  filter(urm_female==1) %>%
  summarise(Seminars = n())

at_least_one_urm_female_over_time <- df %>%
  group_by(semester, seminar_id) %>%
  filter(urm_female==1) %>%
  summarise(Seminars = n())

at_least_one_urm_female_by_region_over_time <- df %>%
  group_by(semester, seminar_id, region) %>%
  filter(urm_female==1) %>%
  summarise(Seminars = n())

#### Save data ####
write_csv(df, "data/analysis_data/cleaned_raw_data.csv")

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


