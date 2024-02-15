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

clean_data <-
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
  ) %>%
  # Drop demographic column
  select('seminar_id', 'semester', 'urm', 'female', 'region') %>%
  mutate(region = ifelse(is.na(region), "Unknown", region))

#### Save data ####
write_csv(clean_data, "data/clean_data/clean_data.csv")