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

analysed_data <- clean_data %>%
  group_by(semester) %>%
  summarise(total = n(),
            female = sum(gender == "Female"),
            male = sum(gender == "Male"),
            urm_status = sum(urm == "URM"),
            rm_status = sum(urm == "Non-URM"),
            f_urm = sum(urm == "URM" & gender == "Female"),
            m_urm = sum(urm == "URM" & gender == "Male"),
            f_rm = sum(urm == "Non-URM" & gender == "Female"),
            m_rm = sum(urm == "Non-URM" & gender == "Male")) %>%
  mutate(female_percentage = (female / total) * 100,
         male_percentage = (male / total) * 100,
         urm_status_percentage = (urm_status / total) * 100,
         rm_status_percentage = (rm_status / total) * 100,
         f_urm_percentage = (f_urm / total) * 100,
         m_urm_percentage = (m_urm / total) * 100,
         f_rm_percentage = (f_rm / total) * 100,
         m_rm_percentage = (m_rm / total) * 100)

analysed_regional_data <- clean_data %>%
  group_by(semester, region) %>%
  summarise(total = n(),
            female = sum(gender == "Female"),
            male = sum(gender == "Male"),
            urm_status = sum(urm == "URM"),
            rm_status = sum(urm == "Non-URM"),
            f_urm = sum(urm == "URM" & gender == "Female"),
            m_urm = sum(urm == "URM" & gender == "Male"),
            f_rm = sum(urm == "Non-URM" & gender == "Female"),
            m_rm = sum(urm == "Non-URM" & gender == "Male")) %>%
  mutate(female_percentage = (female / total) * 100,
         male_percentage = (male / total) * 100,
         urm_status_percentage = (urm_status / total) * 100,
         rm_status_percentage = (rm_status / total) * 100,
         f_urm_percentage = (f_urm / total) * 100,
         m_urm_percentage = (m_urm / total) * 100,
         f_rm_percentage = (f_rm / total) * 100,
         m_rm_percentage = (m_rm / total) * 100,)

#### Save data ####
write_csv(analysed_data, "data/analysis_data/analysed_data.csv")
write_csv(analysed_regional_data, "data/analysis_data/analysed_regional_data.csv")