#### Preamble ####
# Purpose: Runs tests on the cleaned dataset obtained from "02-data_cleaning.R"
# to check the validity of the dataset entries.
# Author: Sima Shmuylovich
# Date: 15 February 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run "99-import_packages.R", "01-download_data.R", and
# "02-data_cleaning.R" before running this script.

#### Workspace setup ####
library(tidyverse)

#### Test data ####
cleaned_data_test <- read_csv("data/clean_data/clean_data.csv")

# Dataset 1: Data Validation/Tests
# 1. <sequence_id> is of type character
cleaned_data_test$seminar_id %>% typeof() %in% c("character") == TRUE

# 2. <semester> is between 2014 and 2019 inclusive
cleaned_data_test$semester %>% min() >= as.Date("2014-01-01")
cleaned_data_test$semester %>% min() <= as.Date("2020-01-01")

# 3. <semester> MM-DD is either 01-01 or 08-01
all(cleaned_data_test$semester %>% format("%m") %in% c("01", "08"))
all(cleaned_data_test$semester %>% format("%d") == "01")

# 4. <urm> is as expected
all(cleaned_data_test$urm %in% c("URM", "Non-URM"))

# 5. <gender> is as expected
all(cleaned_data_test$gender %in% c("Female", "Male"))

# 6. <region> one of the following: Midwest, Northeast, South, International, West
all(cleaned_data_test$region %>% unique() %in% c("Midwest", "Northeast", "South", "International", "West"))

#7. <demographic> is as expected
all(cleaned_data_test$demographic %>% unique() %in% c("Non-URM Male", "Non-URM Female", "URM Male", "URM Female"))





