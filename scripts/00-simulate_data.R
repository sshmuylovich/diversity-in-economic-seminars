#### Preamble ####
# Purpose: Simulates datasets regarding economic seminar speaker diversity from 2014 to 2019
# Author: Sima Shmuylovich
# Date: 15 February 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run "import_packages.R" before running this script.

#### Loading Packages ####
library(tidyverse)

#### Simulate data ####
set.seed(483)

### Global Variables ###
semester_options <- as.Date(c("2014-01-01", "2014-08-01",
                              "2015-01-01", "2015-08-01",
                              "2016-01-01", "2016-08-01",
                              "2017-01-01", "2017-08-01",
                              "2018-01-01", "2018-08-01",
                              "2019-01-01", "2019-08-01"))
urm_options <- c("URM", "Non-URM")
gender_options <- c("Female", "Male")
region_options <- c("Midwest", "Northeast", "South", "International", "West")
num_rows <- 100

## Dataset: Simulate seminar id, semester, urm status, gender, and host department geographical region. 
# Expected Columns: seminar_id | semester | urm | gender | region
speaker_diversity_data_simulation <-
  tibble(
    # Sequence of <seminar_id>. Random variables representing the id of the seminar a speaker spoke at.
    # Use Uniform Distribution
    # Round Random variables to whole numbers
    seminar_id = round(runif(n=num_rows, min = 1, max = 15000)),
    # Sequence of <semester>. Simulated from semester_options. 
    semester = sample(semester_options, num_rows, replace = TRUE),
    # Sequence of <urm>. Simulated from urm_options 
    urm = sample(urm_options, num_rows, replace = TRUE),
    # Sequence of <gender>. Simulated from gender_options 
    gender = sample(gender_options, num_rows, replace = TRUE),
    # Sequence of <region>. Simulated from region_options 
    region = sample(region_options, num_rows, replace = TRUE)
  )
# Create table for the simulated data
speaker_diversity_data_simulation <- speaker_diversity_data_simulation %>% mutate(demographic = case_when(
  urm == "Non-URM" & gender == "Male"~ "Non-URM Male",
  urm == "Non-URM" & gender == "Female"~ "Non-URM Female",
  urm == "URM" & gender == "Male"~ "URM Male",
  urm == "URM" & gender == "Female"~ "URM Female"
))

# Dataset 1: Data Validation/Tests
# 1. <sequence_id> is of type integer or double
speaker_diversity_data_simulation$seminar_id %>% typeof() %in% c("double", "integer") == TRUE

# 2. <sequence_id> is positive
speaker_diversity_data_simulation$seminar_id %>% min() >= 0

# 3. <semester> is between 2014 and 2019 inclusive
speaker_diversity_data_simulation$semester %>% min() >= as.Date("2014-01-01")
speaker_diversity_data_simulation$semester %>% min() <= as.Date("2020-01-01")

# 4. <semester> MM-DD is either 01-01 or 08-01
all(speaker_diversity_data_simulation$semester %>% format("%m") %in% c("01", "08"))
all(speaker_diversity_data_simulation$semester %>% format("%d") == "01")

# 5. <urm> is as expected
all(speaker_diversity_data_simulation$urm %in% c("URM", "Non-URM"))

# 6. <gender> is as expected
all(speaker_diversity_data_simulation$gender %in% c("Female", "Male"))

# 7. <region> is as expected
all(speaker_diversity_data_simulation$region %>% unique() %in% c("Midwest", "Northeast", "South", "International", "West"))

#8. <demographic> is as expected
all(speaker_diversity_data_simulation$demographic %>% unique() %in% c("Non-URM Male", "Non-URM Female", "URM Male", "URM Female"))



