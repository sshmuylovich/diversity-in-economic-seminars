#### Preamble ####
# Purpose: Reproduces a table and graph from Doleac, Hengel, and Pancotti's paper 
# Author: Sima Shmuylovich
# Date: 15 February 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run "99-import_packages.R" and "02-data_cleaning.R" before running this script.

library(knitr)
library(tidyverse)

# Plot theme.
theme <- theme_set(theme_light())
theme <- theme_update(
  legend.position="bottom",
  legend.title = element_blank(),
  legend.text=element_text(colour="gray25", size=7),
  panel.border=element_blank(),
  legend.background=element_rect(colour = "transparent", fill=NA),
  legend.key.size=unit(0.45, "cm")
)

#### Reproduce data ####
# Import clean data
clean_data <- read_csv("data/clean_data/clean_data.csv", col_names=TRUE, col_types=cols())

# Analyze data
demographics_presentations <- clean_data %>%
  group_by(urm, female)%>%
  tally() %>%
  mutate(percent = n/sum(n), dimension="presentation")

demographics_seminars <- clean_data %>%
  distinct(seminar_id, urm, female) %>%
  group_by(urm, female) %>%
  tally() %>%
  mutate(percent = n/sum(n), dimension="seminar")

demographics <- demographics_presentations %>% 
  bind_rows(demographics_seminars) %>% 
  mutate(urm_status = case_when(
    urm == 0 & female == 0~ "Non-URM male",
    urm == 0 & female == 1~ "Non-URM female",
    urm == 1 & female == 0~ "URM male",
    urm == 1 & female == 1~ "URM female"
  )) %>%
  group_by(urm_status) %>%
  select(urm_status, n, percent, dimension) %>%
  mutate(variable="demographics") %>%
  rename(label=urm_status)

regions_presentations <- clean_data %>%
  group_by(region)%>%
  tally() %>%
  mutate(percent = n/sum(n), dimension="presentation")

regions_seminars <- clean_data %>%
  distinct(seminar_id, region) %>%
  group_by(region) %>%
  tally() %>%
  mutate(percent = n/sum(n), dimension="seminar")

regions <- regions_presentations %>% 
  bind_rows(regions_seminars) %>% 
  group_by(region) %>%
  select(region, n, percent, dimension) %>%
  mutate(variable="regions") %>%
  rename(label=region)

# Table
n <- data.frame(
  dimension = c("presentation", "seminar"),
  variable = c("n", "n"),
  label = c("\\(N\\)", "\\(N\\)"),
  n = c(nrow(clean_data), length(unique(clean_data$seminar_id)))
)


write_csv(
  n %>% 
    bind_rows(demographics) %>% 
    bind_rows(regions),
  "data/reproduction_data/table-1.csv"
)


# Figure
demo_category <- clean_data %>%
  mutate(urm_status = case_when(
    urm == 0 & female == 0~ "Non-URM male",
    urm == 0 & female == 1~ "Non-URM female",
    urm == 1 & female == 0~ "URM male",
    urm == 1 & female == 1~ "URM female"
  )) %>%
  select(seminar_id, semester, urm_status) %>%
  group_by(semester, seminar_id, urm_status) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup() %>%
  complete(seminar_id, nesting(semester, urm_status), fill=list(n=0, percent=0)) %>%
  group_by(semester, urm_status) %>%
  summarise('Mean' = mean(percent), 'Median' = median(percent)) %>%
  pivot_longer(c('Mean', 'Median'), names_to="type", values_to="percent")

write_csv(demo_category, "data/reproduction_data/figure-1.csv")

figure1.data <- read_csv("data/reproduction_data/figure-1.csv", col_names=TRUE, col_types=cols())
figure1.plot <- ggplot(data=figure1.data, aes(x=semester, y=percent, linetype=type)) +
   geom_line() +
   facet_wrap(~ urm_status, ncol=2, scales="free_y") +
   labs(x = "Semester", y = "Percent")
 ggsave(figure1.plot, file="data/reproduction_data/figure-1.png")

