# Niko Ilomäki (niko.ilomaki@helsinki.fi)
# March 8, 2017
# Reads the full dataset into R, calculates some averages and sums and joins the result into single data.frame that is saved as a serialized R object (.Rds)

library(magrittr)
library(tibble)
library(dplyr)
library(tidyr)

# Read individual tables into R
# Although some variables could easily be thought of as factors...
# ...stringsAsFactors = FALSE makes things easier, because some numeric variables are presented as characters in the data
assessments <- read.csv("data/assessments.csv", stringsAsFactors = FALSE) %>% mutate(date = as.numeric(date))
courses <- read.csv("data/courses.csv", stringsAsFactors = FALSE)
studentAssessment <- read.csv("data/studentAssessment.csv", stringsAsFactors = FALSE) %>% mutate(score = as.numeric(score))
studentInfo <- read.csv("data/studentInfo.csv", stringsAsFactors = FALSE)
studentRegistration <- read.csv("data/studentRegistration.csv", stringsAsFactors = FALSE) %>% mutate(date_registration = as.numeric(date_registration), date_unregistration = as.numeric(date_unregistration))
vle <- read.csv("data/vle.csv", stringsAsFactors = FALSE)
studentVle <- read.csv("data/studentVle.csv", stringsAsFactors = FALSE)

# Calculate average lateness of submissions (in days) and portion of late submissions of all submissions
assessment_averages <-
  assessments %>%
  left_join(studentAssessment, by = c("id_assessment")) %>%
  filter(!is.na(date), !is.na(date_submitted), !is.na(score), is_banked == 0) %>% # remove cases where deadline or submission date is not available, or submission has been transferred from previous instance of the course
  mutate(ahead_of_time = date - date_submitted) %>%
  group_by(code_module, code_presentation, id_student) %>%
  summarise(total_exercises = n(), late_exercises = sum(ahead_of_time < 0), lateness = late_exercises/total_exercises, average_ahead_of_time = mean(ahead_of_time)) %>%
  ungroup() %>%
  select(-total_exercises, -late_exercises)

# Calculate total numbers of interactions per activity type
vle_summaries <-
  studentVle %>%
  inner_join(vle, by = c("code_module", "code_presentation", "id_site")) %>%
  group_by(code_module, code_presentation, id_student, activity_type) %>%
  summarise(interactions = sum(sum_click)) %>%
  ungroup() %>%
  spread(activity_type, interactions) %>%
  inset(is.na(.), value = 0)

socioeconomic_factors <- c("gender", "region", "highest_education", "imd_band", "age_band", "disability") # these are good to know but they are not needed for the analysis

# Combine everything into single data.frame and save it
oulad <-
  studentInfo %>% # base table
  select(-one_of(socioeconomic_factors)) %>% # remove variables unnecessary for the analysis
  inner_join(assessment_averages, by = c("code_module", "code_presentation", "id_student")) %>% # joining late submission statistics calculated above, inner join because situations where there are not any submission are not interesting for the analysis
  left_join(vle_summaries, by = c("code_module", "code_presentation", "id_student")) %>% # joining VLE interaction statistics calculated above
  select(-id_student) %>%
  inset(is.na(.), value = 0) %T>% # replace NAs (these come from left joining vle_summaries) with zeros
  saveRDS("data/oulad.Rds") # save data in serialized R object format
