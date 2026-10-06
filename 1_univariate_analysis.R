# every script should be self inclusive. Include libraries and data for each script. 
library(tidyverse)
library(here)
library(naniar)
cps_untidy <- read_csv(here("data/raw/Chicago_public_schools.csv"))
chicago_community_data <- read_csv(here("data/raw/Chicago_Demographic_data.csv")) 
# cps_cleaned <- read_rds(here("data/Chicago_Public_School_cleaned.rds"))
# cps_survey <- read_rds(here("data/Chicago_Public_School_tidy_with_survey_results.rds"))
# cps_joined <- read_rds(here("data/cps_community_joined_tidy_dataset.rds"))
zip_joined <- read_rds(here("data/cps_and_demographic_cleaned.rds"))

# first research question -------------------------------------------------
missingness_overall <- zip_joined|>
  gg_miss_var() + 
  labs(
    title = "General Missingness in Chicago Public School's Data",
    subtitle = "Collected by City of Chicago"
  )

missingness_top <- zip_joined |> 
  filter(primary_category == "HS")|>
  miss_var_summary() |>
  filter(n_miss > 20)

distribution_college_enroll <- zip_joined |> 
  ggplot(aes(x = college_enrollment_school_pct)) +
  geom_histogram(fill = "darkcyan", color = "darkslategray") + 
  labs(
    title = "Distribution of College Enrollment Rate", 
    subtitle = "Based on Chicago Public Schools 2023 Data",
    x = "College Enrollment Rate", 
    y = "Count"
  ) +
  theme_bw() 

college_enrollment_top <- zip_joined |> 
  drop_na() |> 
  filter(primary_category == "HS", year == "2023") |>
  arrange(desc(college_enrollment_school_pct)) |> 
  select(short_name, school_type, college_enrollment_school_pct) |>
  slice_head(n = 10)

college_enrollment_top <- zip_joined |> 
  drop_na() |> 
  filter(primary_category == "HS", year == "2023") |>
  arrange(desc(college_enrollment_school_pct)) |> 
  select(short_name, income_level, college_enrollment_school_pct) |>
  slice_head(n = 10)

cps_by_school_type <- zip_joined |> 
  filter(primary_category == "HS") |>
  summarize(count = n(), .by = school_type) |> 
  arrange(desc(count))

# Graduation Rate Analysis ------------------------------------------------
zip_joined |>
  group_by(school_type) |>
  select(school_type, graduation_4_year_school_pct) |> 
  arrange(desc(graduation_4_year_school_pct))

zip_joined |> 
  ggplot(aes(x = graduation_4_year_school_pct, color = school_type)) +
  geom_freqpoly()

zip_joined |> 
  ggplot(aes(x = graduation_4_year_school_pct)) +
  geom_histogram(fill = "darkcyan", color = "darkslategray") + 
  labs(
    title = "Distribution of Graduation in 4 Years Rate", 
    subtitle = "Based on Chicago Public Schools 2022-2023 Data",
    x = "4 Year Graduation Rate", 
    y = "Count"
  ) +
  theme_bw() 

zip_joined |> 
  ggplot(aes(x = graduation_5_year_school_pct)) +
  geom_histogram()

# College Enrollment Rate Analysis ----------------------------------------

# College Retainment Analysis ---------------------------------------------
zip_joined |> 
  ggplot(aes(x = graduation_5_year_school_pct, y = college_persistence_school_pct)) +
  geom_point()

zip_joined |> 
  ggplot(aes(x = graduation_4_year_school_pct, y = college_persistence_school_pct)) +
  geom_point()

## I will now take out variables that don't have a strong association to my varibles exploring
zip_joined |> 
  select(long_name, school_type, primary_category, year, college_persistence_school_pct, college_enrollment_school_pct, 
         graduation_4_year_school_pct, graduation_5_year_school_pct, 
         graduation_4_year_school_pct, one_year_dropout_rate, student_attendance, 
         suspensions_per_100_students, sat_grade_11_score_school_avg, 
         chronic_truancy_pct, mobility_rate_pct)

# first step of anaylisis -------------------------------------------------
zip_joined |> 
  filter(primary_category == "HS") |> 
  select(school_id, short_name, graduation_4_year_school_pct, 
         college_enrollment_school_pct, college_persistence_school_pct, year) |> 
  ggplot(aes(x = graduation_4_year_school_pct, y = college_persistence_school_pct, 
             color = year)) +
  geom_point()

zip_joined |> 
  filter(primary_category == "HS") |> 
  drop_na() |> 
  ggplot(aes(x = student_attendance, y = college_enrollment_school_pct, 
             color = year)) +
  geom_point()

# cps_pivoted |> 
#   select(-(school_survey_involved_families:school_survey_safety)) |>
#   gg_miss_var() + 
#   labs(
#     title = "General Missingness in Chicago Public School's Data", 
#     subtitle = "Collected by City of Chicago"
#   )

# cps_tidy_w_survey |> 
#   gg_miss_var() + 
#   labs(
#     title = "General Missingness in Chicago Public School's Data", 
#     subtitle = "Collected by City of Chicago"
#   )

zip_joined |> 
  filter(primary_category == "HS", sat_grade_11_score_school_avg > 0) |> 
  ggplot(aes(x = sat_grade_11_score_school_avg)) +
  geom_histogram(bins = 20)

zip_joined |> 
  filter(primary_category == "HS", sat_grade_11_score_school_avg > 0) |> 
  ggplot(aes(x = sat_grade_11_score_school_avg, y = student_attendance, 
             color = year)) +
  geom_point()

zip_joined |> 
  filter(primary_category == "HS", sat_grade_11_score_school_avg > 0) |> 
  ggplot(aes(x = student_attendance, y = college_persistence_school_pct)) +
  geom_point() +
  facet_wrap(~year)

# zip_joined |>
#   distinct(long_name) |>
#   summarize(avg_suspensions = mean(suspensions_per_100_students), .by = school_type)
# 
# zip_joined |> 
#   filter(primary_category == "HS", sat_grade_11_score_school_avg == 0) |> 
#   distinct(short_name)

