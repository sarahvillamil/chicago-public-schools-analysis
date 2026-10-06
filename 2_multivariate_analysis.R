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

#  second research question-----------------------------------------------
cps_by_school_type <- zip_joined |> 
  filter(primary_category == "HS") |>
  summarize(avg_graduation_5_rate = mean(graduation_4_year_school_pct, na.rm = TRUE),
            avg_graduation_4_rate = mean(graduation_5_year_school_pct, na.rm = TRUE),
            avg_college_persistance = mean(college_persistence_school_pct, na.rm = TRUE), 
            avg_college_enrollment = mean(college_enrollment_school_pct, na.rm = TRUE), 
            avg_sat = mean(sat_grade_11_score_school_avg, na.rm = TRUE), 
            avg_student_attendance = mean(student_attendance, na.rm = TRUE),
            count = n(), .by = school_type) 

## avg_college enrollment by school type
avg_college_by_type_graph <- zip_joined |> 
  filter(primary_category == "HS")|> 
  summarize(mean_college_enrollment = mean(college_enrollment_school_pct, na.rm = TRUE), .by = school_type) |>
  ggplot(aes(x = school_type, y = mean_college_enrollment)) +
  geom_col(fill = "darkcyan", color = "darkslategray") + 
  labs(
    title = "Average Percent of College Enrollment by School Type", 
    subtitle = "Based on Chicago Public Schools 2022-2023 Data",
    x = "Type of School", 
    y = "Average Percent of College Enrollment"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 


## avg_college persistence by school type
avg_college_persistence_by_type_graph <- zip_joined |> 
  filter(primary_category == "HS")|> 
  summarize(mean_college_persistence = mean(college_persistence_school_pct, na.rm = TRUE), .by = school_type) |>
  ggplot(aes(x = school_type, y = mean_college_persistence)) +
  geom_col(fill = "darkcyan", color = "darkslategray") + 
  labs(
    title = "Average Percent of College Enrollment by School Type", 
    subtitle = "Based on Chicago Public Schools 2022-2023 Data",
    x = "Type of School", 
    y = "Average Percent of College Persistence"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 


## college persisitnace to college enrollment by school type
enrollment_vs_persisitance_by_type_graph <- zip_joined |> 
  filter(primary_category == "HS") |>
  ggplot(aes(x = college_enrollment_school_pct, y = college_persistence_school_pct, color = school_type)) +
  geom_point() +
  labs(
    title = "Relation of College Enrollment to College Persistance Rates by School Type", 
    subtitle = "Based on Chicago Public Schools 2022-2023 Data",
    x = "College Enrollment Percent", 
    y = "College Persistance Percent", 
    color = "School Type"
  ) 
theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

## graduation 4 years to college enrollment by school type
four_year_vs_enrollment_by_type_graph <- zip_joined |> 
  filter(primary_category == "HS") |>
  ggplot(aes(x = graduation_4_year_school_pct, y = college_enrollment_school_pct, color = school_type)) +
  geom_point() +
  labs(
    title = "Relation of College Enrollment to 4 Year Graduation Rate", 
    subtitle = "Based on Chicago Public Schools 2022-2023 Data",
    x = "4 Year Graduation Rate", 
    y = "College Enrollment Percent", 
    color = "School Type"
  ) +
theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

college_enrollment_top <- zip_joined |> 
  drop_na() |> 
  filter(primary_category == "HS", year == "2023") |>
  arrange(desc(college_enrollment_school_pct)) |> 
  select(short_name, school_type, college_enrollment_school_pct) |>
  slice_tail(n = 10)

college_enrollment_top_bottom <- zip_joined |> 
  drop_na() |> 
  filter(primary_category == "HS", year == "2023") |>
  arrange(college_enrollment_school_pct) |> 
  select(short_name, income_level, college_enrollment_school_pct) |>
  slice_head(n = 10)

college_enrollment_by_income_level <- zip_joined |> 
  filter(primary_category == "HS") |>
  summarize(avg_college_enrollment = mean(college_enrollment_school_pct, na.rm = TRUE), .by = income_level) |> 
  arrange(avg_college_enrollment) |>
  ggplot(aes(x = income_level, y = avg_college_enrollment)) +
  geom_col(fill = "darkcyan", color = "darkslategray") + 
  labs(
    title = "Average Percent of College Enrollment by Income Level", 
    subtitle = "Based on Chicago Public Schools 2023 Data and Chicago Demographic 2023 Data",
    x = "Income Level", 
    y = "Average Percent of College Enrollment"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## graduation 5 years to college enrollment by school type
five_year_vs_enrollment_by_type_graph <- zip_joined |> 
  filter(primary_category == "HS") |>
  ggplot(aes(x = graduation_5_year_school_pct, y = college_enrollment_school_pct, color = school_type)) +
  geom_point() +
  labs(
    title = "Relation of College Enrollment to 5 Year Graduation Rate", 
    subtitle = "Based on Chicago Public Schools 2022-2023 Data",
    x = "5 Year Graduation Rate", 
    y = "College Enrollment Percent", 
    color = "School Type"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

## graduation 5 vs 4 years to college enrollment
five_year_vs_four_year_by_type_graph <- zip_joined |> 
  filter(primary_category == "HS") |>
  ggplot(aes(x = graduation_4_year_school_pct, y = graduation_5_year_school_pct, color = school_type)) +
  geom_point() +
  labs(
    title = "Relation of College Enrollment to 5 Year Graduation Rate", 
    subtitle = "Based on Chicago Public Schools 2022-2023 Data",
    x = "4 Year Graduation Rate", 
    y = "4 Year Rate", 
    color = "College Enrollment Percent"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

# looking specifically at selective enrollment ----------------------------
# zip_joined |> 
#   filter(school_type == "neighborhood enrollment") |> 
#   summarize(
#     avg_white = mean(avg_white, na.rm = TRUE), 
#     avg_black_or_african_american = mean(avg_black_or_african_american, na.rm = TRUE), 
#     avg_hispanic_or_latino = mean(avg_hispanic_or_latino, na.rm = TRUE), 
#     avg_asian = mean(avg_asian, na.rm = TRUE), 
#     avg_indigenious_to_alaska_and_america = mean(avg_indigenious_to_alaska_and_america, na.rm = TRUE), 
#     avg_multiracial = mean(avg_multiracial, na.rm = TRUE), .by = zip
#   ) 
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

# cps_tidy_numeric_only <- zip_joined |> 
#   select(is.numeric) |>  
#   select(where(~sd(., na.rm = TRUE) > 0)) |>
#   cor(use = "pairwise.complete.obs")
# 
# ggcorrplot::ggcorrplot(cps_tidy_numeric_only, lab = TRUE, digit = 1, 
#                        title = "Correlation Plot of CPS Data")
# 

# zip_joined |> 
#   filter(primary_category == "HS")|> 
#   summarize(mean_college_enrollment = mean(college_enrollment_school_pct, na.rm = TRUE), .by = school_type) |>
#   ggplot(aes(x = school_type, y = mean_college_enrollment)) +
#   geom_col(fill = "darkcyan") + 
#   labs(
#     title = "Average Percent of College Enrollment by School Type", 
#     subtitle = "Based on Chicago Public Schools 2022-2023 Data",
#     x = "Type of School", 
#     y = "Average Percent of College Enrollment"
#   ) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

# zip_joined |> 
#   filter(primary_category == "HS")|> 
#   summarize(mean_college_enrollment = mean(sat_grade_11_score_school_avg, na.rm = TRUE), .by = school_type) |>
#   ggplot(aes(x = school_type, y = mean_college_enrollment)) +
#   geom_col(fill = "darkcyan") + 
#   labs(
#     title = "Average Percent of College Enrollment by School Type", 
#     subtitle = "Based on Chicago Public Schools 2022-2023 Data",
#     x = "Type of School", 
#     y = "Average Percent of College Enrollment"
#   ) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

# 
# avg_college_persistence_by_type_graph <- zip_joined |> 
#   filter(primary_category == "HS")|> 
#   summarize(mean_college_persistence = mean(college_persistence_school_pct, na.rm = TRUE), .by = school_type) |>
#   ggplot(aes(x = school_type, y = mean_college_persistence)) +
#   geom_col(fill = "darkcyan", color = "darkslategray") + 
#   labs(
#     title = "Average Percent of College Persistence by School Type", 
#     subtitle = "Based on Chicago Public Schools 2023 Data",
#     x = "Type of School", 
#     y = "Average Percent of College Persistence"
#   ) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 


# Student Attendance by type Graph
  
attendance_by_type_graph <- zip_joined |> 
  filter(primary_category == "HS") |>
  ggplot(aes(x = student_attendance, y = college_enrollment_school_pct, color = school_type)) +
  geom_point() +
  labs(
    title = "Relation of College Enrollment to Student Attendance", 
    subtitle = "Based on Chicago Public Schools 2023 Data",
    x = "Student Attendance", 
    y = "College Enrollment", 
    color = "School Type")
