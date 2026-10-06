# every script should be self inclusive. Include libraries and data for each script. 

# libraries 
library(tidyverse)
library(here)
library(naniar)

# datasets
cps_untidy <- read_csv(here("data/raw/Chicago_public_schools.csv"))
chicago_community_data <- read_csv(here("data/raw/Chicago_Demographic_data.csv")) 
# cps_cleaned <- read_rds(here("data/Chicago_Public_School_cleaned.rds"))
# cps_survey <- read_rds(here("data/Chicago_Public_School_tidy_with_survey_results.rds"))
# cps_joined <- read_rds(here("data/cps_community_joined_tidy_dataset.rds"))

# Data Cleaning CPS Data --------------------------------------------------
# We are first going to clean the cps_untidy data
# This data set has 183 columns with many that need to be pivoted.
# There are also some columns that are NULL for all observations. These can be selected out. 
# These columns include: growth_reading_grades_tested_pct_es, growth_reading_grades_tested_label_es, 
# growth_math_grades_tested_pct_es, growth_math_grades_tested_label_es, attainment_reading_pct_es,
# attainment_reading_lbl_es, attainment_math_pct_es, attainment_math_lbl_es,
# school_survey_parent_response_rate_pct, school_survey_parent_response_rate_avg_pct,

# This can be proved by proportion of missingness to whole values. 
cps_only_null <- cps_untidy |>
  select(where(~ n_distinct(.x, na.rm = TRUE) == 0)) 

# We then also took out all columns that only had one observation as it doesn't 
# show any trends and can be observed in the written analysis. 
cps_one_value <- cps_untidy |>
  rename_all(tolower) |>
  select(where(~ n_distinct(.x, na.rm = TRUE) == 1) & -c(city, progress_report_year))

# Now we can make the data tidy
# first we select out these null columns and the columns with one value
# I also selected out some other columns that did not provide any interesting 
# information related to my project. 
cps_tidy <- cps_untidy |>
  rename_all(tolower) |>
  select(where(~ n_distinct(.x, na.rm = TRUE) > 0) & -((where(~ n_distinct(.x, na.rm = TRUE) == 1) 
                                                        & -c(city, progress_report_year))) & 
           -c(fax, blue_ribbon_award_year:excellence_award_year, 
         other_metrics_year_1, other_metrics_year_2,
         healthy_school_certification, healthy_school_certification_description, 
         student_growth_description, student_growth_description, student_attainment_description, 
         creative_school_certification_description, supportive_school_award_desc, 
         location))

# Now we pivot our table. There are many columns were the data is separated into 
# year 1 and year 2 with year 1 as 2022 and year 2 as 2023. 
cps_pivoted <- cps_tidy |>
  pivot_longer(
    cols = contains("year_"),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_year_(\\d+)(?:_pct)?$"
  ) |> 
  mutate(year = ifelse(year == "1", "2022", "2023")) |> 
  select(-c("school_survey_involved_families":"school_survey_safety")) |> 
  relocate(year)

 

# Now I am going to make a separate cleaned cps dataset with only the results from the survey. 
# cps_tidy_w_survey <- cps_pivoted |> 
#   pivot_longer(
#     cols = ("school_survey_involved_families":"school_survey_safety"), 
#     names_to = "survey_category", 
#     values_to = "survey_rating", 
#   ) |> 
#   mutate(
#     survey_category = (case_when(
#       survey_category == "school_survey_involved_families" ~ "Involved Families", 
#       survey_category == "school_survey_supportive_environment" ~ "Supportive Environment", 
#       survey_category == "school_survey_ambitious_instruction" ~ "Ambitious Instruction", 
#       survey_category == "school_survey_effective_leaders" ~ "Effective Leaders",
#       survey_category == "school_survey_collaborative_teachers" ~ "Collaborative Teachers",
#       survey_category == "school_survey_safety" ~ "Safety")), 
#     survey_rating = (case_when(
#       survey_rating == "NEUTRAL" | survey_rating == "Neutral" ~ "Neutral", 
#       survey_rating == "VERY WEAK" | survey_rating == "Very Weak" ~ "Very Weak",
#       survey_rating == "Strong" | survey_rating == "STRONG" ~ "Strong",
#       survey_rating == "Weak" | survey_rating ==  "WEAK" ~ "Weak",
#       survey_rating == "NOT ENOUGH DATA" ~ NA,
#       survey_rating == "Very Strong" | survey_rating == "VERY STRONG" ~ "Very Strong"
#     ))
#   ) |>
#   select(school_id, long_name, school_type, primary_category, zip, survey_category, survey_rating)

# write_rds(cps_tidy_w_survey, "data/Chicago_Public_School_tidy_with_survey_results.rds") 

# This is the most up to date tidied form of cps data. 
# It includes the result of the survey in a more legible way 
# and includes only variables that might be of use in exploration. 
# Further specific subsets of this data will be created when comparing specific relations

#Now I will make a code book to include all of the data including description variables 
# and one distinct value varibles. 
cps_tidy_w_survey |> 
  distinct(school_type)
cps_tidy_w_survey |> 
  distinct(primary_category)

# Data Cleaning Chicago Community Data -----------------------------------------------
# chicago_community_data <- read_rds(here("data/raw/Chicago_community_dataset.rds"))
# This data set was joined and initially cleaned in 0b_spatial_key.R
# chicago_community_tidy_data <- read_rds(here("data/chicago_community_tidy_by_zip.rds"))

# Now I will create specific datasets that show the avg income levels, avg age 
# ranges, and race/ethnicity levels. It should be noted that age ranges that were 
# intially separated by gender in the dataset were combined by age ranges as gender 
# is not a demographic to be explored in my analysis

chicago_avg_income_by_zip <- chicago_community_tidy |> 
  pivot_longer(
    cols = (avg_under_25_000:avg_125_000),
    names_to = "income_level",
    values_to = "income_population"
  ) |> 
  select(zip, income_level, income_population)
# write_rds(chicago_avg_income_by_zip, "data/chicago_community_avg_income_by_zip.rds")

chicago_avg_age_by_zip <- chicago_community_tidy |> 
  pivot_longer(
    cols = (avg_0_to_17:avg_over_65),
    names_to = "age_range",
    values_to = "age_population"
  ) |> 
  select(zip, age_range, age_population)
# write_rds(chicago_avg_age_by_zip, "data/chicago_community_avg_age_by_zip.rds")

chicago_avg_race_ethnicity_by_zip <- chicago_community_tidy |> 
  pivot_longer(
    cols = (avg_white:avg_hispanic_or_latino),
    names_to = "race_ethnicity",
    values_to = "race_ethnicity_population"
  ) |> 
  select(zip, race_ethnicity, race_ethnicity_population) 
# write_rds(chicago_avg_race_ethnicity_by_zip, "data/chicago_avg_race_ethnicity_by_zip.rds")

# Now that the Chicago community dataset is tidied. I will be joining it to the CPS dataset.
CPS_joined <- cps_cleaned |> 
  left_join(chicago_community_tidy_data, join_by(zip == zip))

# write_rds(CPS_joined, "data/cps_community_joined_tidy_dataset.rds")

# by zip and income -------------------------------------------------------
zip_income <- chicago_community_avg_income_by_zip |> 
  group_by(zip) |> 
  arrange(desc(income_population)) |> 
  slice_head() |> 
  mutate(zip = as.numeric(zip))

zip_joined <- cps_cleaned |> 
  left_join(zip_income, join_by(zip == zip))

# write_rds(zip_joined, "data/cps_and_demographic_cleaned.rds")

# codebook ----------------------------------------------------------------
cps_and_demographic_codebook <- tibble(
  variables = colnames(zip_joined),
  desciption = c("character, year of reported data", 
                 "double, unique id for each school", 
                 "character, shorten name of school", 
                 "character, official name of school", 
                 "character, type of school with 14 options including: Charter, Contract, Citywide-Option, 
    Career academy, Selective enrollment, Neighborhood, Magnet, Special Education, Military academy, 
    Regional gifted center, Classical, Small, NA, and Virtual", 
                 "character, school education level with 3 options HS (high school), MS (middle school), and ES (elementary school).
address: character, school's address", 
                 "address: character, school's address", 
                "character, school's city",
               "character, school's state",
               "double, school's zip code", 
              "double, school's phone number without any punctuation",
              "character, school's profile on cps website",
              "character, school's personal website",
              "double, year of report, which for this data set is 2024",
             "character, Student Attainment measures how well the school performed 
                 on standardized tests at a single point in time. This school's score is based on 
attainment relative to the combined College Readiness Benchmark scores, set by College Board.", 
             "character, Results are based on student and teacher responses to 
             the My Voice, My School 5Essentials survey.", 
             "double, percent of students who responded to the survey", 
             "double, percent of teacher who responded to the survey",
             "character, the school's rating in the arts with five options based on whether it meets the goals and priorities outlined in the CPS Arts Education Plan, 
  including Staffing & Instruction, Partnerships, Community & Culture, and Budget 
  & Planning.",
             "character, url to the school's report card administred by cps, (not all links work)", 
             "double, percent based on the number of times students enroll in or leave a school 
             during the school year. Students who left more than once were counted multiple times.",
             "double, percentage of students with low attendance over extended periods of time. A chronic truant student is one who misses 5 percent of 
school days within an academic year without a valid excuse. That’s nine days of 
an average 180-day school year.",
             "character, states weather the school has put in place 
systems and structures to support social and emotional learning (SEL). 
This may include SEL action plans, ongoing training for teachers and staff, 
partnerships with community organizations, time for SEL in the master schedule, 
and/or targeted and intensive SEL services.",
             "double, school's latitude",
             "double, school's longitude", 
             "double, average sat score of grade 11 students with the average sat score across cps being 933.", 
             "school's proportion of student suspended for every 100 students, with 4.2 as cps average.", 
             "double, school's proportion of misconducts reported to number of suspensions, with 10.8 as cps average. 
This shows how often a misconduct may lead to a suspension ",
             "double, average length of one suspension in days for each school, 
with 2.0 as cps average.",
             "double, average student attendance out of 100, with 88.3 as cps average.",
             "double, average teacher attendance out of 100, with 94 as cps average.",
             "double, Dropouts include students in grades 9-12 whose 
names have been removed from the district-housed roster for any reason other 
than death, extended illness, graduation/completion of a program of study, 
transfer to another public/private school or expulsion, with 5.3 as cps average.", 
             "double, percent of freshman on-track, 
with average 88.7 cps students on track in 2022 and an average of 88.8 cps 
students on track in 2023. A student is on-track if they earn at least five 
full-year course credits and no more than one semester F in a core course in 
their first year of high school", 
             "double, percent of students who graduated in four year, with 
with 82.9 as cps average in 2022 and 84 as cps average in 2023.", 
             "double, percent of students who graduate within
five years of the start of their freshman year, with 84 as cps average in 2022 
and 85.6 as cps average in 2023.",
             "double, measures the percentage of students graduating 
from CPS in the previous year who enrolled in a 2-year or 4-year college in the 
fall or spring after graduation from high school, with 61.8 as cps average in 2022 
and 65.2 as cps average in 2023.",
             "double, measures the percentage of students 
enrolled in college in the fall or spring after graduation from high school 
that remain enrolled in college the following fall or spring, with 73.4 as cps average in 2022 
and 71.2 as cps average in 2023.", 
             "character, most promenient income level of the zip code the school resides in.", 
             "double, number of population in the zipcode who fall within themost promenient income level of the zip code"
                 ))

write_csv(cps_and_demographic_codebook, "data/cps_and_demographic_codebook.csv")

# scratch work ------------------------------------------------------------
# In this data set, I hope to explore the relation between graduation rate, 
# location, and school type. 
# How does a school location in Chicago affect their graduation rate? 
# How does the school safety score or family involvement score affect it? 
# What variables could school target to improve things such a student attendance, 
# test scores, and college enrollment rate. 
# There are now 95 columns that have at least one distinct value. 
# There are some columsn that have general information about cps public schools such as 
# listed above. These columns only held on distinct value and offered general information about cps 
# that can exist in the analysis but has not need to exist in the table
# Now there are many columns that share purpose but with one for year 1 and one for year 2. We will pivot this. 
# cps_tidy <- cps_untidy |>
#   rename_all(tolower) |>
#   select(where(~ n_distinct(.x, na.rm = TRUE) > 0) & -((where(~ n_distinct(.x, na.rm = TRUE) == 1) 
#                                                         & -c(city, progress_report_year))) & 
#            -c(fax, blue_ribbon_award_year:excellence_award_year, 
#               other_metrics_year_1, other_metrics_year_2,
#               healthy_school_certification, healthy_school_certification_description, 
#               student_growth_description, student_growth_description, student_attainment_description, 
#               creative_school_certification_description, supportive_school_award_desc, 
#               location))

# Now we pivot our table. There are many columns were the data is separated into 
# year 1 and year 2 with year 1 as 2022 and year 2 as 2023. 
# cps_pivoted <- cps_tidy |>
#   pivot_longer(
#     cols = contains("year_"),
#     names_to = c(".value", "year"),
#     names_pattern = "(.*)_year_(\\d+)(?:_pct)?$"
#   ) |> 
#   mutate(year = ifelse(year == "1", "2022", "2023"))
# 
# write_csv(cps_pivoted, "data/Chicago_Public_School_cleaned.csv") 

# Notes 
## Use here() to wrap the code read in if using sub directories to reference 
## everything from project level. 


