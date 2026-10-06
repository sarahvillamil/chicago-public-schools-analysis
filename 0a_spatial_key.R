
library(tidyverse)
library(sf)

chicago_community_data <- read_csv(here("data/raw/Chicago_Demographic_data.csv"))


# zipcode shape file obtained from:
# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-ZIP-Codes-Map/gdcf-axmw
zip <- st_read("data/zipcode_chi/geo_export_962a8e6b-f275-4032-a19d-077a3250b7f5.shp")

# community shape file obtained from:
# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Neighborhoods/bbvz-uum9
nbr <- st_read("data/neighborhoods_chi/geo_export_0f2966c0-b273-4b8a-a06b-7c36ba616e6f.shp")

# check compatibility
st_crs(zip)
st_crs(nbr)
# same coord system: wgs84

# Join the datasets based on intersecting polygons
zip_to_comm <- st_join(zip, 
                       nbr, 
                       join = st_intersects)

# This returns each ZIP polygon with the community area attributes added.
# One ZIP may match multiple community areas
# One community area may match multiple ZIPs

# Condense to relevant columns
data_key <- zip_to_comm |> 
  select(zip, pri_neigh, sec_neigh) |>
  mutate(pri_neigh = tolower(pri_neigh)) 

# you will need to think about how to handle the multiple matches;
# perhaps using averages over all matches
# I joined chicago community data to the zip codes by neighborhoods.
chicago_joined <- chicago_community_data |> 
  janitor::clean_names() |>
  mutate(community_area = tolower(community_area)) |>
  left_join(data_key, join_by(community_area== pri_neigh)) |> 
  select(-c(geometry, record_id, acs_year, sec_neigh))

# I then took the joined data set to avg all the values by zip codes so it was 
# organized by zipcode and not by neighborhood. 
chicago_community_tidy <- chicago_joined |>
  summarize(
    avg_under_25_000 = mean(under_25_000), 
    avg_25_000_to_49_999 = mean(x25_000_to_49_999), 
    avg_50_000_to_74_999 = mean(x50_000_to_74_999),
    avg_75_000_to_125_000 = mean(x75_000_to_125_000),
    avg_125_000 = mean(x125_000),
    avg_0_to_17 = mean(male_0_to_17 + female_0_to_17),
    avg_18_to_24 = mean(male_18_to_24 + female_18_to_24),
    avg_male_25_to_34 = mean(male_25_to_34 + female_25_to_34),
    avg_35_to_49 = mean(male_35_to_49 + female_35_to_49),
    avg_50_to_64 = mean(male_50_to_64 + female_50_to_64),
    avg_over_65 = mean(male_65 + female_65),
    avg_white = mean(white), 
    avg_black_or_african_american = mean(black_or_african_american),
    avg_indigenious_to_alaska_and_america= mean(american_indian_or_alaska_native),
    avg_asian= mean(asian), 
    avg_native_hawaiian_or_pacific_islander= mean(native_hawaiian_or_pacific_islander), 
    avg_other_race= mean(other_race), 
    avg_multiracial= mean(multiracial), 
    avg_white_not_hispanic_or_latino = mean(white_not_hispanic_or_latino),
    avg_hispanic_or_latino = mean(hispanic_or_latino),
    .by = zip
  ) 

write_rds(chicago_community_tidy, "data/chicago_community_tidy_by_zip.rds")