rm(list = ls())
library(tidyverse)

race_vars <- c(
  "total_pop", "white", "black", "native", "asian", "pacific",
  "other_race", "two_or_more", "hispanic", "not_hispanic"
)
pct_race <- c(
  "pct_white", "pct_black", "pct_native", "pct_asian", "pct_pacific",
  "pct_other_race", "pct_two_or_more", "pct_hispanic", "pct_not_hispanic"
)

tracts <- read_rds(file = here::here("dataset", "intersect_tracts.rds"))

air_qual_data <- read_rds(file = here::here("dataset", "air_qual_clean.rds")) |>
  group_by(month)|>
  summarize(across())

census_data <- read_rds(file = here::here("dataset", "census_data.rds")) |> 
  rename(GEOID = FIPS)

tracts_joined <- tracts |> 
  left_join(census_data, by = "GEOID")

data_combined <- air_qual_data |>
  group_by(site_name, month) |>
  left_join(tracts_joined, by = "site_id") |>
  select(-NAME,-method_code,-method_desc,-units,-geometry)

write_rds(data_combined, file = here::here("dataset", "air_qual_census.rds"))
