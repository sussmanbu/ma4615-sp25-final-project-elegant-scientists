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

air_qual_data <- read_rds(file = here::here("dataset", "air_qual_clean.rds")) |>
  group_by(site_name, month)|>
  summarize(across())

census_data <- read_rds(file = here::here("dataset", "census_data.rds")) |>
  group_by(site_name) |>
  summarize(
    across(all_of(race_vars), sum, na.rm = TRUE),
    across(all_of(pct_race), mean, na.rm = TRUE)
  )


data_combined <- air_qual_data |>
  group_by(site_name, month) |>
  left_join(census_data, by = "site_name", relationship = "many-to-many")

write_rds(data_combined, file = here::here("dataset", "air_qual_census.rds"))
