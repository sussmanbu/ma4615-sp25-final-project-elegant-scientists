rm(list = ls()) #clear environment
#install.packages("tidycensus")

library(tidycensus)
library(tidyverse)
library(purrr)

# census API key requested Mar. 31, 2025 by CRM
census_api_key("011ae02d8a0a6a5f13e1341184c745503846f0a9", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# show variable codes for 2020 census Demographic and Housing Characteristics?
variable.options <- load_variables(2020, "dhc")

# tract list extracted from 0.5km radius of monitoring sites
tract_list <- read_rds(file = here::here("dataset", "intersect_tracts.rds")) |>
  st_drop_geometry() |>
  distinct(GEOID, NAME)|>
  mutate(
    state_fips = substr(GEOID, 1, 2),
    county_fips = substr(GEOID, 3, 5)
  ) 

# identify race variables and total pop for dec census
race_vars <- c(total_pop = "P10_001N", white = "P10_003N", black = "P10_004N", 
  native = "P10_005N", asian = "P10_006N", pacific = "P10_007N", 
  other_race = "P10_008N", two_or_more = "P10_009N",
  hispanic = "P11_002N", not_hispanic = "P11_003N"
)

# ACS 5-yr variables for socio status
socio_vars <- c(
  median_income = "B19013_001",
  income_below_poverty = "B17001_002",
  poverty_universe = "B17001_001"
)

# create df with variables of interest from the desired time and locations
dhc_data <- map_dfr(
  split(tract_list, paste(tract_list$state_fips, tract_list$county_fips)),
  function(df) {
    get_decennial(
      geography = "tract",
      variables = race_vars,
      state = df$state_fips[1],
      county = df$county_fips[1],
      year = 2020,
      sumfile = "dhc",
      geometry = FALSE,
      output = "wide",
      cache_table = TRUE
    ) |>
      filter(GEOID %in% df$GEOID) |>
      left_join(df, by = "GEOID")
  }
) |>
  mutate(
    pct_white = white / total_pop * 100, pct_black = black / total_pop * 100,
    pct_native = native / total_pop * 100, pct_asian = asian / total_pop * 100,
    pct_pacific = pacific / total_pop * 100, pct_other_race = other_race / total_pop * 100,
    pct_two_or_more = two_or_more / total_pop * 100,
    pct_hispanic = hispanic / total_pop * 100, pct_not_hispanic = not_hispanic / total_pop * 100
  )

# create df of socio variables from ACS
socio_data <- map_dfr(
  split(tract_list, paste(tract_list$state_fips, tract_list$county_fips)),
  function(df) {
    get_acs(
      geography = "tract",
      variables = socio_vars,
      state = df$state_fips[1],
      county = df$county_fips[1],
      year = 2020,
      survey = "acs5",
      output = "wide",
      cache_table = TRUE
    ) |>
      filter(GEOID %in% df$GEOID) |>
      left_join(df, by = "GEOID")
  }
) |>
  mutate(
    pct_poverty = 100 * income_below_povertyE / poverty_universeE
  )

# combine dhs and socio

census_data <- full_join(dhc_data, socio_data, by = "GEOID") |>
  rename(FIPS = GEOID) |>
  select(
    -ends_with("M"),                             # drop all MOE columns
    -matches("^NAME(\\.x|\\.y)*$"),              # drop all NAME.* columns
    -matches("^state_fips"),                     # drop all state_fips.* columns
    -matches("^county_fips")                     # drop all county_fips.* columns
  ) |>
  relocate(FIPS) |>
  filter(total_pop != 0, !is.na(total_pop)) 

write_rds(census_data, file = here::here("dataset", "census_data.rds"))
