rm(list = ls()) #clear environment
#install.packages("tidycensus")

library(tidycensus)
library(tidyverse)

# census API key requested Mar. 31, 2025 by CRM
census_api_key("011ae02d8a0a6a5f13e1341184c745503846f0a9", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# show variable codes for 2020 census Demographic and Housing Characteristics?
variable.options <- load_variables(2020, "dhc")

# tracts intersecting 0.5 km radius around each sensor (min range 'neighborhood' scale EPA)
FIPS <- c("25025010103", "25025010104", "25025010206", "25025010204", "25025010205", # kenmore
            "25025081700", "25025080300", "25025080401", "25025080601", "25025090700", # roxbury
            "25025061101", "25025061201", "25025981201") # von hillern dorchester

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
dhc_data <- get_decennial(
  geography = "tract",
  variables = race_vars,
  state = "25", #MA
  county = "025", # Suffolk County
  year = 2020,
  sumfile = "dhc", # Demographic and Housing Characteristics
  geometry = FALSE,
  output = "wide",  # wide format gives one row per tract
  cache_table = TRUE
)  |>
  # keep only intersecting tracts
  filter(GEOID %in% FIPS) |> 
  # add site names by intersecting tract
  mutate(
    site_name = case_when(
      GEOID %in% c("25025010103", "25025010104", "25025010206", "25025010204", "25025010205") ~ "BOSTON KENMORE SQ",
      GEOID %in% c("25025081700", "25025080300", "25025080401", "25025080601", "25025090700") ~ "DUDLEY SQUARE ROXBURY",
      GEOID %in% c("25025061101", "25025061201", "25025981201") ~ "VON HILLERN ST",
      TRUE ~ NA_character_
    )
  ) |>
 # add columns for percent by race
   mutate(
    pct_white = white / total_pop * 100, pct_black = black / total_pop * 100,
    pct_native = native / total_pop * 100, pct_asian = asian / total_pop * 100,
    pct_pacific = pacific / total_pop * 100, pct_other_race = other_race / total_pop * 100,
    pct_two_or_more = two_or_more / total_pop * 100,
    pct_hispanic = hispanic / total_pop * 100, pct_not_hispanic = not_hispanic / total_pop * 100
  )

# create df of socio variables from ACS
socio_data <- get_acs(
  geography = "tract",
  state = "25",
  county = "025",
  year = 2020,
  variables = socio_vars,
  survey = "acs5",
  output = "wide",
  cache_table = TRUE
)  |>
  # keep only intersecting tracts
  filter(GEOID %in% FIPS) |> 
  # add site names by intersecting tract
  mutate(
    site_name = case_when(
      GEOID %in% c("25025010103", "25025010104", "25025010206", "25025010204", "25025010205") ~ "BOSTON KENMORE SQ",
      GEOID %in% c("25025081700", "25025080300", "25025080401", "25025080601", "25025090700") ~ "DUDLEY SQUARE ROXBURY",
      GEOID %in% c("25025061101", "25025061201", "25025981201") ~ "VON HILLERN ST",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    pct_poverty = 100 * income_below_povertyE / poverty_universeE
  )

census_data <- full_join(dhc_data, socio_data, by = c("GEOID", "site_name")) |>
  select(-"NAME.x", -"NAME.y") |>
  rename(FIPS = GEOID) |>
  relocate(site_name, .after = FIPS)

write_rds(census_data, file = here::here("dataset", "census_data.rds"))
