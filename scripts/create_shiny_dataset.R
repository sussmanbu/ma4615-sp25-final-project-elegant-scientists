rm(list = ls())
library(tidyverse)

air_qual_data <- read_rds(file = here::here("dataset", "air_qual_clean.rds")) 
census_data <- read_rds(file = here::here("dataset", "census_data.rds"))

data_combined <- left_join(air_qual_data, census_data, by = "site_name", relationship = "many-to-many")

write_rds(data_combined, file = here::here("dataset", "air_qual_census.rds"))
