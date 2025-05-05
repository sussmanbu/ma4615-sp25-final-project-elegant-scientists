rm(list = ls()) #clear environment
#install.packages("tidycensus")

library(tidyverse)
library(sf)
library(tidycensus)

census_api_key("011ae02d8a0a6a5f13e1341184c745503846f0a9", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

air_data <- read_rds(file = here::here("dataset", "air_qual_clean.rds"))

# identify individual site points
site_points <- air_data |>
  select(site_id, site_name, state, lat, long) |>
  distinct()

# reproject map points as sf features
site_sf <- st_as_sf(site_points, coords = c("long", "lat"), crs = 4326)
site_proj <- st_transform(site_sf, crs = 3857) # meters

# use census tracts to get 0.5 km intersecting buffer
site_buffers <- st_buffer(site_proj, dist = 500)

unique_states <- unique(site_points$state)

tracts <- map_dfr(unique_states, ~ get_acs(
  geography = "tract",
  variables = "B01003_001",  # Total population (optional)
  state = .x,
  geometry = TRUE,
  year = 2020
))

tracts_proj <- st_transform(tracts, crs = 3857)

tracts_joined <- st_join(tracts_proj, site_buffers, join = st_intersects, left = FALSE)
tracts_named <- tracts_joined |>
  group_by(state, site_id) |>
  mutate(neighborhood = paste0(tolower(state), "_", row_number())) |>
  ungroup()
air_qual_tracts <- tracts_named |>
  select(site_id, GEOID, NAME, estimate, neighborhood)

write_rds(air_qual_tracts, file = here::here("dataset", "intersect_tracts.rds"))
