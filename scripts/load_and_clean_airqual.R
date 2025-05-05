rm(list = ls())
library(tidyverse)
library(lubridate)

var <- c("Date","Daily Mean PM2.5 Concentration", "Daily AQI Value", "Units","Local Site Name", "Site ID", "State","Method Code","Method Description",
"Site Latitude","Site Longitude")

boston <- read_csv(here::here("dataset", "boston_air_qual.csv")) |>
  select(all_of(var)) |>
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         month = lubridate::month(Date))|>
  rename(
         date = Date,
         site_name = `Local Site Name`,
         pm2.5_dailymean = `Daily Mean PM2.5 Concentration`,
         aqi = `Daily AQI Value`,
         units = Units,
         method_code = `Method Code`,
         method_desc = `Method Description`,
         lat = `Site Latitude`,
         long = `Site Longitude`,
         state = State,
         site_id = `Site ID`
         ) |>
  mutate(pm2.5_dailymean = if_else(pm2.5_dailymean < 0, 0, pm2.5_dailymean)) |>
  relocate(month, .before = pm2.5_dailymean)

nyc <- read_csv(here::here("dataset", "nyc_air_qual.csv")) |>
  select(all_of(var)) |>
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         month = lubridate::month(Date))|>
  rename(
    date = Date,
    site_name = `Local Site Name`,
    pm2.5_dailymean = `Daily Mean PM2.5 Concentration`,
    aqi = `Daily AQI Value`,
    units = Units,
    method_code = `Method Code`,
    method_desc = `Method Description`,
    lat = `Site Latitude`,
    long = `Site Longitude`,
    state = State,
    site_id = `Site ID`
  ) |>
  mutate(pm2.5_dailymean = if_else(pm2.5_dailymean < 0, 0, pm2.5_dailymean)) |>
  relocate(month, .before = pm2.5_dailymean)

philly <- read_csv(here::here("dataset", "philly_air_qual.csv")) |>
  select(all_of(var)) |>
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         month = lubridate::month(Date))|>
  rename(
    date = Date,
    site_name = `Local Site Name`,
    pm2.5_dailymean = `Daily Mean PM2.5 Concentration`,
    aqi = `Daily AQI Value`,
    units = Units,
    method_code = `Method Code`,
    method_desc = `Method Description`,
    lat = `Site Latitude`,
    long = `Site Longitude`,
    state = State,
    site_id = `Site ID`
  ) |>
  mutate(pm2.5_dailymean = if_else(pm2.5_dailymean < 0, 0, pm2.5_dailymean)) |>
  relocate(month, .before = pm2.5_dailymean)
  
air_qual <- bind_rows(boston, nyc, philly)

write_rds(air_qual, file = here::here("dataset", "air_qual_clean.rds"))
write_csv(air_qual, file = here::here("dataset","site_air_qual.csv"))
