rm(list = ls())
library(tidyverse)
library(lubridate)

var <- c("Date","Daily Mean PM2.5 Concentration", "Daily AQI Value", "Units","Local Site Name", "Method Code","Method Description",
"Site Latitude","Site Longitude")

air_qual <- read_csv(here::here("dataset", "epa_air_qual.csv")) |>
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
         long = `Site Longitude`
         ) |>
  relocate(month, .before = pm2.5_dailymean)

write_rds(air_qual, file = here::here("dataset", "air_qual_clean.rds"))
