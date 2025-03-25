
library(tidyverse)

var <- c("Date","Daily Mean PM2.5 Concentration", "Daily AQI Value", "Units","Local Site Name", "Method Code","Method Description",
"County","Site Latitude","Site Longitude")

air_qual <- read_csv(here::here("dataset", "epa_air_qual.csv")) |>
  select(all_of(var))



write_rds(air_qual, file = here::here("dataset", "air_qual_clean.rds"))
