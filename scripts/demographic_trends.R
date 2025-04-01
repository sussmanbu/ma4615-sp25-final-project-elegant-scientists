rm(list = ls())

library(tidyverse)
library(mediocrethemes)

census_data <- read_rds(file = here::here("dataset", "census_data.rds")) |>
  select(-FIPS, -starts_with("pct_"))

race_cols <- names(census_data) |>
  setdiff(c("site_name", "total_pop"))
  
# Summarize total population and race counts per site
race_count <- c("white","black","native","asian","pacific","other_race","two_or_more")  
site_race <- c("site_name","total_pop","white","black","native","asian","pacific","other_race","two_or_more")  

census_site_summary <- census_data |>
  group_by(site_name) |>
  summarise(
    total_pop = sum(total_pop, na.rm = TRUE),
    across(all_of(race_cols), sum, na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(all_of(site_race)) |>
  mutate(across(all_of(race_count), ~ .x / total_pop * 100, .names = "perc_{.col}"))


census_long <- census_site_summary |>
  pivot_longer(
    cols = starts_with("perc_"), 
    names_to = "race", 
    values_to = "percentage"
  ) |>
  mutate(
    race = str_replace(race, "perc_", "") |> str_to_title(),
    site_name = recode(site_name,
                              "BOSTON KENMORE SQ" = "Kenmore",
                              "DUDLEY SQUARE ROXBURY" = "Roxbury",
                              "VON HILLERN ST" = "Dorchester"
    )
  ) 

site_demog <- ggplot(census_long, aes(x = race, y = percentage, fill = race)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ site_name) +
  labs(
    title = "Racial Composition by Site",
    x = "Race",
    y = "Percent Total Pop",
    fill = "Race"
  ) +
  theme_mediocre() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
site_demog

ggsave(filename = here::here("images", "site_demographics.png"),
       plot = site_demog)
