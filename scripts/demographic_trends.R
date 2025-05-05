rm(list = ls())

library(tidyverse)
library(mediocrethemes)
library(ggsci)

census_data <- read_rds(file = here::here("dataset", "census_neighborhood.rds")) |>
  select(-GEOID,-site_id, -starts_with("pct_"))

race_cols <- names(census_data) |>
  setdiff(c("site_name", "total_pop"))
  
# Summarize total population and race counts per site
race_count <- c("white","black","native","asian","pacific","other_race","two_or_more")  
neighborhood_race <- c("neighborhood","total_pop","white","black","native","asian","pacific","other_race","two_or_more")  

census_neighborhood_summary <- census_data |>
  group_by(neighborhood) |>
  summarise(
    total_pop = sum(total_pop, na.rm = TRUE),
    across(all_of(race_count), sum, na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(all_of(neighborhood_race)) |>
  mutate(across(all_of(race_count), ~ .x / total_pop * 100, .names = "perc_{.col}"))


census_long <- census_neighborhood_summary |>
  pivot_longer(
    cols = starts_with("perc_"), 
    names_to = "race", 
    values_to = "percentage"
  ) |>
  mutate(
    race = str_replace(race, "perc_", "") |> str_to_title(),
    neighborhood = factor(neighborhood, levels = unique(neighborhood))
    )|>
  mutate(
    site = str_extract(neighborhood, "(?<=_)\\d+"),
    state = str_to_title(str_extract(neighborhood, "^[^_]+"))
        )

neighborhood_demog <- ggplot(census_long, aes(x = site, y = percentage, fill = race)) +
  geom_bar(stat = "identity") +
  coord_flip() +  
  facet_wrap(~ state, scales = "free_y") +  
  labs(
    title = "Racial Composition by Monitored Neighborhood",
    x = "Site Number by State",
    y = "Percent of Total Population",
    fill = "Race"
  ) +
  theme_minimal() +
  scale_fill_jama() +
  theme(
    #axis.text.x = element_text(hjust = 4),
    #axis.text.y = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 14)
  )
neighborhood_demog

ggsave(filename = here::here("images", "site_demographics.png"),
      plot = neighborhood_demog)
