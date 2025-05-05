rm(list = ls())

library(sf)
library(tidyverse)
library(stringr)
library(mediocrethemes)
library(ggsci)

census_data <- read_rds(file = here::here("dataset", "census_neighborhood.rds")) |>
  select(-GEOID,-site_id, -starts_with("pct_")) |>
  st_drop_geometry()

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

# ANOVA race by state
library(purrr)
library(broom)

census_neighborhood_summary <- census_neighborhood_summary |>
  mutate(state = str_extract(neighborhood, "^[a-zA-Z]+"))

census_pct_long <- census_neighborhood_summary |>
  select(neighborhood, starts_with("perc_"), state) |>
  pivot_longer(cols = starts_with("perc_"), 
               names_to = "race", 
               values_to = "percentage") |>
  mutate(race = str_remove(race, "perc_"))

avg_pct_state <- census_pct_long |>
  group_by(state, race) |>
  summarise(mean_pct = mean(percentage, na.rm = TRUE), .groups = "drop")

kruskal_results <- census_pct_long |>
  group_split(race) |>
  map(~ {
    result <- kruskal.test(percentage ~ state, data = .x)
    tibble(race = unique(.x$race), p.value = result$p.value)
  }) |>
  bind_rows()

kruskal_results |>
  select(race, p.value)

# by state: neighborhood comparisons, skips maryland (1 neighborhood)
kruskal_by_state_race <- census_pct_long |>
  dplyr::group_by(state, race) |>
  dplyr::group_split() |>
  purrr::map_dfr(\(df) {
    if (length(unique(df$neighborhood)) < 2) {
      return(NULL)  # Skip if only one neighborhood (maryland)
    }
    result <- kruskal.test(percentage ~ neighborhood, data = df)
    tibble(
      state = unique(df$state),
      race = unique(df$race),
      p.value = result$p.value
    )
  })
