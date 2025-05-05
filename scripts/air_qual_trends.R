rm(list = ls())

library(lubridate)
library(dplyr)
library(tidyverse)
library(broom.mixed)
library(mediocrethemes)

# columns of interest for this graph
cols <- c("month","date","pm2.5_dailymean","aqi","state","neighborhood")

air_qual_data <- read_rds(file = here::here("dataset", "air_qual_census.rds")) |>
  select(cols)
  
seasonality <- air_qual_data |>
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ NA_character_
    ),
    season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")),
    neighborhood = factor(neighborhood),
    site = str_extract(as.character(neighborhood), "(?<=_)\\d+"),
    state = str_to_title(state)
  )

# create boxplot of seasonal PM2.5 and AQI by season for each state

season_summary <- seasonality |>
  group_by(state, season) |>
  summarise(
    mean_pm25 = mean(pm2.5_dailymean, na.rm = TRUE),
    mean_aqi = mean(aqi, na.rm = TRUE),
    .groups = "drop"
  )
pm25_season_plot <- ggplot(season_summary, aes(x = mean_pm25, y = season, fill = season)) +
  geom_col() +
  facet_wrap(~ state) +
  labs(
    title = "Average Seasonal PM2.5 Concentration by State",
    x = "Season",
    y = "PM 2.5 (µg/m³)",
    fill = "Season"
  ) +
  theme_mediocre() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )
pm25_season_plot
ggsave(filename = here::here("images", "seasonal_pm_boxplot.png"),
       plot = pm25_season_plot)

# now AQI
aqi_season_plot <- ggplot(season_summary, aes(x = mean_aqi, y = season, fill = season)) +
  geom_col() +
  facet_wrap(~ state) +
  labs(
    title = "Average Seasonal AQI by State",
    x = "Season",
    y = "Air Quality Index (AQI)",
    fill = "Season"
  ) +
  theme_mediocre() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )
aqi_season_plot
ggsave(filename = here::here("images", "aqi_season_boxplot.png"),
       plot = aqi_season_plot)


# linear mixed model for repeated measure (daily means) at each site
library(lme4)

lmm <- lmer(pm2.5_dailymean ~ neighborhood * season + (1 | date), data = seasonality)
summary(lmm)

# compare the full mixed model lmm to one without accounting repeated daily
lmm_main <- lmer(pm2.5_dailymean ~ neighborhood + season + (1 | date), data = seasonality)
anova(lmm_main, lmm)
# full lmm is significantly better

# clean up output for display
tidy_lmm <- broom.mixed::tidy(lmm, effects = "fixed", conf.int = TRUE)
tidy_lmm

# save the model for blog post options
write_rds(tidy_lmm, file = here::here("dataset","air_qual_lmm.rds"))
