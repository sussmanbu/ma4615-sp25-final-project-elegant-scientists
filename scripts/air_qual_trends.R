rm(list = ls())

library(lubridate)
library(dplyr)
library(tidyverse)
library(broom.mixed)
library(mediocrethemes)

air_qual_data <- read_rds(file = here::here("dataset", "air_qual_clean.rds")) 
  
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
    site_name = factor(site_name)
  )

# create boxplot of PM2.5 by season at each site
season_box <- ggplot(seasonality, aes(x = site_name, y = pm2.5_dailymean)) +
  geom_boxplot() +
  facet_grid(~ season) +
  scale_x_discrete(labels = c(
    "BOSTON KENMORE SQ" = "Kenmore",
    "DUDLEY SQUARE ROXBURY" = "Roxbury",
    "VON HILLERN ST" = "Dorchester"
  )) +
  labs(title = "Seasonal Distribution of PM2.5 by Site",
       x = "Site Name",
       y = "Mean PM2.5",
       fill = "Season") +
  theme_mediocre() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
season_box
ggsave(filename = here::here("images", "seasonal_pm_boxplot.png"),
       plot = season_box)
  
# linear mixed model for repeated measure (daily means) at each site
library(lme4)

lmm <- lmer(pm2.5_dailymean ~ site_name * season + (1 | date), data = seasonality)
summary(lmm)

# compare the full mixed model lmm to one without accounting repeated daily
lmm_main <- lmer(pm2.5_dailymean ~ site_name + season + (1 | date), data = seasonality)
anova(lmm_main, lmm)
# full lmm is significantly better

# clean up output for display
tidy_lmm <- broom.mixed::tidy(lmm, effects = "fixed", conf.int = TRUE)
tidy_lmm

# save the model for blog post options
write_rds(tidy_lmm, file = here::here("dataset","air_qual_lmm.rds"))
