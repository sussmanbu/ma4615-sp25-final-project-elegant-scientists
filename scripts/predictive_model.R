rm(list = ls())
#install.packages("cplm")

library(dplyr)
library(tidyverse)
library(cplm)
library(mediocrethemes)
library(sf)

dataset <- read_rds(file = here::here("dataset/air_qual_census.rds")) |>
  st_drop_geometry()

# right skewed distribution, includes true 0 values (no gamma)
pm2.5_dist <- ggplot(dataset, aes(x = pm2.5_dailymean)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribution of PM2.5 Daily Mean") +
  theme_mediocre()

pm2.5_dist

#ggsave(filename = here::here("images", "pm25_distr.png"),
#       plot = pm2.5_dist)

summary(dataset$pm2.5_dailymean)

# variance check
check_var <- dataset %>%
  group_by(month) %>%
  summarize(mean_pm = mean(pm2.5_dailymean, na.rm = TRUE),
            var_pm = var(pm2.5_dailymean, na.rm = TRUE))

var_mean_PM <- ggplot(check_var, aes(x = mean_pm, y = var_pm)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightblue", alpha = 0.5, linewidth = 1) +
  labs(
    title = "Variance vs Mean of PM2.5 Daily Mean",
    x = "Mean PM2.5",
    y = "Variance of PM2.5"
  ) +
  geom_point(size = 2) +
  theme_mediocre()
var_mean_PM
ggsave(filename = here::here("images", "pm25_var_mean.png"),
       plot = var_mean_PM)

# diversity index Tweedie
simpson_pm <- dataset |>
  mutate(
    prop_white = white / total_pop,
    prop_black = black / total_pop,
    prop_asian = asian / total_pop,
    prop_other = (native + pacific + other_race + two_or_more) / total_pop,
    simpson_diversity = 1 - (prop_white^2 + prop_black^2 + prop_asian^2 + prop_other^2)
  )
model_diversity <- cpglm(pm2.5_dailymean ~ month + simpson_diversity, data = simpson_pm)
summary(model_diversity)

# percent relative to white Tweedie
model_relWhite <- cpglm(pm2.5_dailymean ~ month + pct_black + pct_asian, data = dataset)
summary(model_relWhite)
    

