rm(list = ls())

library(lubridate)
library(dplyr)
library(tidyverse)
library(broom.mixed)
library(mediocrethemes)
library(sf)

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
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
    neighborhood = factor(neighborhood),
    site = str_extract(as.character(neighborhood), "(?<=_)\\d+"),
    state = str_to_title(state)
  )
# by month, season misses some pandemic nuance
month_pm <- seasonality |>
  st_drop_geometry() |>
  mutate(month = factor(month.abb[month], levels = month.abb)) |>
  group_by(state, month) |>
  summarise(mean_pm25 = mean(pm2.5_dailymean, na.rm = TRUE), .groups = "drop") |>
  mutate(highlight = ifelse(month %in% c("Mar", "Apr", "May"), "National COVID Restrictions", "Other"))

month_pm_gg <- ggplot(month_pm, aes(x = month, y = mean_pm25, fill = highlight)) +
  geom_col() +
  facet_wrap(~ state) +
  scale_fill_manual(values = c("National COVID Restrictions" = "tomato3", "Other" = "azure4")) +
  labs(
    title = "Monthly Average PM2.5 Concentration by State",
    x = "Month",
    y = "PM2.5 (µg/m³)",
    fill = "COVID-19 Highlight"
  ) +
  theme_minimal(base_size = 14)
month_pm_gg
#ggsave(filename = here::here("images", "month_pm_boxplot.png"),
 #      plot = month_pm_gg)

# create boxplot of seasonal PM2.5 and AQI by season for each state

season_summary <- seasonality |>
  group_by(state, season) |>
  summarise(
    mean_pm25 = mean(pm2.5_dailymean, na.rm = TRUE),
    mean_aqi = mean(aqi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))
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
  scale_fill_manual(values = c(
    "Winter" = "#ADD8E6",   
    "Spring" = "#90EE90",   
    "Summer" = "#F08080",   
    "Fall"   = "#D2691E"   
  )) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )
pm25_season_plot

#ggsave(filename = here::here("images", "seasonal_pm_boxplot.png"),
#      plot = pm25_season_plot)

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
#ggsave(filename = here::here("images", "aqi_season_boxplot.png"),
#       plot = aqi_season_plot)


# linear mixed model for repeated measure (daily means) at each site
library(lme4)

lmm <- lmer(pm2.5_dailymean ~ neighborhood * season + (1 | date), data = seasonality) # with interaction
summary(lmm)

# compare the full mixed model lmm to one without accounting repeated daily
lmm_main <- lmer(pm2.5_dailymean ~ neighborhood + season + (1 | date), data = seasonality) # no interaction
anova(lmm_main, lmm)
# lmm with interaction is significantly better

# clean up output for display
tidy_lmm <- broom.mixed::tidy(lmm, effects = "fixed", conf.int = TRUE)
tidy_lmm

# save the model for blog post options
#write_rds(tidy_lmm, file = here::here("dataset","air_qual_lmm.rds"))

# post hoc
library(emmeans)

emmeans_df<- emmeans(lmm, pairwise ~ season | neighborhood, adjust = "tukey")

# Save the results

save_emmeans<- emmeans_df$emmeans |>
  as.data.frame()

write.csv(save_emmeans, file = here::here("dataset", "emmeans_emmeans.csv"))

save_contrasts<- emmeans_df$contrasts |>
  as.data.frame()

write.csv(save_contrasts, file = here::here("dataset", "emmeans_contrasts.csv"))

# visualize outputs

vis_emmeans<- as.data.frame(emmeans_df$emmeans) |>
  mutate(
    season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")),
    state = str_extract(neighborhood, "^[a-zA-Z ]+")
  )

# Create summary plot by state
state_summary <- vis_emmeans |>
  group_by(state, season) |>
  summarise(
    emmean = mean(emmean),
    lower = mean(asymp.LCL),
    upper = mean(asymp.UCL),
    .groups = "drop"
  )
state_sum_gg <- ggplot(state_summary, aes(x = season, y = emmean, group = state)) +
  geom_line(aes(color = state), linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(
    title = "State-Level Average PM2.5 by Season",
    x = "Season",
    y = "Estimated PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )
state_sum_gg

# Faceted by neighborhood
vis_emmeans <- as.data.frame(emmeans_df$emmeans) |>
  mutate(
    state = str_extract(neighborhood, "^[a-zA-Z ]+"),
    state_number = as.integer(as.factor(state)),
    facet_label = paste("State", state_number),
    season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))
  )

neighborhood_emmeans <- ggplot(vis_emmeans, aes(x = season, y = emmean, group = neighborhood)) +
  geom_line(color = "gray50") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  facet_wrap(~ state) +
  labs(
    title = "Estimated PM2.5 by Season and Neighborhood",
    x = "Season",
    y = "Estimated PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
neighborhood_emmeans

ggsave(filename = here::here("images", "neighborhood_emmeans.png"),
       plot = neighborhood_emmeans)

vis_emmeans_gg <- ggplot(vis_emmeans, aes(x = season, y = emmean, group = neighborhood)) +
  geom_line(color = "gray50") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  facet_wrap(~ neighborhood) +
  labs(
    title = "Estimated PM2.5 by Season and Neighborhood",
    x = "Season",
    y = "Estimated PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14)

print(vis_emmeans_gg)

# try a similar plot highlighting significant contrasts
contrasts <- as.data.frame(emmeans_df$contrasts)
sig_contrasts <- contrasts |>
  filter(p.value <0.05)
sig_neighborhoods <- unique(c(sig_contrasts$neighborhood1, sig_contrasts$neighborhood2))


vis_emmeans <- vis_emmeans |>
  mutate(significant = ifelse(neighborhood %in% sig_neighborhoods, TRUE, FALSE))

neighborhood_emmeans <- ggplot(vis_emmeans, aes(x = season, y = emmean, group = neighborhood)) +
  geom_line(aes(color = significant), linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  scale_color_manual(values = c("gray80", "red")) +
  facet_wrap(~ state) +
  labs(
    title = "Estimated PM2.5 by Season and Neighborhood",
    x = "Season",
    y = "Estimated PM2.5 (µg/m³)",
    color = "Significant"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
neighborhood_emmeans

# dot plot
dot_plot <- ggplot(vis_emmeans, aes(x = emmean, y = state, color = season)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                 position = position_dodge(width = 0.5), height = 0.2) +
  labs(
    title = "Estimated PM2.5 by State and Season",
    x = "Estimated PM2.5 (µg/m³)",
    y = "State",
    color = "Season"
  ) +
  theme_minimal(base_size = 14)
dot_plot
ggsave(filename = here::here("images", "dot_plot.png"),
       plot = dot_plot)


# More visualizations: now not grouped by time but averaged across stations
library(zoo)

pm_daily <- seasonality |>
group_by(date) |>
  summarize(
    mean_pm25 = mean(pm2.5_dailymean, na.rm = TRUE),
    se_pm25 = sd(pm2.5_dailymean, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) |>
  mutate(
    mean_pm25_roll = rollmean(mean_pm25, k = 7, fill = NA, align = "center")
  )

daily_pm_line <- ggplot(pm_daily, aes(x = date)) +
  geom_ribbon(aes(ymin = mean_pm25 - se_pm25, ymax = mean_pm25 + se_pm25),
              fill = "gray70", alpha = 0.2) +
  geom_line(aes(y = mean_pm25), color = "gray60", alpha = 0.3, linewidth = 0.3) +
  geom_line(aes(y = mean_pm25_roll), color = "firebrick", linewidth = 1.2) +
  labs(
    title = "PM2.5 Concentration: Daily Values and 7-Day Rolling Average",
    x = "Date",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

daily_pm_line
ggsave(filename = here::here("images", "daily_pm_roll.png"),
       plot = daily_pm_line)
