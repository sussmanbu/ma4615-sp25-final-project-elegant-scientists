rm(list = ls())
#install.packages("cplm")

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
model_relWhite <- cpglm(pm2.5_dailymean ~ month + pct_black + pct_asian + 
                          pct_native + pct_pacific + pct_other_race + pct_two_or_more,  
                        data = dataset)
summary(model_relWhite)
    
# Visualizing the outputs of the models (coefficients)--------------------

# extract coeffs
coef_div <- summary(model_diversity)$coefficients |> as.data.frame()
coef_div$term <- rownames(coef_div)
coef_div$model <- "Diversity Model"

coef_race <- summary(model_relWhite)$coefficients |> as.data.frame()
coef_race$term <- rownames(coef_race)
coef_race$model <- "Race Model"

# combine coeffs, remove intercept
coef_plot_df <- bind_rows(coef_div, coef_race)
colnames(coef_plot_df) <- c("estimate", "std_error", "t_value", "p_value", "term", "model")

coef_plot_df <- coef_plot_df |> filter(term != "(Intercept)")

# add significance stars
coef_plot_df <- coef_plot_df |> 
  mutate(sig = cut(p_value,
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                   labels = c("***", "**", "*", ".", "")))

# add clean labels and reorder
coef_plot_df <- coef_plot_df |> 
  mutate(term_clean = case_when(
    term == "month" ~ "Month",
    term == "pct_black" ~ "% Black Population",
    term == "pct_asian" ~ "% Asian Population",
    term == "pct_native" ~ "% Native American Population",
    term == "pct_pacific" ~ "% Pacific Islander Population",
    term == "pct_other_race" ~ "% Other Race Population",
    term == "pct_two_or_more" ~ "% Two or More Races",
    term == "simpson_diversity" ~ "Simpson Diversity Index",
    TRUE ~ term
  ))
coef_plot_df$term_clean <- factor(coef_plot_df$term_clean, levels = c(
  "Month",
  "% Black Population",
  "% Asian Population",
  "% Native American Population",
  "% Pacific Islander Population",
  "% Other Race Population",
  "% Two or More Races",
  "Simpson Diversity Index"
))

# faceted plot with significance indicators

cpglm_coeff_vis <- ggplot(coef_plot_df, aes(x = term_clean, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - std_error, ymax = estimate + std_error), width = 0.2) +
  geom_text(aes(label = sig), vjust = -1.2, size = 5) +
  facet_wrap(~model, scales = "free_x") +
  labs(
    x = "Predictor",
    y = "Estimate (± SE)",
    title = "Coefficient Estimates from CPGLM Models",
    subtitle = "Faceted by Model, with Significance Levels"
  ) +
  theme_mediocre() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cpglm_coeff_vis

#ggsave(filename = here::here("images", "cpglm_output.png"),
#         plot = cpglm_coeff_vis)
