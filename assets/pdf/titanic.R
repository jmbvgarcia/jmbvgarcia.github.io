library(tidyverse)
library(haven)
library(ggplot2)

# Load dataset
titanic <- read_dta("https://github.com/scunning1975/mixtape/raw/master/titanic.dta") |>
  mutate(
    d = if_else(class == 1, 1, 0),  # Treatment: 1st class
    group = case_when(
      sex == 0 & age == 1 ~ "Woman",
      sex == 0 & age == 0 ~ "Girl",
      sex == 1 & age == 1 ~ "Man",
      sex == 1 & age == 0 ~ "Boy",
      TRUE ~ NA_character_
    )
  )

titanic |> 
  summarise(mean(survived),mean(sex),mean(age))

titanic |> group_by(d) |> 
  summarise(mean(survived),mean(sex),mean(age))

titanic |> group_by(sex) |> 
  summarise(mean(survived))

# Check group sizes
titanic |> count(group, d)

# 1. Compute conditional treatment effects by subgroup
effects_by_group <- titanic |>
  filter(!is.na(group)) |>
  group_by(group) |>
  summarize(
    treated_mean = mean(survived[d == 1], na.rm = TRUE),
    control_mean = mean(survived[d == 0], na.rm = TRUE),
    effect = treated_mean - control_mean,
    control_n = sum(d == 0),
    treat_n = sum(d == 1),
    total_n = control_n + treat_n,
    .groups = "drop"
  )

# 2. Compute weights using control group distribution
total <- sum(effects_by_group$total_n)
treated <- sum(effects_by_group$treat_n)
effects_by_group <- effects_by_group |>
  mutate(weight_total = total_n / total,
         weight_treat = treat_n / treated)

# 3. Compute weighted average treatment effect (ATE)
ate <- sum(effects_by_group$effect * effects_by_group$weight_total)
cat(sprintf("Estimated ATE: %.3f\n", ate))

att <- sum(effects_by_group$effect * effects_by_group$weight_treat)
cat(sprintf("Estimated ATT: %.3f\n", att))

titanic |> 
  mutate(class = if_else(d == 1, "1st class", "Other classes")) |>
  group_by(class) |>
  summarize(survival_rate = mean(survived), .groups = "drop") |>
  ggplot(aes(x = class, y = survival_rate)) +
  geom_col(position = "dodge") +
  labs(
    title = "Survival Rates by Class",
    x = "Passenger Class", y = "Survival Rate"
  ) +
  theme_minimal()

titanic |> 
  mutate(class = if_else(d == 1, "1st class", "Other classes")) |>
  group_by(group, class) |>
  summarize(survival_rate = mean(survived), .groups = "drop") |>
  ggplot(aes(x = group, y = survival_rate, fill = class)) +
  geom_col(position = "dodge") +
  labs(
    title = "Survival Rates by Subgroup and Class",
    x = "Group", y = "Survival Rate", fill = "Passenger Class"
  ) +
  theme_minimal()

lm(survived ~ d, data=titanic) |> summary()
lm(survived ~ d + group, data=titanic) |> summary()
lm(survived ~ d:group + group, data=titanic) |> summary()
