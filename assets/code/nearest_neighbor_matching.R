
# Nearest Neighbor Matching Simulation with Visualization

library(tidyverse)
library(ggrepel)

# Set seed for reproducibility
set.seed(1234)

nt <- 25
nc <- 75

# Generate treatment group

# Generate control group
control_group <- tibble(
  id = (nt + 1):(nt + nc),
  treatment = 0,
  age = round(rnorm(nc, mean = 40, sd = 5), 0),
  prev_earnings = runif(nc, min = 65000, max = 89000),
  promotion = rbinom(nc, 1, -0.2+ 0.011*age + prev_earnings/900000)
)

treatment_group <- tibble(
  id = 1:nt,
  treatment = 1,
  age = round(rnorm(nt, mean = 45, sd = 3), 0),
  prev_earnings = runif(nt, min = 70000, max = 90000),
  promotion = rbinom(nt, 1, 0.1 + 0.011*age + prev_earnings/900000)
)



# Combine data
data <- bind_rows(treatment_group, control_group)

# Standardize covariates (on full sample)
data <- data |> mutate(
  age_std = (age - mean(age)) / sd(age),
  earnings_std = (prev_earnings - mean(prev_earnings)) / sd(prev_earnings)
)

# Nearest neighbor matching by Euclidean distance
match_nearest <- function(treated_df, control_df) {
  matches <- tibble()
  
  for (i in 1:nrow(treated_df)) {
    t_row <- treated_df[i, ]
    control_df <- control_df |> mutate(
      distance = sqrt((age_std - t_row$age_std)^2 + (earnings_std - t_row$earnings_std)^2)
    )
    best_match <- control_df |> 
      arrange(distance) |> 
      slice(1) %>% 
      mutate(treated_id = t_row$id)
    matches <- bind_rows(matches, best_match)
  }
  
  return(matches)
}

# Split treatment and control
treated <- data |> filter(treatment == 1)
control <- data |> filter(treatment == 0)

# Find matches
matched_controls <- match_nearest(treated, control)

# Prepare comparison dataset
comparison <- matched_controls |> 
  rename(idC = id, ageC = age, earnC = prev_earnings, promC = promotion, ageC_std = age_std, earnC_std = earnings_std) |> 
  select(idC, promC, ageC, earnC, ageC_std, earnC_std, distance, treated_id) |> 
  left_join(
    treated |> rename(idT = id, ageT = age, earnT = prev_earnings, promT = promotion, ageT_std = age_std, earnT_std = earnings_std),
    by = c("treated_id" = "idT")
  )

# Print matched pairs
comparison |> 
  mutate(dist = round(distance, 3), earnT = floor(earnT), earnC = floor(earnC)) |> 
  select(treated_id, idC, ageT, ageC, earnT, earnC, promT, promC, dist) |> 
  head()

# Plot in standardized covariate space with matched pairs
plot_data <- bind_rows(
  treated |> mutate(role = "Treated"),
  control |> mutate(role = "Control")
)

ggplot(plot_data, aes(x = age_std, y = earnings_std, color = factor(treatment))) +
  geom_point(size = 3)  +
  theme_minimal()

ggplot(plot_data, aes(x = age_std, y = earnings_std, color = factor(treatment))) +
  geom_point(size = 3) +
  geom_segment(data = comparison,
               aes(x = ageT_std, y = earnT_std,
                   xend = ageC_std, yend = earnC_std),
               arrow = arrow(length = unit(0.02, "npc")), color = "gray50") +
  labs(title = "Nearest Neighbor Matching in Standardized Covariate Space",
       x = "Standardized Age", y = "Standardized Earnings",
       color = "Treatment Group") +
  theme_minimal()

# Balance diagnostics
balance_table <- bind_rows(
  treated |> mutate(group = "Treated"),
  control |> mutate(group = "Control (pre-match)"),
  matched_controls |> mutate(group = "Matched Control")
) |> 
  group_by(group) |> 
  summarise(
    mean_age = mean(age),
    mean_earnings = mean(prev_earnings),
    mean_promotion = mean(promotion),
    .groups = "drop"
  )

print(balance_table)


# ATT estimate
att_estimate <- comparison |> 
  mutate(diff = promT - promC) |> 
  summarise(ATT = mean(diff)) |> 
  pull(ATT)

cat(sprintf("\nEstimated ATT from NN matching: %.3f\n", att_estimate))


# Promotion rate before and after matching
promotion_summary <- bind_rows(
  treated |> mutate(group = "Treated"),
  control |> mutate(group = "Control"),
  matched_controls |> mutate(group = "Matched Control")
) |> 
  group_by(group) |> 
  summarise(promotion_rate = mean(promotion), .groups = "drop")

# Plot
ggplot(promotion_summary, aes(x = group, y = promotion_rate, fill = group)) +
  geom_col(show.legend = FALSE) +
  ylim(0, 1) +
  labs(title = "Promotion Rate by Group", y = "Promotion Rate", x = "Group") +
  theme_minimal()



# Caliper matching by Euclidean distance
match_caliper <- function(treated_df, control_df, caliper = 0.5) {
  matches <- tibble()
  
  for (i in 1:nrow(treated_df)) {
    t_row <- treated_df[i, ]
    control_df <- control_df |> mutate(
      distance = sqrt((age_std - t_row$age_std)^2 + (earnings_std - t_row$earnings_std)^2)
    )
    best_match <- control_df |> 
      filter(distance<caliper) |> 
      summarise(age=mean(age),
                prev_earnings = mean(prev_earnings),
                promotion = mean(promotion),
                age_std=mean(age_std),
                earnings_std = mean(earnings_std),
                number_of_matches = n()) %>% 
      mutate(treated_id = t_row$id)
    matches <- bind_rows(matches, best_match)
  }
  
  return(matches)
}

matched_controls <- match_caliper(treated, control, 0.5)


# Prepare comparison dataset
comparison <- matched_controls |> 
  rename(ageC = age, earnC = prev_earnings, promC = promotion, ageC_std = age_std, earnC_std = earnings_std) |> 
  select(promC, ageC, earnC, ageC_std, earnC_std, treated_id, number_of_matches) |> 
  left_join(
    treated |> rename(idT = id, ageT = age, earnT = prev_earnings, promT = promotion, ageT_std = age_std, earnT_std = earnings_std),
    by = c("treated_id" = "idT")
  )

# Print matched pairs
comparison |> 
  mutate( earnT = floor(earnT), earnC = floor(earnC)) |> 
  select(treated_id, ageT, ageC, earnT, earnC, promT, promC, number_of_matches) |> 
  head()

# ATT estimate
att_estimate <- comparison |> mutate(diff = promT - promC) |> summarise(ATT = mean(diff,na.rm=TRUE)) |> pull(ATT)
cat(sprintf("\nEstimated ATT from NN matching: %.3f\n", att_estimate))



# Plot in standardized covariate space with matched pairs
plot_data <- bind_rows(
  treated |> mutate(role = "Treated"),
  control |> mutate(role = "Control")
)

ggplot(plot_data, aes(x = age_std, y = earnings_std, color = factor(treatment))) +
  geom_point(size = 3, alpha = 0.8)  +
  theme_minimal()

library(ggforce)  # for geom_circle

# Radius for visual circles (standardized units, so ~0.25 looks good)
circle_radius <- 0.5

circle_data <- treated |> 
  mutate(radius = circle_radius) |> 
  select(age_std, earnings_std, radius)

ggplot() +
  geom_point(data = plot_data,
             aes(x = age_std, y = earnings_std, color = role),
             size = 3, alpha = 0.8) +
  geom_circle(data = circle_data,
              aes(x0 = age_std, y0 = earnings_std, r = radius),
              inherit.aes = FALSE,
              color = "black", linetype = "dashed", alpha = 0.3) +
  labs(title = "Matched Controls Highlighted with Circles Around Treated Units",
       x = "Standardized Age", y = "Standardized Earnings") +
  scale_color_manual(values = c("Treated" = "red")) +
  theme_minimal()


### Second: Only matched units and synthetic averages

# This uses your `comparison` object created after caliper matching
# It includes treated units and synthetic (averaged) matches

synth_plot_data <- comparison |> 
  mutate(role = "Synthetic Match") |> 
  rename(age_std = ageC_std, earnings_std = earnC_std) |> 
  select(treated_id, age_std, earnings_std, role) |> 
  bind_rows(
    treated |> 
      select(treated_id = id, age_std, earnings_std = earnings_std) |> 
      mutate(role = "Treated")
  )

ggplot(synth_plot_data, aes(x = age_std, y = earnings_std, color = role)) +
  geom_point(size = 3) +
  geom_line(aes(group = treated_id), color = "gray50", linetype = "dotted") +
  labs(title = "Treated Units and Synthetic Matches from Caliper Averaging",
       x = "Standardized Age", y = "Standardized Earnings") +
  scale_color_manual(values = c("Treated" = "red", "Synthetic Match" = "blue")) +
  theme_minimal()
