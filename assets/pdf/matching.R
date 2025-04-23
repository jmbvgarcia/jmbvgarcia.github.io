# Load necessary libraries
library(tidyverse)

# Set seed for reproducibility
set.seed(123)

nt=5
nc=15

# Generate data
# Treatment group: 5 units
treatment_group <- tibble(
  id = 1:nt,
  treatment = 1,
  age = round(runif(nt, min = 35, max = 50),0),  # Treatment group is older
  prev_earnings = runif(nt, min = 70000, max = 90000),  # Higher previous earnings
  # Outcome with treatment effect
  promotion = rbinom(nt, 1, 0.7)  # Higher probability of promotion
)

# Control group: 15 units
control_group <- tibble(
  id = 1:nc + nt,
  treatment = 0,
  age = runif(nc, min = 30, max = 45),  # Control group is younger
  prev_earnings = runif(nc, min = 65000, max = 85000),  # Lower previous earnings
  # Outcome without treatment
  promotion = rbinom(nc, 1, 0.4)  # Lower probability of promotion
)

# Combine data
full_data <- bind_rows(treatment_group, control_group)

# Print summary statistics by group
cat("Treatment Group Summary:\n")
treatment_group %>%
  summarise(
    n = n(),
    mean_age = mean(age),
    mean_earnings = mean(prev_earnings),
    promotion_rate = mean(promotion)
  ) %>%
  print()

cat("\nControl Group Summary:\n")
control_group %>%
  summarise(
    n = n(),
    mean_age = mean(age),
    mean_earnings = mean(prev_earnings),
    promotion_rate = mean(promotion)
  ) %>%
  print()

# Standardize covariates
standardized_data <- full_data %>%
  mutate(
    age_std = (age - mean(age)) / sd(age),
    earnings_std = (prev_earnings - mean(prev_earnings)) / sd(prev_earnings)
  )

# Function to calculate Euclidean distance between two points
euclidean_distance <- function(x1, x2, y1, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Calculate distances from each treated unit to all control units
matches <- tibble()

for (t_id in standardized_data$id[standardized_data$treatment == 1]) {
  # Get the treated unit
  treated_unit <- standardized_data %>% 
    filter(id == t_id)
  
  # Calculate distances to all control units
  distances <- standardized_data %>%
    filter(treatment == 0) %>%
    mutate(
      treated_id = t_id,
      distance = euclidean_distance(
        treated_unit$age_std, age_std,
        treated_unit$earnings_std, earnings_std
      )
    ) %>%
    arrange(distance)
  
  # Select the closest match
  best_match <- distances %>%
    slice(1)
  
  # Add to matches dataframe
  matches <- bind_rows(matches, best_match)
}

# Print the matches
cat("\nSelected Matches (Best control match for each treated unit):\n")
matches %>%
  select(id, treated_id, age, prev_earnings, promotion, distance) %>%
  print()

# Get the original treated units
treated_units <- standardized_data %>%
  filter(treatment == 1) %>%
  select(id, age, prev_earnings, promotion)

# Combine treated and matched control units for comparison
comparison_data <- matches %>%
  select(id, age, prev_earnings, promotion, distance) %>%
  rename(control_id = id, control_promotion = promotion) %>%
  bind_cols(
    treated_units %>%
      select(treated_id = id, treated_age = age, treated_earnings = prev_earnings, treated_promotion = promotion)
  )

# Print the comparison data
cat("\nTreated-Control Pairs Comparison:\n")
comparison_data %>%
  select(treated_id, control_id, treated_age, control_age = age, 
         treated_earnings, control_earnings = prev_earnings,
         treated_promotion, control_promotion, distance) %>%
  print()

# Calculate Treatment Effect
# 1. Individual treatment effects
individual_effects <- comparison_data %>%
  mutate(individual_effect = treated_promotion - control_promotion)

# 2. Average Treatment Effect (ATE)
ate <- mean(individual_effects$individual_effect)

cat("\nEstimated Average Treatment Effect (ATE):", ate, "\n")

# 3. Actual average outcomes by group for comparison
actual_treatment_outcome <- mean(treatment_group$promotion)
actual_control_outcome <- mean(control_group$promotion)
actual_ate <- actual_treatment_outcome - actual_control_outcome

cat("Actual Treatment Group Promotion Rate:", actual_treatment_outcome, "\n")
cat("Actual Control Group Promotion Rate:", actual_control_outcome, "\n")
cat("Actual Difference in Promotion Rates:", actual_ate, "\n")

# 4. Compare matched pairs with overall groups
cat("\nMatched Analysis vs. Overall Comparison:\n")
matched_control_outcome <- mean(matches$promotion)

cat("Treatment Group Promotion Rate:", actual_treatment_outcome, "\n")
cat("Matched Control Group Promotion Rate:", matched_control_outcome, "\n")
cat("Matched Analysis Treatment Effect:", actual_treatment_outcome - matched_control_outcome, "\n")
