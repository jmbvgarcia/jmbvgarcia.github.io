# Exact Inference with Randomization Test in R
# This script demonstrates exact inference calculation using a randomization test approach

# Load required packages
library(tidyverse)
library(magrittr)
library(haven)
library(gridExtra)  # For side-by-side plots

# Function to read data from the 'mixtape' GitHub repository
load_mixtape_data <- function(filename) {
  repo_url <- "https://github.com/scunning1975/mixtape/raw/master/"
  full_path <- paste0(repo_url, filename)
  return(read_dta(full_path))
}

# Load the randomization inference dataset and add ID numbers
ri_data <- load_mixtape_data("ri.dta") %>% 
  mutate(id = 1:8)

# Define which units received treatment in the actual experiment
actual_treated_units <- 1:4

# Generate all possible combinations of 4 treated units out of 8 total units
# Each combination represents one possible randomization scenario
randomization_scenarios <- ri_data %$% 
  as_tibble(t(combn(id, 4))) %>%
  # Rename the columns for clarity
  transmute(
    treated_unit1 = V1, 
    treated_unit2 = V2,
    treated_unit3 = V3, 
    treated_unit4 = V4
  ) %>%
  # Add a unique ID for each randomization scenario
  mutate(scenario_id = 1:n()) %>%
  # Create a full dataset with all units under each randomization scenario
  crossing(., ri_data) %>%
  arrange(scenario_id, name) %>% 
  # Mark each unit as treated (1) or control (0) based on the scenario
  mutate(is_treated = case_when(
    id == treated_unit1 | id == treated_unit2 |
      id == treated_unit3 | id == treated_unit4 ~ 1,
    TRUE ~ 0
  ))

# Calculate the mean outcome for treated units in each randomization scenario
treated_means <- randomization_scenarios %>%
  group_by(scenario_id) %>%
  filter(is_treated == 1) %>% 
  summarize(mean_treated = mean(y, na.rm = TRUE))

# Calculate the mean outcome for control units in each randomization scenario
control_means <- randomization_scenarios %>%
  group_by(scenario_id) %>%
  filter(is_treated == 0) %>% 
  summarize(mean_control = mean(y, na.rm = TRUE))

# Calculate treatment effects for each randomization scenario
treatment_effects <- inner_join(treated_means, control_means, by = "scenario_id") %>%
  mutate(effect_size = mean_treated - mean_control)

# Calculate total number of randomization scenarios
total_scenarios <- nrow(treatment_effects)

# Calculate p-value by determining how many scenarios have a treatment effect
# at least as large as observed in the actual experiment (scenario_id = 1)
p_value <- treatment_effects %>%
  arrange(desc(abs(effect_size))) %>% 
  mutate(rank = 1:total_scenarios) %>% 
  filter(scenario_id == 1) %>%
  pull(rank) / total_scenarios

# Extract the actual observed effect (from scenario_id = 1)
actual_effect <- treatment_effects %>%
  filter(scenario_id == 1) %>%
  pull(effect_size)

# Display the p-value
print(paste("P-value from randomization inference:", p_value))
print(paste("Actual observed treatment effect:", round(actual_effect, 3)))

# Create a visualization of the randomization distribution
ggplot(treatment_effects, aes(x = effect_size)) +
  # Add histogram of all possible treatment effects
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "white", alpha = 0.7) +
  # Add vertical line for the actual observed effect
  geom_vline(xintercept = actual_effect, color = "red", linewidth = 1.2, linetype = "dashed") +
  # Add text annotation for the actual effect
  annotate("text", x = actual_effect, y = max(table(cut(treatment_effects$effect_size, breaks = 20)))/1.5, 
           label = paste("Actual effect:", round(actual_effect, 2)), 
           color = "red", hjust = -0.1, size = 4) +
  # Add title and labels
  labs(
    title = "Randomization Distribution of Treatment Effects",
    x = "Treatment Effect",
    y = "Frequency",
  ) +
  # Apply a clean theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ============================================================================
# PART 2: Exact Inference Using Mean Rank Differences
# ============================================================================

# Function to compute centralized ranks
# Takes a vector, computes ranks with ties handled as averages, then centralizes by subtracting mean
compute_centralized_ranks <- function(values) {
  # Calculate ranks (with average for ties)
  ranks <- rank(values, ties.method = "average")
  # Centralize ranks by subtracting the mean (so they sum to 0)
  centralized_ranks <- ranks - mean(ranks)
  return(centralized_ranks)
}

# For each randomization scenario, we need to:
# 1. Compute ranks for all units based on outcome y
# 2. Calculate the mean rank for treated and control groups
# 3. Find the difference between these mean ranks

# First, we compute centralized ranks within each randomization scenario
rank_scenarios <- randomization_scenarios %>%
  group_by(scenario_id) %>%
  # Compute centralized ranks of y within each scenario
  mutate(centralized_rank = compute_centralized_ranks(y)) %>%
  ungroup()

# Calculate the mean centralized rank for treated units in each scenario
treated_ranks <- rank_scenarios %>%
  group_by(scenario_id) %>%
  filter(is_treated == 1) %>% 
  summarize(mean_rank_treated = mean(centralized_rank, na.rm = TRUE))

# Calculate the mean centralized rank for control units in each scenario
control_ranks <- rank_scenarios %>%
  group_by(scenario_id) %>%
  filter(is_treated == 0) %>% 
  summarize(mean_rank_control = mean(centralized_rank, na.rm = TRUE))

# Calculate rank-based treatment effects for each randomization scenario
rank_effects <- inner_join(treated_ranks, control_ranks, by = "scenario_id") %>%
  mutate(rank_effect = mean_rank_treated - mean_rank_control)

# Calculate p-value based on rank differences
rank_p_value <- rank_effects %>%
  arrange(desc(abs(rank_effect))) %>% 
  mutate(rank = 1:n()) %>% 
  filter(scenario_id == 1) %>%
  pull(rank) / total_scenarios

# Extract the actual observed rank effect (from scenario_id = 1)
actual_rank_effect <- rank_effects %>%
  filter(scenario_id == 1) %>%
  pull(rank_effect)

# Display the rank-based p-value
print(paste("P-value from rank-based randomization inference:", rank_p_value))
print(paste("Actual observed rank-based effect:", round(actual_rank_effect, 3)))

# Create a visualization of the rank-based randomization distribution
ggplot(rank_effects, aes(x = rank_effect)) +
  # Add histogram of all possible rank-based treatment effects
  geom_histogram(binwidth = 0.25, fill = "lightgreen", color = "white", alpha = 0.7) +
  # Add vertical line for the actual observed effect
  geom_vline(xintercept = actual_rank_effect, color = "darkgreen", linewidth = 1.2, linetype = "dashed") +
  # Add text annotation for the actual effect
  annotate("text", x = actual_rank_effect, y = max(table(cut(rank_effects$rank_effect, breaks = 20)))/1.5, 
           label = paste("Actual rank effect:", round(actual_rank_effect, 2)), 
           color = "darkgreen", hjust = -0.1, size = 4) +
  # Add title and labels
  labs(
    title = "Rank-Based Randomization Distribution",
    subtitle = "Histogram of effects using centralized mean ranks",
    x = "Rank-Based Treatment Effect",
    y = "Frequency",
    caption = "Green dashed line represents the actual observed rank-based effect"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Compare the two approaches side by side
grid.arrange(
  # Plot 1: Mean difference approach
  ggplot(treatment_effects, aes(x = effect_size)) +
    geom_histogram(binwidth = 0.5, fill = "skyblue", color = "white", alpha = 0.7) +
    geom_vline(xintercept = actual_effect, color = "red", linewidth = 1.2, linetype = "dashed") +
    labs(
      title = "Mean Difference Approach",
      x = "Effect Size (Mean Difference)",
      y = "Frequency"
    ) +
    annotate("text", x = max(treatment_effects$effect_size) * 0.7, 
             y = max(table(cut(treatment_effects$effect_size, breaks = 20)))/1.5, 
             label = paste("p =", round(p_value, 3)), size = 4) +
    theme_minimal(),
  
  # Plot 2: Mean rank difference approach
  ggplot(rank_effects, aes(x = rank_effect)) +
    geom_histogram(binwidth = 0.25, fill = "lightgreen", color = "white", alpha = 0.7) +
    geom_vline(xintercept = actual_rank_effect, color = "darkgreen", linewidth = 1.2, linetype = "dashed") +
    labs(
      title = "Mean Rank Difference Approach",
      x = "Effect Size (Mean Rank Difference)",
      y = "Frequency"
    ) +
    annotate("text", x = max(rank_effects$rank_effect) * 0.7, 
             y = max(table(cut(rank_effects$rank_effect, breaks = 20)))/1.5, 
             label = paste("p =", round(rank_p_value, 3)), size = 4) +
    theme_minimal(),
  
  ncol = 2,
  top = "Comparison of Inference Methods"
)