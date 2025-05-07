# Simplified Clustered Data Analysis
# This code demonstrates the importance of accounting for clustered errors in statistical inference

# Load necessary packages
library(tidyverse)  # For data manipulation and visualization
library(MASS)       # For mvrnorm (multivariate normal distribution)
library(sandwich)   # For robust standard errors
library(lmtest)     # For coeftest (testing linear model coefficients)

# Function to generate clustered data
generate_clustered_data <- function(intercept = 0, 
                                    slope = 1, 
                                    n_obs = 1000, 
                                    n_clusters = 50, 
                                    cluster_correlation = 0.5) {
  
  # Calculate observations per cluster
  obs_per_cluster <- n_obs / n_clusters
  
  # Generate cluster IDs
  cluster_id <- rep(1:n_clusters, each = obs_per_cluster)
  
  # Generate individual-level components
  individual_components <- mvrnorm(
    n = n_obs, 
    mu = c(0, 0),
    Sigma = matrix(c(1, 0, 0, 1 - cluster_correlation), ncol = 2)
  )
  
  # Generate cluster-level components
  cluster_components <- mvrnorm(
    n = n_clusters, 
    mu = c(0, 0),
    Sigma = matrix(c(1, 0, 0, cluster_correlation), ncol = 2)
  )
  
  # Combine components to create predictor and error terms
  x <- individual_components[, 1] + rep(cluster_components[, 1], each = obs_per_cluster)
  error <- individual_components[, 2] + rep(cluster_components[, 2], each = obs_per_cluster)
  
  # Generate outcome variable
  y <- intercept + slope * x + error
  
  # Return as a tibble
  tibble(x = x, y = y, cluster = cluster_id)
}

# Function to run a single simulation
run_simulation <- function(intercept = 1,
                           slope = 2,
                           n_obs = 1000,
                           n_clusters = 50,
                           cluster_correlation = 0.5,
                           use_robust_se = FALSE) {
  
  # Generate data
  data <- generate_clustered_data(
    intercept = intercept,
    slope = slope,
    n_obs = n_obs,
    n_clusters = n_clusters,
    cluster_correlation = cluster_correlation
  )
  
  # Fit linear model
  model <- lm(y ~ x, data = data)
  
  # Extract coefficient
  estimated_slope <- coef(model)[2]
  
  # Calculate standard errors and confidence intervals
  if (use_robust_se) {
    # Clustered robust standard errors
    vcov_cluster <- sandwich::vcovCL(model, cluster = data$cluster)
    model_summary <- lmtest::coeftest(model, vcov = vcov_cluster)
    se <- model_summary[2, 2]
    
    # Calculate 95% confidence interval
    t_critical <- qt(0.975, df = n_obs - 2)
    ci_lower <- estimated_slope - t_critical * se
    ci_upper <- estimated_slope + t_critical * se
  } else {
    # Regular standard errors
    se <- summary(model)$coefficients[2, 2]
    ci <- confint(model)[2, ]
    ci_lower <- ci[1]
    ci_upper <- ci[2]
  }
  
  # Check if confidence interval contains true parameter value
  contains_true_value <- (ci_lower <= slope) & (ci_upper >= slope)
  
  # Return results
  tibble(
    estimate = estimated_slope,
    std_error = se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    contains_true_value = contains_true_value
  )
}

# Function to run multiple simulations
run_multiple_simulations <- function(n_sims = 1000,
                                     intercept = 1,
                                     slope = 2,
                                     n_obs = 1000,
                                     n_clusters = 50,
                                     cluster_correlation = 0.5,
                                     use_robust_se = FALSE) {
  
  # Run simulations
  results <- map_dfr(1:n_sims, ~{
    run_simulation(
      intercept = intercept,
      slope = slope, 
      n_obs = n_obs,
      n_clusters = n_clusters,
      cluster_correlation = cluster_correlation,
      use_robust_se = use_robust_se
    )
  }, .id = "sim_id")
  
  return(results)
}

# Set random seed for reproducibility
set.seed(42)

# DEMONSTRATION 1: No clustering
# Set parameters (slope = 0 means no effect of x on y)
true_params <- list(intercept = 1, slope = 2)

# Run simulations with no clustering
no_clustering_results <- run_multiple_simulations(
  n_sims = 1000,
  intercept = true_params$intercept,
  slope = true_params$slope,
  cluster_correlation = 0  # No correlation within clusters
)

# DEMONSTRATION 2: With clustering, ignoring cluster structure
clustering_results_naive <- run_multiple_simulations(
  n_sims = 1000,
  intercept = true_params$intercept,
  slope = true_params$slope,
  cluster_correlation = 0.5  # Moderate correlation within clusters
)

# DEMONSTRATION 3: With clustering, using cluster-robust standard errors
clustering_results_robust <- run_multiple_simulations(
  n_sims = 1000,
  intercept = true_params$intercept,
  slope = true_params$slope,
  cluster_correlation = 0.5,  # Moderate correlation within clusters
  use_robust_se = TRUE
)

# VISUALIZATION 1: Histogram of slope estimates
ggplot() +
  geom_histogram(data = no_clustering_results, 
                 aes(x = estimate, fill = "No Clustering"),
                 alpha = 0.5, position = "identity", bins = 30) +
  geom_histogram(data = clustering_results_naive, 
                 aes(x = estimate, fill = "Clustered"),
                 alpha = 0.5, position = "identity", bins = 30) +
  geom_vline(xintercept = true_params$slope, color = "red", linetype = "dashed") +
  labs(
    title = "Distribution of Slope Estimates",
    x = "Estimated Slope",
    y = "Count",
    fill = "Data Structure"
  ) +
  scale_fill_manual(values = c("No Clustering" = "blue", "Clustered" = "green")) +
  theme_minimal()

# VISUALIZATION 2: Confidence intervals (randomly sample 100 for clarity)

ci_no_clustering <- no_clustering_results %>% 
  sample_n(100) %>%
  mutate(method = "No Clustering") 

ci_clustered_naive <- clustering_results_naive %>% 
  sample_n(100) %>%
  mutate(method = "Clustered (Naive)")

ci_clustered_robust <- clustering_results_robust %>% 
  sample_n(100) %>%
  mutate(method = "Clustered (Robust)")

ci_combined <- bind_rows(ci_no_clustering, ci_clustered_naive, ci_clustered_robust)

# Plot confidence intervals for each method
# First, arrange the data in order of estimate values
ci_combined <- ci_combined %>%
  arrange(estimate) %>%
  group_by(method) %>%
  mutate(rank = row_number()) %>%
  ungroup()


# Create plot with only the confidence interval lines (no points)
ci_combined |> filter(method == "Clustered (Robust)") |> 
  ggplot(aes(x = rank, 
           y = estimate, 
           ymin = ci_lower, 
           ymax = ci_upper,
           color = contains_true_value)) +
  geom_errorbar(width = 0) +  # Use errorbar with zero width instead of pointrange
  geom_hline(yintercept = true_params$slope, linetype = "dashed") +
  facet_wrap(~ method, ncol = 1) +
  labs(
    title = "95% Confidence Intervals by Method",
    x = "Rank (Intervals ordered by estimate size)",
    y = "Estimated Slope",
    color = "Contains True Value"
  ) +
  scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
  coord_flip() +
  theme_minimal()

# ANALYSIS: Type I error rates
type1_error_rates <- bind_rows(
  no_clustering_results %>% 
    summarize(method = "No Clustering", 
              type1_error_rate = 1 - mean(contains_true_value)),
  
  clustering_results_naive %>% 
    summarize(method = "Clustered (Naive)", 
              type1_error_rate = 1 - mean(contains_true_value)),
  
  clustering_results_robust %>% 
    summarize(method = "Clustered (Robust)", 
              type1_error_rate = 1 - mean(contains_true_value))
)

# Print type I error rates (probability of falsely rejecting null hypothesis)
print(type1_error_rates)
