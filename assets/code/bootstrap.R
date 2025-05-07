# Bootstrap Standard Errors for Regression Coefficients
# This script demonstrates bootstrapping to estimate standard errors of regression coefficients

# Load required packages
library(tidyverse)
library(broom)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# 1. Generate sample data with a true linear relationship
n <- 100  # Sample size
beta_0 <- 3  # True intercept
beta_1 <- 2  # True slope
sigma <- 5   # Error standard deviation

# Generate predictor variable
x <- runif(n, min = 0, max = 10)

# Generate response with noise
y <- beta_0 + beta_1 * x + rnorm(n, mean = 0, sd = sigma)

# Create dataset
data <- tibble(x = x, y = y)

# 2. Fit the original linear model
original_model <- lm(y ~ x, data = data)
(summary_original <- summary(original_model))

# Extract coefficient estimates and standard errors
coef_original <- tidy(original_model)

print(coef_original)

# 3. Visualize the data and regression line
regression_plot <- ggplot(data, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(
    title = "Linear Regression Model",
    subtitle = paste0("y = ", round(beta_0, 2), " + ", round(beta_1, 2), "x + error"),
    caption = "Blue line: OLS fit; Gray area: 95% confidence interval"
  ) +
  theme_minimal()

print(regression_plot)

# 4. Implement bootstrap for regression coefficients
bootstrap_regression <- function(data, num_bootstraps = 1000) {
  n <- nrow(data)
  
  # Prepare storage for coefficient estimates
  results <- tibble(
    bootstrap_id = 1:num_bootstraps,
    intercept = numeric(num_bootstraps),
    slope = numeric(num_bootstraps)
  )
  
  for (i in 1:num_bootstraps) {
    # Sample with replacement
    boot_indices <- sample(1:n, n, replace = TRUE)
    boot_data <- data[boot_indices, ]
    
    # Fit regression model on bootstrap sample
    boot_model <- lm(y ~ x, data = boot_data)
    
    # Store coefficient estimates
    results$intercept[i] <- coef(boot_model)[1]
    results$slope[i] <- coef(boot_model)[2]
  }
  
  return(results)
}

# 5. Run bootstrap
num_bootstraps <- 2000
bootstrap_results <- bootstrap_regression(data, num_bootstraps)

# 6. Analyze bootstrap results
bootstrap_summary <- bootstrap_results %>%
  pivot_longer(
    cols = c(intercept, slope),
    names_to = "coefficient",
    values_to = "estimate"
  ) %>%
  group_by(coefficient) %>%
  summarize(
    mean = mean(estimate),
    bootstrap_se = sd(estimate),
    lower_ci = quantile(estimate, 0.025),
    upper_ci = quantile(estimate, 0.975)
  )

print(bootstrap_summary)

# Compare with original standard errors
comparison <- coef_original %>%
  select(term, estimate, std.error) %>%
  mutate(
    method = "Traditional",
    term = case_when(
      term == "(Intercept)" ~ "intercept",
      term == "x" ~ "slope",
      TRUE ~ term
    )
  ) %>%
  rename(coefficient = term) %>%
  bind_rows(
    bootstrap_summary %>%
      select(coefficient, estimate = mean, std.error = bootstrap_se) %>%
      mutate(method = "Bootstrap")
  )

print(comparison)

# 7. Visualize the bootstrap distributions
bootstrap_plots <- bootstrap_results %>%
  pivot_longer(
    cols = c(intercept, slope),
    names_to = "coefficient", 
    values_to = "estimate"
  ) %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(
    data = comparison %>% filter(method == "Traditional"),
    aes(xintercept = estimate),
    color = "red", linewidth = 1
  ) +
  geom_vline(
    data = comparison %>% filter(method == "Traditional"),
    aes(xintercept = estimate - 1.96 * std.error),
    color = "red", linetype = "dashed", linewidth = 0.8
  ) +
  geom_vline(
    data = comparison %>% filter(method == "Traditional"),
    aes(xintercept = estimate + 1.96 * std.error),
    color = "red", linetype = "dashed", linewidth = 0.8
  ) +
  facet_wrap(~ coefficient, scales = "free", ncol = 1) +
  labs(
    title = "Bootstrap Distributions for Regression Coefficients",
    x = "Coefficient Estimate",
    y = "Frequency"
  ) +
  theme_minimal()

print(bootstrap_plots)

