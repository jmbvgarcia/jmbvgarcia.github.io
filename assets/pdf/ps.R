# Propensity Score Example: NSW vs. CPS Controls
library(tidyverse)
library(haven)

# Read data
read_data <- function(df) {
  full_path <- paste0("https://github.com/scunning1975/mixtape/raw/master/", df)
  read_dta(full_path)
}

# Load and clean treated sample
nsw_dw <- read_data("nsw_mixtape.dta") %>%
  filter(treat == 1)

# Load CPS controls and bind with treated sample
data <- read_data("cps_mixtape.dta") %>% 
  bind_rows(nsw_dw) %>% 
  mutate(
    agesq = age^2,
    agecube = age^3,
    educsq = educ^2,
    u74 = if_else(re74 == 0, 1, 0),
    u75 = if_else(re75 == 0, 1, 0),
    interaction1 = educ * re74,
    re74sq = re74^2,
    re75sq = re75^2,
    interaction2 = u74 * hisp,
    id = row_number()
  )

# Estimate propensity score via logistic regression
logit_nsw <- glm(
  treat ~ age + agesq + agecube + educ + educsq + 
    marr + nodegree + black + hisp + re74 + re75 + u74 +
    u75 + interaction1,
  family = binomial(link = "logit"),
  data = data
)

data <- data %>% 
  mutate(pscore = predict(logit_nsw, type = "response"))


# Plot histogram of PS by group
data %>% filter(treat==1) %>% 
  ggplot(aes(x = pscore)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
  labs(title = "Propensity Score Distribution", x = "Propensity Score", fill = "Treatment") +
  theme_minimal()

data %>% filter(treat==0) %>% 
  ggplot(aes(x = pscore)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
  labs(title = "Propensity Score Distribution", x = "Propensity Score", fill = "Treatment") +
  theme_minimal()



# A. Regression controlling for PS
model_ps_reg <- lm(re78 ~ treat + pscore, data = data)
summary(model_ps_reg)

# B. Nearest neighbor matching on the PS 
treated <- data %>% filter(treat == 1)
control <- data %>% filter(treat == 0)

match_nn_ps <- treated %>%
  rowwise() %>%
  mutate(
    match_id = control$id[which.min(abs(control$pscore - pscore))],
    match_outcome = control$re78[which.min(abs(control$pscore - pscore))],
    match_distance = min(abs(control$pscore - pscore))
  ) %>%
  ungroup()

att_nn <- mean(match_nn_ps$re78 - match_nn_ps$match_outcome)
cat("\nNearest neighbor PS matching ATT:", round(att_nn, 2), "\n")

# C. Coarsened exact matching on the PS (manual binning)
bins <- quantile(data$pscore, probs = seq(0, 1, by = 0.01))
data <- data %>%
  mutate(pscore_bin = cut(pscore, breaks = bins, include.lowest = TRUE))

# Drop unmatched bins
matched_bins <- data %>%
  group_by(pscore_bin, treat) %>%
  summarise(n = n(), .groups = "drop") %>%
  count(pscore_bin) %>%
  filter(n == 2) %>%
  pull(pscore_bin)

cem_data <- data %>%
  filter(pscore_bin %in% matched_bins)

# Compute bin-specific effects and aggregate to ATE
bin_effects <- cem_data %>%
  group_by(pscore_bin) %>%
  summarise(
    mean_treated = mean(re78[treat == 1]),
    mean_control = mean(re78[treat == 0]),
    n = n(),
    n_t = sum(treat),
    diff = mean_treated - mean_control,
    .groups = "drop"
  )

ate_cem <- weighted.mean(bin_effects$diff, w = bin_effects$n)
cat("\nCEM ATE (weighted bin differences):", round(ate_cem, 2), "\n")

att_cem <- weighted.mean(bin_effects$diff, w = bin_effects$n_t)
cat("\nCEM ATT (weighted bin differences):", round(att_cem, 2), "\n")


# D. IPW
data <- data %>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1-treat) * re78/(1-pscore),
         ht = y1 - y0)

data %>% 
  pull(ht) %>% 
  mean()

data %>% filter(pscore<0.95 & pscore>0.05) %>% 
  pull(ht) %>% 
  mean()


