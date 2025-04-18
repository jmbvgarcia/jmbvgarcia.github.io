library(tidyverse)

set.seed(12345)

# Number of observations
n <- 10000

# Compliance types
types <- sample(c("complier", "always", "never"),n, 
                prob = c(0.5, 0.25, 0.25), 
                replace = TRUE)

# Generate tibble with baseline potential outcome Y0 depending on type
df <- tibble(
  id = 1:n,
  type = types,
  Z = rbinom(n, 1, 0.5),
  Y0 = case_when(
    type == "complier" ~ rnorm(n, mean = 2, sd = 1),
    type == "always" ~ rnorm(n, mean = 1, sd = 1),
    type == "never" ~ rnorm(n, mean = 3, sd = 1)
  ),
  tau = case_when(
    type == "complier" ~ 2,
    type == "always" ~ 1,
    type == "never" ~ 0
  )
) |>
  mutate(
    D = case_when(
      type == "complier" ~ Z,
      type == "always" ~ 1,
      type == "never" ~ 0
    ),
    Y = Y0 + tau * D
  )

# Estimates
naive <- df |> summarize(naive = mean(Y[D == 1]) - mean(Y[D == 0])) |> pull()

ITT <- df |> summarize(ITT = mean(Y[Z == 1]) - mean(Y[Z == 0])) |> pull()

PrC <- df |> summarize(fs = mean(D[Z == 1]) - mean(D[Z == 0])) |> pull()

LATE <- ITT / first_stage

ATE <- mean(df$tau)

# Show all in a tibble
results <- tibble(
  Estimate = c("Naive", "ITT", "LATE", "ATE"),
  Value = c(naive, ITT, LATE, ATE)
)

print(results)

lm(Y~D,data=df) %>% summary()
lm(Y~Z,data=df) %>% summary()
lm(D~Z,data=df) %>% summary()
ivreg(Y~ D | Z, data=df) %>% summary()
