# Clustered SEs: simplified simulation with direct error component specification
# ---------------------------------------------------------------------------

set.seed(42)

# 1) Data-generating process with direct variance specification
simulate_clustered <- function(G = 50, m = 20,
                               beta0 = 1, beta1 = 0.5,
                               sigma_c = 0.5,    # cluster error std dev
                               sigma_e = 1.0) {  # individual error std dev
  n <- G * m
  g <- rep(1:G, each = m)
  
  # Error components
  c_g <- rnorm(G, mean = 0, sd = sigma_c)     # cluster effects
  e_ig <- rnorm(n, mean = 0, sd = sigma_e)    # individual errors
  u <- c_g[g] + e_ig                          # total error
  
  # Regressor: constant within each cluster
  x_cluster <- rnorm(G)                       # one value per cluster
  x <- rep(x_cluster, each = m)               # repeat within clusters
  
  # Outcome
  y <- beta0 + beta1 * x + u
  
  data.frame(y = y, x = x, g = g)
}

# 2) OLS with clustered standard errors - simplified calculation
ols_clustered <- function(dat) {
  # OLS estimation
  fit <- lm(y ~ x, data = dat)
  X <- model.matrix(fit)
  u <- resid(fit)
  n <- nrow(X)
  k <- ncol(X)
  
  # Basic components
  bread <- solve(crossprod(X))              # (X'X)^{-1}
  
  # Homoskedastic variance
  sigma2_hat <- sum(u^2) / (n - k)
  V_homo <- sigma2_hat * bread
  
  # Robust (HC1) variance
  meat_robust <- crossprod(X * u)           # X' diag(u^2) X
  V_robust <- (n/(n-k)) * bread %*% meat_robust %*% bread
  
  # Clustered variance: two-stage meat calculation
  G <- max(dat$g)
  # Stage 1: Compute score for each cluster
  cluster_scores <- matrix(0, G, k)
  for (j in 1:G) {
    idx <- which(dat$g == j)
    X_g <- X[idx,]
    u_g <- u[idx]
    cluster_scores[j, ] <- crossprod(X_g, u_g)  # X_g' * u_g
  }
  
  # Stage 2: Sum outer products across clusters  
  meat_cluster <- crossprod(cluster_scores)     # sum_g (score_g)(score_g)'
  
  # Cluster-robust variance with finite sample correction
  dof_correction <- (G/(G-1)) * ((n-1)/(n-k))
  V_cluster <- dof_correction * bread %*% meat_cluster %*% bread
  
  # Standard errors
  se_homo <- sqrt(diag(V_homo))
  se_robust <- sqrt(diag(V_robust))
  se_cluster <- sqrt(diag(V_cluster))
  
  return(list(
    fit = fit,
    se_table = data.frame(
      homoskedastic = se_homo,
      robust = se_robust,
      clustered = se_cluster
    ),
    variances = list(
      V_homo = V_homo,
      V_robust = V_robust, 
      V_cluster = V_cluster
    )
  ))
}

# 3) Single run example
cat("=== Single simulation example ===\n")
dat <- simulate_clustered(G = 50, m = 20, beta0 = 0, beta1 = 1, 
                          sigma_c = 2, sigma_e = 2)

# Estimate model
result <- ols_clustered(dat)

# 4) Monte Carlo study
monte_carlo <- function(R = 500, G = 50, m = 20, 
                        sigma_c = 2, sigma_e = 2,
                        beta1 = 1, verbose_freq = 100) {
  
  # Storage
  results <- data.frame(
    beta1_hat = numeric(R),
    se_homo = numeric(R),
    se_robust = numeric(R), 
    se_cluster = numeric(R)
  )
  
  for (r in 1:R) {
    # Generate data and estimate
    dat_r <- simulate_clustered(G = G, 
                                m = m, 
                                sigma_c = sigma_c, 
                                sigma_e = sigma_e, 
                                beta1 = beta1)
    res_r <- ols_clustered(dat_r)
    
    # Store results  
    results$beta1_hat[r] <- coef(res_r$fit)["x"]
    results$se_homo[r] <- res_r$se_table["x", "homoskedastic"]
    results$se_robust[r] <- res_r$se_table["x", "robust"] 
    results$se_cluster[r] <- res_r$se_table["x", "clustered"]
  }
  
  # Summary statistics
  mc_sd <- sd(results$beta1_hat)
  mean_ses <- colMeans(results[, c("se_homo", "se_robust", "se_cluster")])
  
  # Coverage rates (95% confidence intervals)
  coverage <- sapply(c("se_homo", "se_robust", "se_cluster"), function(se_col) {
    mean(abs(results$beta1_hat - beta1) <= 1.96 * results[[se_col]])
  })
  names(coverage) <- c("homoskedastic", "robust", "clustered")
  
  summary_table <- data.frame(
    Monte_Carlo_SD = mc_sd,
    Mean_SE_Homo = mean_ses[1],
    Mean_SE_Robust = mean_ses[2], 
    Mean_SE_Cluster = mean_ses[3],
    Coverage_Homo = coverage[1],
    Coverage_Robust = coverage[2],
    Coverage_Cluster = coverage[3]
  )
  
  cat("\n=== Monte Carlo Results ===\n")
  print(round(summary_table, 4))
  
  cat("\nInterpretation:\n")
  
  return(list(summary = summary_table, results = results))
}

# Run Monte Carlo
cat("\n=== Running Monte Carlo Study ===\n")
mc_results <- monte_carlo(R = 300, G = 100, m = 100, sigma_c = 2, sigma_e = 2)

# Plot results
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Histogram of beta1 estimates  
hist(mc_results$results$beta1_hat, main = "Distribution of b1 estimates", 
     xlab = "b1 estimate", col = "lightblue", breaks = 20)
abline(v = 1, col = "red", lwd = 2)
abline(v = mean(mc_results$results$beta1_hat), col = "blue", lwd = 2, lty = 2)

# Standard error comparison
se_data <- mc_results$results[, c("se_homo", "se_robust", "se_cluster")]
boxplot(se_data, main = "Standard Errors Distribution", 
        names = c("Homo", "Robust", "Cluster"),
        col = c("lightblue", "lightgreen", "lightyellow"))
abline(h = sd(mc_results$results$beta1_hat), col = "red", lwd = 2)

cat("\nRed lines show Monte Carlo SD of b1 estimates\n")
cat("Cluster SEs should be closest to this 'true' sampling variability\n")
