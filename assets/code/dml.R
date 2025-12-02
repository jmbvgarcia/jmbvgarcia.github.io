# ============================================
# 0. Packages
# ============================================
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(data.table)

set.seed(12345)

sim_dgp_plr_teaching <- function(n = 2000, p = 10, theta0 = 1) {
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("x", 1:p)
  
  # Confounding driven by first 3 covariates, with simple nonlinearities
  h <- 2 * (X[, 1] > 0) + 1.5 * (X[, 2] > 0) + X[, 3]^2
  
  # Treatment and outcome share h(X)
  m0 <- h                  # E[D|X]
  g0 <- h                  # part of E[Y|X]
  
  v   <- rnorm(n)
  eps <- rnorm(n)
  
  d <- m0 + v
  y <- theta0 * d + g0 + eps
  
  data.table(y = y, d = d, X)
}

n <- 2000
p <- 6
theta0 <- 0
dat <- sim_dgp_plr_teaching(n, p, theta0)

# 1) Naive OLS: no controls
ols_naive <- lm(y ~ d, data = dat)
summary(ols_naive)

# 2) OLS with linear controls (still misspecified)
ols_lin <- lm(
  y ~ d + x1 + x2 + x3,  # "reasonable but wrong" spec
  data = dat
)
summary(ols_lin)

# 3) OLS with the right controls
dat_infeasible <- dat
dat_infeasible[, w1:=x1>0] 
dat_infeasible[, w2:=x2>0] 
dat_infeasible[, w3:=x3^2] 

ols_inf <- lm(
  y ~ d + w1 + w2 + w3,  # "reasonable but wrong" spec
  data = dat_infeasible
)
summary(ols_inf)

# 3) DoubleML: PLR with RF nuisance, all X
dml_data <- DoubleMLData$new(
  data   = dat,
  y_col  = "y",
  d_cols = "d",
  x_cols = paste0("x", 1:p)
)

ml_l <- lrn("regr.ranger", num.trees = 500, mtry=5)
ml_m <- lrn("regr.ranger", num.trees = 500, mtry =5)

dml_plr <- DoubleMLPLR$new(
  data   = dml_data,
  ml_l   = ml_l,
  ml_m   = ml_m,
  n_folds = 5,
  score  = "partialling out"
)

dml_plr$fit()
dml_plr$summary()

