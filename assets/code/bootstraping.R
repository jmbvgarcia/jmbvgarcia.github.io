################################################################################
# INTERLUDIO: BOOTSTRAP
################################################################################

# --- Generar datos con heterocedasticidad ---
cat("--- Generando datos con heterocedasticidad ---\n")

n <- 500
x <- runif(n, 0, 10)
# Heterocedasticidad: varianza aumenta con x
epsilon <- rnorm(n, 0, sd = 0.5 + 2 * x^2)
y <- 2 + 1.5 * x + epsilon

# Visualizar datos
plot(x, y, pch = 19, col = rgb(0, 0, 1, 0.5),
     main = "Datos con Heterocedasticidad",
     xlab = "X", ylab = "Y")
abline(lm(y ~ x), col = "red", lwd = 2)

# --- Estimación OLS ---
cat("\n--- Estimación OLS ---\n")
model_ols <- lm(y ~ x)
beta_hat <- coef(model_ols)[2]

cat("Coeficiente estimado (beta):", round(beta_hat, 4), "\n")

# Error estándar tradicional (INCORRECTO bajo heterocedasticidad)
se_traditional <- summary(model_ols)$coefficients[2, 2]
cat("SE tradicional (supone homocedasticidad):", round(se_traditional, 4), "\n")

# Error estándar robusto (correcto)
if(require(sandwich, quietly = TRUE) & require(lmtest, quietly = TRUE)) {
  se_robust <- sqrt(vcovHC(model_ols, type = "HC1")[2, 2])
  cat("SE robusto (Huber-White):", round(se_robust, 4), "\n")
}

# --- Bootstrap para estimar SE ---
cat("\n--- Bootstrap para estimar SE ---\n")

B <- 1000  # Número de muestras bootstrap
beta_boot <- numeric(B)

cat("Generando", B, "muestras bootstrap...\n")

for (b in 1:B) {
  # Remuestrear índices con reemplazo
  boot_idx <- sample(1:n, n, replace = TRUE)
  
  # Crear muestra bootstrap
  x_boot <- x[boot_idx]
  y_boot <- y[boot_idx]
  
  # Estimar beta en muestra bootstrap
  model_boot <- lm(y_boot ~ x_boot)
  beta_boot[b] <- coef(model_boot)[2]
}

# Calcular SE bootstrap
se_bootstrap <- sd(beta_boot)
cat("SE bootstrap:", round(se_bootstrap, 4), "\n")

# --- Comparación ---
cat("\n--- Comparación de Errores Estándar ---\n")
comparison_se <- data.frame(
  Método = c("Tradicional (OLS)", "Bootstrap", "Robusto (HC1)"),
  SE = c(se_traditional, se_bootstrap, 
         if(exists("se_robust")) se_robust else NA)
)
print(comparison_se)


# --- Visualizar distribución bootstrap ---
cat("\n--- Distribución Bootstrap de Beta ---\n")

hist(beta_boot, breaks = 30, col = rgb(0.5, 0.5, 1, 0.5),
     main = "Distribución Bootstrap de Beta",
     xlab = "Beta estimado",
     xlim = c(beta_hat - 4*se_bootstrap, beta_hat + 4*se_bootstrap))
abline(v = beta_hat, col = "red", lwd = 2, lty = 1)
abline(v = beta_hat - 1.96*se_bootstrap, col = "blue", lwd = 2, lty = 2)
abline(v = beta_hat + 1.96*se_bootstrap, col = "blue", lwd = 2, lty = 2)

# --- Intervalo de confianza bootstrap ---
cat("\n--- Intervalo de Confianza Bootstrap (95%) ---\n")

# Método percentil
ic_lower <- quantile(beta_boot, 0.025)
ic_upper <- quantile(beta_boot, 0.975)

cat("IC (método percentil): [", round(ic_lower, 4), ",", round(ic_upper, 4), "]\n")

# Método normal (basado en SE bootstrap)
ic_normal_lower <- beta_hat - 1.96 * se_bootstrap
ic_normal_upper <- beta_hat + 1.96 * se_bootstrap

cat("IC (método normal):    [", round(ic_normal_lower, 4), ",", 
    round(ic_normal_upper, 4), "]\n")

# --- El famoso 37% ---
cat("\n--- Proporción de observaciones OOB ---\n")

# En cada muestra bootstrap, ¿cuántas obs únicas?
unique_counts <- numeric(100)
for (b in 1:100) {
  boot_idx <- sample(1:n, n, replace = TRUE)
  unique_counts[b] <- length(unique(boot_idx))
}

prop_in_bag <- mean(unique_counts) / n
prop_oob <- 1 - prop_in_bag

cat("Proporción promedio 'in-bag':", round(prop_in_bag, 3), "\n")
cat("Proporción promedio 'out-of-bag' (OOB):", round(prop_oob, 3), "\n")
cat("Valor teórico (1 - 1/e):", round(1 - 1/exp(1), 3), "\n")
