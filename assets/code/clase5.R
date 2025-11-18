################################################################################
# Clase 5: Mas Alla de la Linealidad - Ejemplo Aplicado
# Perfil de Ingresos por Edad usando Splines
#
# Dataset: ISLR::Wage
# Objetivo: Modelar la relacion no lineal entre edad y salarios
################################################################################

# Paquetes necesarios
library(ISLR)        # Para datos de Wage
library(tidyverse)   # Manipulacion de datos y graficos
library(splines)     # Para B-splines y natural splines
library(broom)       # Para tidy model outputs

# Configuracion
set.seed(12345)

################################################################################
# PARTE 1: EXPLORACION DE DATOS
################################################################################

cat("\n=== CARGANDO Y EXPLORANDO DATOS ===\n")

# Cargar datos
data(Wage)

# Resumen basico
cat("\nDimensiones del dataset:\n")
cat("Observaciones:", nrow(Wage), "\n")
cat("Variables:", ncol(Wage), "\n")

# Variables de interes
cat("\nVariables principales:\n")
cat("- wage: Salario por hora (miles de dolares)\n")
cat("- age: Edad del trabajador\n")
cat("- education: Nivel educativo\n")
cat("- year: Anio de observacion\n")

# Estadisticas descriptivas
cat("\nEstadisticas descriptivas:\n")
summary(Wage %>% select(wage, age, education))

# Remover outliers extremos para visualizacion
wage_data <- Wage %>% 
  filter(wage < 250, as.numeric(region)==2, as.numeric(jobclass)==1) %>%
  select(wage, age, year, education, race)

model <-lm(wage~factor(year)+education + race, data=wage_data)

summary(model)
wage_data[["wage"]] <- model$residuals


cat("\nDespues de remover outliers extremos (wage >= 250):\n")
cat("Observaciones restantes:", nrow(wage_data), "\n")

################################################################################
# VISUALIZACION 1: Dispersion basica
################################################################################

cat("\n=== VISUALIZACION INICIAL ===\n")

p1 <- ggplot(wage_data, aes(x = age, y = wage)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Salarios vs Edad (con smoother LOESS)",
       x = "Edad (anos)", 
       y = "Salario (miles de dolares por hora)") +
  theme_minimal(base_size = 12)

print(p1)

cat("\nObservacion: La relacion claramente no es lineal.\n")
cat("Parece haber un pico alrededor de los 45-50 anos.\n")

################################################################################
# PARTE 2: MODELOS DE COMPLEJIDAD CRECIENTE
################################################################################

cat("\n\n=== AJUSTANDO MODELOS DE DIFERENTES COMPLEJIDADES ===\n")

# Dividir datos en entrenamiento (70%) y prueba (30%)
n <- nrow(wage_data)
train_idx <- sample(1:n, size = floor(0.6 * n))
train_data <- wage_data[train_idx, ]
test_data <- wage_data[-train_idx, ]

cat("\nDatos de entrenamiento:", nrow(train_data), "\n")
cat("Datos de prueba:", nrow(test_data), "\n")

################################################################################
# Modelo 1: Lineal
################################################################################

cat("\n--- MODELO 1: LINEAL ---\n")
fit_linear <- lm(wage ~ age, data = train_data)

cat("Coeficientes:\n")
print(tidy(fit_linear))

# Predicciones
pred_linear_train <- predict(fit_linear, newdata = train_data)
pred_linear_test <- predict(fit_linear, newdata = test_data)

# Errores
rss_linear <- sum((train_data$wage - pred_linear_train)^2)
mse_linear_train <- mean((train_data$wage - pred_linear_train)^2)
mse_linear_test <- mean((test_data$wage - pred_linear_test)^2)

cat(sprintf("RSS (in-sample): %.2f\n", rss_linear))
cat(sprintf("MSE entrenamiento: %.2f\n", mse_linear_train))
cat(sprintf("MSE prueba: %.2f\n", mse_linear_test))

################################################################################
# Modelos 2-7: Polinomios de grado 2, 3, 5, 10, 15, 20
################################################################################

cat("\n--- MODELOS POLINOMIALES ---\n")

degrees <- c(2, 3, 5, 10, 15, 20)
poly_results <- tibble(
  degree = integer(),
  rss = numeric(),
  mse_train = numeric(),
  mse_test = numeric(),
  aic = numeric(),
  bic = numeric()
)

for (d in degrees) {
  cat(sprintf("\nAjustando polinomio de grado %d...\n", d))
  
  # Ajustar modelo
  fit_poly <- lm(wage ~ poly(age, d), data = train_data)
  
  # Predicciones
  pred_poly_train <- predict(fit_poly, newdata = train_data)
  pred_poly_test <- predict(fit_poly, newdata = test_data)
  
  # Errores
  rss <- sum((train_data$wage - pred_poly_train)^2)
  mse_train <- mean((train_data$wage - pred_poly_train)^2)
  mse_test <- mean((test_data$wage - pred_poly_test)^2)
  
  # Criterios de informacion
  aic_val <- AIC(fit_poly)
  bic_val <- BIC(fit_poly)
  
  cat(sprintf("  RSS: %.2f | MSE train: %.2f | MSE test: %.2f\n", 
              rss, mse_train, mse_test))
  
  poly_results <- poly_results %>%
    add_row(
      degree = d,
      rss = rss,
      mse_train = mse_train,
      mse_test = mse_test,
      aic = aic_val,
      bic = bic_val
    )
}

# Agregar modelo lineal para comparacion
poly_results <- poly_results %>%
  add_row(
    degree = 1,
    rss = rss_linear,
    mse_train = mse_linear_train,
    mse_test = mse_linear_test,
    aic = AIC(fit_linear),
    bic = BIC(fit_linear),
    .before = 1
  )

cat("\n=== RESUMEN DE RESULTADOS POLINOMIALES ===\n")
print(poly_results)

################################################################################
# VISUALIZACION 2: Comparacion de polinomios
################################################################################

cat("\n=== VISUALIZANDO POLINOMIOS ===\n")

# Crear grid de edades para predicciones suaves
age_grid <- seq(min(wage_data$age), max(wage_data$age), length.out = 200)

# Ajustar modelos para visualizacion (en todos los datos para ver el comportamiento)
poly_preds <- tibble(age = age_grid)

for (d in c(1, 2, 4, 10, 15)) {
  fit_temp <- lm(wage ~ poly(age, d), data = wage_data)
  poly_preds[[paste0("degree_", d)]] <- predict(fit_temp, 
                                                  newdata = tibble(age = age_grid))
}

# Graficar
poly_preds_long <- poly_preds %>%
  pivot_longer(-age, names_to = "model", values_to = "prediction") %>%
  mutate(model = str_replace(model, "degree_", "Grado "))

p2 <- ggplot() +
  geom_point(data = wage_data, aes(x = age, y = wage), 
             alpha = 0.1, size = 0.5) +
  geom_line(data = poly_preds_long, 
            aes(x = age, y = prediction, color = model), 
            size = 1) +
  labs(title = "Comparacion de Polinomios de Diferentes Grados",
       subtitle = "Observa el overfitting en grados altos",
       x = "Edad", y = "Salario",
       color = "Modelo") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p2)

################################################################################
# VISUALIZACION 3: Trade-off sesgo-varianza
################################################################################

cat("\n=== TRADE-OFF SESGO-VARIANZA ===\n")

p3 <- poly_results %>% filter(degree<15) |> 
  pivot_longer(c(mse_train, mse_test), 
               names_to = "conjunto", 
               values_to = "mse") %>%
  mutate(conjunto = ifelse(conjunto == "mse_train", 
                           "Entrenamiento", "Prueba")) %>%
  ggplot(aes(x = degree, y = mse, color = conjunto)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Entrenamiento" = "blue", "Prueba" = "red")) +
  labs(title = "MSE vs Grado del Polinomio",
       subtitle = "Observa el overfitting: MSE de prueba aumenta con complejidad",
       x = "Grado del Polinomio",
       y = "MSE",
       color = "Conjunto") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p3)

cat("\nObservacion clave:\n")
cat("- MSE de entrenamiento disminuye monotonicamente (mejor ajuste)\n")
cat("- MSE de prueba tiene forma de U (overfitting despues del minimo)\n")
cat("- Grado optimo segun MSE de prueba: ", 
    poly_results$degree[which.min(poly_results$mse_test)], "\n")

################################################################################
# PARTE 3: REGRESION CON SPLINES
################################################################################

cat("\n\n=== REGRESION CON SPLINES ===\n")

################################################################################
# Modelo con B-splines (knots fijos)
################################################################################

cat("\n--- B-SPLINES CON KNOTS FIJOS ---\n")

# Colocar knots en cuartiles de edad
knots <- quantile(train_data$age, probs = c(0.25, 0.5, 0.75))
cat("Knots colocados en cuartiles de edad:\n")
print(knots)

# Ajustar modelo
fit_bs <- lm(wage ~ bs(age, knots = knots), data = train_data)

cat("\nNumero de parametros:", length(coef(fit_bs)), "\n")

# Predicciones y errores
pred_bs_train <- predict(fit_bs, newdata = train_data)
pred_bs_test <- predict(fit_bs, newdata = test_data)

mse_bs_train <- mean((train_data$wage - pred_bs_train)^2)
mse_bs_test <- mean((test_data$wage - pred_bs_test)^2)

cat(sprintf("MSE entrenamiento: %.2f\n", mse_bs_train))
cat(sprintf("MSE prueba: %.2f\n", mse_bs_test))

################################################################################
# Modelo con natural splines
################################################################################

cat("\n--- NATURAL CUBIC SPLINES ---\n")

# Natural splines: lineales en las colas
fit_ns <- lm(wage ~ ns(age, df = 5), data = train_data)

pred_ns_train <- predict(fit_ns, newdata = train_data)
pred_ns_test <- predict(fit_ns, newdata = test_data)

mse_ns_train <- mean((train_data$wage - pred_ns_train)^2)
mse_ns_test <- mean((test_data$wage - pred_ns_test)^2)

cat(sprintf("MSE entrenamiento: %.2f\n", mse_ns_train))
cat(sprintf("MSE prueba: %.2f\n", mse_ns_test))

################################################################################
# PARTE 4: SMOOTHING SPLINES CON CROSS-VALIDATION
################################################################################

cat("\n\n=== SMOOTHING SPLINES CON CROSS-VALIDATION ===\n")

# Probar diferentes valores de df (grados de libertad efectivos)
df_values <- seq(3, 15, by = 1)
cv_results <- tibble(
  df = integer(),
  mse_cv = numeric()
)

cat("Probando valores de df desde 3 hasta 15...\n")

# K-fold cross-validation (k=5)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(train_data)))

for (df_val in df_values) {
  mse_folds <- numeric(k)
  
  for (fold in 1:k) {
    # Dividir en train y validation
    train_fold <- train_data[folds != fold, ]
    val_fold <- train_data[folds == fold, ]
    
    # Ajustar smoothing spline
    fit_smooth <- smooth.spline(train_fold$age, train_fold$wage, df = df_val)
    
    # Predecir en validation
    pred_val <- predict(fit_smooth, x = val_fold$age)$y
    
    # Calcular MSE
    mse_folds[fold] <- mean((val_fold$wage - pred_val)^2)
  }
  
  # Promedio de MSE sobre folds
  mse_cv <- mean(mse_folds)
  
  cv_results <- cv_results %>%
    add_row(df = df_val, mse_cv = mse_cv)
}

cat("\n=== RESULTADOS DE CROSS-VALIDATION ===\n")
print(cv_results)

# Mejor df
best_df <- cv_results$df[which.min(cv_results$mse_cv)]
cat(sprintf("\nMejor df segun CV: %d\n", best_df))
cat(sprintf("MSE de CV: %.2f\n", min(cv_results$mse_cv)))

# Graficar resultados de CV
p4 <- ggplot(cv_results, aes(x = df, y = mse_cv)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = best_df, linetype = "dashed", color = "red") +
  annotate("text", x = best_df + 0.5, y = max(cv_results$mse_cv), 
           label = paste("Optimo:", best_df), color = "red") +
  labs(title = "Cross-Validation para Seleccion de Grados de Libertad",
       x = "Grados de Libertad (df)",
       y = "MSE de Cross-Validation") +
  theme_minimal(base_size = 12)

print(p4)

################################################################################
# Ajustar modelo final con mejor df
################################################################################

cat("\n--- MODELO FINAL CON SMOOTHING SPLINE ---\n")

fit_smooth_final <- smooth.spline(train_data$age, train_data$wage, df = best_df)

# Predicciones
pred_smooth_train <- predict(fit_smooth_final, x = train_data$age)$y
pred_smooth_test <- predict(fit_smooth_final, x = test_data$age)$y

mse_smooth_train <- mean((train_data$wage - pred_smooth_train)^2)
mse_smooth_test <- mean((test_data$wage - pred_smooth_test)^2)

cat(sprintf("MSE entrenamiento: %.2f\n", mse_smooth_train))
cat(sprintf("MSE prueba: %.2f\n", mse_smooth_test))

################################################################################
# PARTE 5: COMPARACION FINAL DE TODOS LOS METODOS
################################################################################

cat("\n\n=== COMPARACION FINAL DE TODOS LOS METODOS ===\n")

# Recopilar resultados
final_comparison <- tibble(
  Modelo = c("Lineal", 
             "Polinomio (grado 2)", 
             "Polinomio (grado 5)",
             "Polinomio (grado 10)",
             "Polinomio (grado 20)",
             "B-splines (3 knots)",
             "Natural Splines (df=5)",
             paste0("Smoothing Spline (df=", best_df, ")")),
  MSE_train = c(mse_linear_train,
                poly_results$mse_train[poly_results$degree == 2],
                poly_results$mse_train[poly_results$degree == 5],
                poly_results$mse_train[poly_results$degree == 10],
                poly_results$mse_train[poly_results$degree == 20],
                mse_bs_train,
                mse_ns_train,
                mse_smooth_train),
  MSE_test = c(mse_linear_test,
               poly_results$mse_test[poly_results$degree == 2],
               poly_results$mse_test[poly_results$degree == 5],
               poly_results$mse_test[poly_results$degree == 10],
               poly_results$mse_test[poly_results$degree == 20],
               mse_bs_test,
               mse_ns_test,
               mse_smooth_test)
) %>%
  mutate(
    Diferencia = MSE_test - MSE_train
  )

print(final_comparison)

cat("\nObservaciones:\n")
cat("1. Modelo lineal: Sesgo alto (MSE alto en ambos conjuntos)\n")
cat("2. Polinomio grado 10: Overfitting severo (gran diferencia entre train y test)\n")
cat("3. Splines: Balance entre sesgo y varianza\n")
cat("4. Smoothing spline con CV: Mejor desempeno en conjunto de prueba\n")

################################################################################
# VISUALIZACION FINAL: Comparacion visual
################################################################################

cat("\n=== VISUALIZACION FINAL ===\n")

# Crear predicciones para todos los modelos en grid
pred_final <- tibble(age = age_grid)

# Modelos seleccionados
fit_linear_full <- lm(wage ~ age, data = wage_data)
fit_poly4_full <- lm(wage ~ poly(age, 4), data = wage_data)
fit_bs_full <- lm(wage ~ bs(age, knots = knots), data = wage_data)
fit_smooth_full <- smooth.spline(wage_data$age, wage_data$wage, df = best_df)

pred_final <- pred_final %>%
  mutate(
    Lineal = predict(fit_linear_full, newdata = tibble(age = age_grid)),
    `Polinomio (d=4)` = predict(fit_poly4_full, newdata = tibble(age = age_grid)),
    `B-splines` = predict(fit_bs_full, newdata = tibble(age = age_grid)),
    `Smoothing Spline` = predict(fit_smooth_full, x = age_grid)$y
  ) %>%
  pivot_longer(-age, names_to = "Modelo", values_to = "Prediccion")

p5 <- ggplot() +
  geom_point(data = wage_data, aes(x = age, y = wage), 
             alpha = 0.1, size = 0.5, color = "gray50") +
  geom_line(data = pred_final, 
            aes(x = age, y = Prediccion, color = Modelo), 
            size = 1.2) +
  labs(title = "Comparacion Final: Diferentes Metodos de Regresion",
       subtitle = paste0("Smoothing spline usa df = ", best_df, " (seleccionado por CV)"),
       x = "Edad", 
       y = "Salario",
       color = "Modelo") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p5)
