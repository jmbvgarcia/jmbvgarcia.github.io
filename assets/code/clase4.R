
# Cargar librerías necesarias
library(tidyverse)
library(glmnet)
# ============================================
# PARTE 1: PREPARACIÓN DE DATOS
# ============================================

  # Cargar datos
  data("Males", package = "plm")
  
  # Explorar estructura
  cat(paste(names(Males), collapse = ", "), "\n\n")
  
  # Preparar datos
  males <- Males %>%
    mutate(
      # Variables numéricas
      exper2 = exper^2,
      exper3 = exper^3,
      exper4 = exper^4,
      exper5 = exper^5,
      school2 = school^2,
      school3 = school^3,
      school4 = school^4,
      # === INTERACCIONES: experiencia con educación ===
      exper_x_school = exper * school,
      exper2_x_school = exper^2 * school,
      exper_x_school2 = exper * school^2,
      
      # Variables categóricas como factores
      occupation = as.factor(occupation),
      industry = as.factor(industry),
      union = as.factor(union),
      ethn = as.factor(ethn),
      married = as.factor(married),
      health = as.factor(health),
      
      # === INTERACCIONES: experiencia con categorías ===
      exper_x_union = exper * as.numeric(union),
      exper_x_married = exper * as.numeric(married),
      
      # === INTERACCIONES: educación con categorías ===
      school_x_union = school * as.numeric(union),
      school_x_married = school * as.numeric(married)
    ) |> filter(year==1980)
  
# ============================================
# PARTE 2: CREAR MATRIZ DE DISEÑO CON MUCHAS VARIABLES
# ===========================================
  
  X_base <- model.matrix(wage ~ . - nr - residence, 
                         data = males)[, -1]
  
  cat("Variables después de dummies base:", ncol(X_base), "\n")
  exper_var <- X_base[, grep("^exper", colnames(X_base)), drop = FALSE][,"exper"]
  school_var <- X_base[, grep("^school", colnames(X_base)), drop = FALSE][,"school"]
  # === AGREGAR INTERACCIONES ENTRE CATEGORÍAS ===
  
  # Extraer dummies
  occ_dummies <- X_base[, grep("^occupation", colnames(X_base)), drop = FALSE]
  ind_dummies <- X_base[, grep("^industry", colnames(X_base)), drop = FALSE]
  union_dummy <- X_base[, grep("^union", colnames(X_base)), drop = FALSE]
  ethn_dummies <- X_base[, grep("^ethn", colnames(X_base)), drop = FALSE]
  married_dummy <- X_base[, grep("^married", colnames(X_base)), drop = FALSE]
  
    # === INTERACCIONES CRÍTICAS: EXPERIENCIA × OCCUPATION ===
    if(ncol(occ_dummies) > 0) {
        interact_exper_occ <- exper_var * occ_dummies
        colnames(interact_exper_occ) <- paste0("exper_x_", colnames(occ_dummies))
        cat("  Experiencia × Occupation:", ncol(interact_exper_occ), "\n")
      } else {
          interact_exper_occ <- NULL
        }
    
    # === INTERACCIONES CRÍTICAS: EXPERIENCIA × INDUSTRY ===
    if(ncol(ind_dummies) > 0) {
        interact_exper_ind <- exper_var * ind_dummies
        colnames(interact_exper_ind) <- paste0("exper_x_", colnames(ind_dummies))
        cat("  Experiencia × Industry:", ncol(interact_exper_ind), "\n")
      } else {
          interact_exper_ind <- NULL
        }
    
    # === INTERACCIONES CRÍTICAS: EDUCACIÓN × OCCUPATION ===
    if(ncol(occ_dummies) > 0) {
        interact_school_occ <- school_var * occ_dummies
        colnames(interact_school_occ) <- paste0("school_x_", colnames(occ_dummies))
        cat("  Educación × Occupation:", ncol(interact_school_occ), "\n")
      } else {
          interact_school_occ <- NULL
        }
    
    # === INTERACCIONES CRÍTICAS: EDUCACIÓN × INDUSTRY ===
    if(ncol(ind_dummies) > 0) {
        interact_school_ind <- school_var * ind_dummies
        colnames(interact_school_ind) <- paste0("school_x_", colnames(ind_dummies))
        cat("  Educación × Industry:", ncol(interact_school_ind), "\n")
      } else {
          interact_school_ind <- NULL
        }
  # Interacciones: Occupation × Industry
  if(ncol(occ_dummies) > 0 && ncol(ind_dummies) > 0) {
    interact_occ_ind <- model.matrix(~ occ_dummies:ind_dummies - 1)
    colnames(interact_occ_ind) <- paste0("occ_x_ind_", 1:ncol(interact_occ_ind))
    cat("  Occupation × Industry interacciones:", ncol(interact_occ_ind), "\n")
  } else {
    interact_occ_ind <- NULL
  }
  
  # Interacciones: Occupation × Union
  if(ncol(occ_dummies) > 0 && ncol(union_dummy) > 0) {
    interact_occ_union <- model.matrix(~ occ_dummies:union_dummy - 1)
    colnames(interact_occ_union) <- paste0("occ_x_union_", 1:ncol(interact_occ_union))
    cat("  Occupation × Union interacciones:", ncol(interact_occ_union), "\n")
  } else {
    interact_occ_union <- NULL
  }
  
  # Interacciones: Industry × Married
  if(ncol(ind_dummies) > 0 && ncol(married_dummy) > 0) {
    interact_ind_married <- model.matrix(~ ind_dummies:married_dummy - 1)
    colnames(interact_ind_married) <- paste0("ind_x_married_", 1:ncol(interact_ind_married))
    cat("  Industry × Married interacciones:", ncol(interact_ind_married), "\n")
  } else {
    interact_ind_married <- NULL
  }
  
  # Interacciones: Occupation × Ethnicity
  if(ncol(occ_dummies) > 0 && ncol(ethn_dummies) > 0) {
    interact_occ_ethn <- model.matrix(~ occ_dummies:ethn_dummies - 1)
    colnames(interact_occ_ethn) <- paste0("occ_x_ethn_", 1:ncol(interact_occ_ethn))
    cat("  Occupation × Ethnicity interacciones:", ncol(interact_occ_ethn), "\n")
  } else {
    interact_occ_ethn <- NULL
  }
  
  # Combinar todo
  X <- cbind(
    X_base,
    interact_exper_occ,
    interact_exper_ind,
    interact_school_occ,
    interact_school_ind,
    interact_occ_ind,
    interact_occ_union,
    interact_ind_married,
    interact_occ_ethn
  )
  y <- males$wage

# ============================================
# PARTE 3: DIVISIÓN TRAIN/TEST
# ============================================

set.seed(2024)
train_idx <- sample(1:nrow(X), size = 0.7 * nrow(X))
test_idx <- setdiff(1:nrow(X), train_idx)

X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[test_idx, ]
y_test <- y[test_idx]

# ============================================
# PARTE 4: MODELO BASE - OLS
# ============================================
# Convertir matrices a data frames con nombres de columnas apropiados
train_data <- as.data.frame(X_train)
train_data$y <- y_train

test_data <- as.data.frame(X_test)
# Asegurar que los nombres de columnas coincidan
colnames(test_data) <- colnames(train_data)[1:(ncol(train_data)-1)]

# Nota: Con p grande, OLS puede tener problemas de multicolinealidad
ols_model <- lm(y ~ ., data = train_data)

# Predicciones
y_train_pred_ols <- predict(ols_model, newdata = train_data)
y_test_pred_ols <- predict(ols_model, newdata = test_data)

# Errores
rmse_train_ols <- sqrt(mean((y_train - y_train_pred_ols)^2))
rmse_test_ols <- sqrt(mean((y_test - y_test_pred_ols)^2))

cat("RMSE Train:", round(rmse_train_ols, 4), "\n")
cat("RMSE Test:", round(rmse_test_ols, 4), "\n")
cat("Número de coeficientes:", length(coef(ols_model)) - 1, "\n")

# ¿Cuántos coeficientes son significativos?
ols_summary <- summary(ols_model)
sig_coefs <- sum(ols_summary$coefficients[-1, 4] < 0.05)
cat("Coeficientes significativos (p < 0.05):", sig_coefs, "\n")

# ============================================
# PARTE 5: RIDGE REGRESSION
# ============================================

# glmnet automáticamente estandariza las variables
# alpha = 0 significa Ridge (L2)
ridge_fit <- glmnet(X_train, y_train, alpha = 0, 
                    lambda = exp(seq(5, -5, length.out = 100)))

# Visualizar camino de coeficientes
plot(ridge_fit, xvar = "lambda", label = TRUE,
     main = "Camino de Coeficientes Ridge")

# Cross-validation para elegir lambda
set.seed(2024)
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 10)

# Visualizar CV
plot(cv_ridge, main = "Cross-Validation Ridge")

# Lambdas óptimos
lambda_min_ridge <- cv_ridge$lambda.min

cat("\nLambda que minimiza CV error:", lambda_min_ridge, "\n")

# Predicciones con lambda óptimo (usamos lambda.1se por parsimonia)
y_train_pred_ridge <- predict(cv_ridge, s = "lambda.min", newx = X_train)
y_test_pred_ridge <- predict(cv_ridge, s = "lambda.min", newx = X_test)

# Errores
rmse_train_ridge <- sqrt(mean((y_train - y_train_pred_ridge)^2))
rmse_test_ridge <- sqrt(mean((y_test - y_test_pred_ridge)^2))

# Coeficientes Ridge
coef_ridge <- coef(cv_ridge, s = "lambda.min")
cat("Coeficientes no-cero:", sum(coef_ridge != 0), "\n")

# ============================================
# PARTE 6: LASSO
# ============================================
# alpha = 1 significa Lasso (L1)
lasso_fit <- glmnet(X_train, y_train, alpha = 1,
                    lambda = exp(seq(5, -5, length.out = 100)))

plot(lasso_fit, xvar = "lambda", label = TRUE,
     main = "Camino de Coeficientes Lasso")

set.seed(2024)
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 10)

# Visualizar CV
plot(cv_lasso, main = "Cross-Validation Lasso")

# Lambdas óptimos
lambda_min_lasso <- cv_lasso$lambda.min

cat("\nLambda que minimiza CV error:", lambda_min_lasso, "\n")

# Predicciones
y_train_pred_lasso_min <- predict(cv_lasso, s = "lambda.min", newx = X_train)
y_test_pred_lasso_min <- predict(cv_lasso, s = "lambda.min", newx = X_test)

# Errores
rmse_train_lasso_min <- sqrt(mean((y_train - y_train_pred_lasso_min)^2))
rmse_test_lasso_min <- sqrt(mean((y_test - y_test_pred_lasso_min)^2))

cat("\nRMSE Train (lambda.min):", round(rmse_train_lasso_min, 4), "\n")
cat("RMSE Test (lambda.min):", round(rmse_test_lasso_min, 4), "\n")

# Coeficientes Lasso
coef_lasso_min <- coef(cv_lasso, s = "lambda.min")

cat("\nCoeficientes no-cero (lambda.min):", sum(coef_lasso_min != 0) - 1, "\n")

# ============================================
# PARTE 6: Elastic Net
# ============================================
# alpha = 1 significa Lasso (L1)
elstnet_fit <- glmnet(X_train, y_train, alpha = 0.5,
                    lambda = exp(seq(5, -5, length.out = 100)))

plot(elstnet_fit, xvar = "lambda", label = TRUE,
     main = "Camino de Coeficientes Lasso")

set.seed(2024)
cv_elstnet <- cv.glmnet(X_train, y_train, alpha = 0.25, nfolds = 10)

# Visualizar CV
plot(cv_elstnet, main = "Cross-Validation Lasso")

# Lambdas óptimos
lambda_min_elstnet <- cv_elstnet$lambda.min

cat("\nLambda que minimiza CV error:", lambda_min_elstnet, "\n")

# Predicciones
y_train_pred_elstnet_min <- predict(cv_elstnet, s = "lambda.min", newx = X_train)
y_test_pred_elstnet_min <- predict(cv_elstnet, s = "lambda.min", newx = X_test)

# Errores
rmse_train_elstnet_min <- sqrt(mean((y_train - y_train_pred_elstnet_min)^2))
rmse_test_elstnet_min <- sqrt(mean((y_test - y_test_pred_elstnet_min)^2))

cat("\nRMSE Train (lambda.min):", round(rmse_train_elstnet_min, 4), "\n")
cat("RMSE Test (lambda.min):", round(rmse_test_elstnet_min, 4), "\n")

# Coeficientes 
coef_elstnet_min <- coef(cv_elstnet, s = "lambda.min")

cat("\nCoeficientes no-cero (lambda.min):", sum(coef_elstnet_min != 0) - 1, "\n")

# ============================================
# PARTE 7: COMPARACIÓN DE MODELOS
# ============================================



comparison <- tibble(
  Modelo = c("OLS", "Ridge", "Lasso", "Elastic Net"),
  RMSE_Train = c(rmse_train_ols, rmse_train_ridge, 
                 rmse_train_lasso_min, rmse_train_elstnet_min),
  RMSE_Test = c(rmse_test_ols, rmse_test_ridge, 
                rmse_test_lasso_min, rmse_test_elstnet_min),
  Num_Variables = c(length(coef(ols_model)) - 1,
                    ncol(X_train),
                    sum(coef_lasso_min != 0) - 1,
                    sum(coef_elstnet_min != 0) - 1)
)

print(comparison)

# Visualización de comparación
comparison_long <- comparison %>%
  pivot_longer(cols = c(RMSE_Train, RMSE_Test),
               names_to = "Set", values_to = "RMSE") %>%
  mutate(Set = ifelse(Set == "RMSE_Train", "Train", "Test"))

ggplot(comparison_long, aes(x = Modelo, y = RMSE, fill = Set)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================
# PARTE 8: ANÁLISIS DE VARIABLES SELECCIONADAS POR LASSO
# ============================================
# Extraer coeficientes no-cero
coef_lasso_min_values <- as.matrix(coef_lasso_min)
selected_vars <- coef_lasso_min_values[coef_lasso_min_values != 0, , drop = FALSE]
selected_vars <- selected_vars[-1, , drop = FALSE]  # Remover intercept

# Ordenar por magnitud absoluta
selected_df <- tibble(
  Variable = rownames(selected_vars),
  Coeficiente = as.vector(selected_vars)
) %>%
  arrange(desc(abs(Coeficiente)))

print(selected_df)


# Para datos reales (Wage), solo mostrar top variables

top_vars <- selected_df %>% slice(1:min(20, nrow(selected_df)))

ggplot(top_vars, aes(x = reorder(Variable, abs(Coeficiente)), 
                     y = Coeficiente)) +
  geom_col(aes(fill = Coeficiente > 0)) +
  coord_flip() + 
  theme_minimal()


# ============================================
# PARTE 9: COMPARACIÓN VISUAL RIDGE VS LASSO
# ============================================

# Extraer coeficientes de ambos modelos para el mismo conjunto de variables
coef_ridge_values <- as.matrix(coef(cv_ridge, s = "lambda.min"))[-1, ]
coef_lasso_values <- as.matrix(coef(cv_lasso, s = "lambda.min"))[-1, ]

# Crear dataframe de comparación
coef_comparison <- tibble(
  Variable = colnames(X_train),
  Ridge = coef_ridge_values,
  Lasso = coef_lasso_values
) %>%
  mutate(Lasso_Selected = abs(Lasso) > 0)

# Comparación para top variables
top_ridge_vars <- coef_comparison %>% 
  slice_max(order_by = abs(Ridge), n = min(20, nrow(.)))

comparison_plot_data <- top_ridge_vars %>%
  pivot_longer(cols = c(Ridge, Lasso),
               names_to = "Método", values_to = "Coeficiente")

ggplot(comparison_plot_data, 
       aes(x = reorder(Variable, abs(Coeficiente)), 
           y = Coeficiente, fill = Método)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("Ridge" = "red", "Lasso" = "darkgreen")) +
  labs(x = "Variable", y = "Coeficiente") +
  theme_minimal()

# ============================================
# PARTE 10: PREDICCIÓN VISUAL
# ============================================

# Crear dataframe con predicciones
predictions_df <- tibble(
  Observado = y_test,
  OLS = y_test_pred_ols,
  Ridge = as.vector(y_test_pred_ridge),
  Lasso = as.vector(y_test_pred_lasso_min)
) %>%
  pivot_longer(cols = -Observado, names_to = "Modelo", values_to = "Predicho")

# Gráfico de dispersión
ggplot(predictions_df, aes(x = Observado, y = Predicho, color = Modelo)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ Modelo) +
  theme_minimal() 

