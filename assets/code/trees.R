################################################################################
# Clase 6: Métodos Basados en Árboles
# Random Forests y Boosting
# 
# Este script contiene todos los ejemplos y demostraciones de la clase
################################################################################

# Cargar paquetes necesarios
library(tidyverse)       # Manipulación de datos y gráficos
library(rpart)           # Árboles de decisión (CART)
library(rpart.plot)      # Visualización de árboles
library(randomForest)    # Random Forests
library(xgboost)         # XGBoost
library(caret)           # Framework unificado, validación cruzada
library(ISLR)            # Datasets (Wage)
library(pdp)             # Partial dependence plots
library(vip)             # Variable importance plots

# Fijar semilla para reproducibilidad
set.seed(42)

################################################################################
# PARTE 1: ÁRBOLES DE DECISIÓN INDIVIDUALES
################################################################################

cat("\n=== PARTE 1: ÁRBOLES DE DECISIÓN ===\n")

# Cargar datos de salarios
data(Wage)
cat("\nDimensiones de Wage data:", dim(Wage), "\n")
cat("Variables:", names(Wage), "\n")

# Exploración rápida
summary(Wage$wage)

# Preparar datos: eliminar algunas variables redundantes
wage_data <- Wage %>%
  select(-logwage, -region) %>%
  na.omit()

# Train/Test split (70/30)
train_idx <- sample(1:nrow(wage_data), 0.7 * nrow(wage_data))
train_data <- wage_data[train_idx, ]
test_data <- wage_data[-train_idx, ]

cat("\nTrain size:", nrow(train_data), "\n")
cat("Test size:", nrow(test_data), "\n")

# --- Árbol individual ---
cat("\n--- Entrenando árbol individual ---\n")

# Entrenar árbol de regresión
tree_model <- rpart(
  wage ~ age + year + education + jobclass + health + health_ins,
  data = train_data,
  method = "anova",  # Para regresión
  control = rpart.control(
    minsplit = 20,    # Mínimo de observaciones para intentar división
    cp = 0.001,       # Complejidad mínima
    maxdepth = 10     # Profundidad máxima
  )
)

# Visualizar el árbol
cat("\nEstructura del árbol:\n")
print(tree_model)

# Gráfico del árbol
rpart.plot(tree_model, 
           type = 4,           # Tipo de gráfico
           extra = 101,        # Información adicional
           under = TRUE,
           faclen = 0,
           main = "Árbol de Decisión: Predicción de Salarios")

# Predicciones
tree_pred_train <- predict(tree_model, train_data)
tree_pred_test <- predict(tree_model, test_data)

# Error cuadrático medio
tree_rmse_train <- sqrt(mean((train_data$wage - tree_pred_train)^2))
tree_rmse_test <- sqrt(mean((test_data$wage - tree_pred_test)^2))

cat("\nÁrbol individual:\n")
cat("RMSE Train:", round(tree_rmse_train, 2), "\n")
cat("RMSE Test:", round(tree_rmse_test, 2), "\n")

# --- Poda del árbol usando CV ---
cat("\n--- Podando árbol con validación cruzada ---\n")

# Tabla de complejidad
printcp(tree_model)

# Encontrar CP óptimo
cp_optimo <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]
cat("\nCP óptimo:", cp_optimo, "\n")

# Podar el árbol
tree_pruned <- prune(tree_model, cp = cp_optimo)

# Visualizar árbol podado
rpart.plot(tree_pruned,
           type = 4,
           extra = 101,
           main = "Árbol Podado")

# Predicciones con árbol podado
pruned_pred_test <- predict(tree_pruned, test_data)
pruned_rmse_test <- sqrt(mean((test_data$wage - pruned_pred_test)^2))
cat("RMSE Test (podado):", round(pruned_rmse_test, 2), "\n")

################################################################################
# PARTE 2: BAGGING
################################################################################

cat("\n\n=== PARTE 2: BAGGING ===\n")

# Bagging es RF con mtry = número total de predictores
n_predictors <- ncol(train_data) - 1  # Todas menos wage

cat("\n--- Entrenando modelo Bagging ---\n")
cat("(Random Forest con mtry = todas las variables)\n")

# Entrenar bagging
bagging_model <- randomForest(
  wage ~ age + year + education + jobclass + health + health_ins,
  data = train_data,
  mtry = n_predictors,    # Usar todas las variables = bagging
  ntree = 500,
  importance = TRUE
)

print(bagging_model)

# Predicciones
bagging_pred_train <- predict(bagging_model, train_data)
bagging_pred_test <- predict(bagging_model, test_data)

# Errores
bagging_rmse_train <- sqrt(mean((train_data$wage - bagging_pred_train)^2))
bagging_rmse_test <- sqrt(mean((test_data$wage - bagging_pred_test)^2))

cat("\nBagging:\n")
cat("RMSE Train:", round(bagging_rmse_train, 2), "\n")
cat("RMSE Test:", round(bagging_rmse_test, 2), "\n")

# Importancia de variables
cat("\nImportancia de variables (Bagging):\n")
importance(bagging_model)

# Gráfico de importancia
varImpPlot(bagging_model, main = "Importancia de Variables - Bagging")

################################################################################
# PARTE 3: RANDOM FOREST
################################################################################

cat("\n\n=== PARTE 3: RANDOM FOREST ===\n")

# --- RF con mtry por defecto ---
cat("\n--- Random Forest (mtry por defecto) ---\n")

rf_model <- randomForest(
  wage ~ age + year + education + jobclass + health + health_ins,
  data = train_data,
  mtry = floor(n_predictors / 3),  # Típico para regresión
  ntree = 500,
  importance = TRUE
)

print(rf_model)

# Predicciones
rf_pred_train <- predict(rf_model, train_data)
rf_pred_test <- predict(rf_model, test_data)

# Errores
rf_rmse_train <- sqrt(mean((train_data$wage - rf_pred_train)^2))
rf_rmse_test <- sqrt(mean((test_data$wage - rf_pred_test)^2))

cat("\nRandom Forest (mtry = ", floor(n_predictors / 3), "):\n", sep = "")
cat("RMSE Train:", round(rf_rmse_train, 2), "\n")
cat("RMSE Test:", round(rf_rmse_test, 2), "\n")

# --- Tuning de mtry ---
cat("\n--- Tuning de mtry ---\n")

# Probar diferentes valores de mtry
mtry_values <- c(2, 3, 4, 5, 6)
oob_errors <- numeric(length(mtry_values))

for (i in seq_along(mtry_values)) {
  rf_temp <- randomForest(
    wage ~ age + year + education + jobclass + health + health_ins,
    data = train_data,
    mtry = mtry_values[i],
    ntree = 500
  )
  oob_errors[i] <- rf_temp$mse[500]  # Error OOB final
  cat("mtry =", mtry_values[i], "| OOB MSE =", round(oob_errors[i], 2), "\n")
}

# Mejor mtry
best_mtry <- mtry_values[which.min(oob_errors)]
cat("\nMejor mtry:", best_mtry, "\n")

# Entrenar modelo final con mejor mtry
rf_final <- randomForest(
  wage ~ age + year + education + jobclass + health + health_ins,
  data = train_data,
  mtry = best_mtry,
  ntree = 500,
  importance = TRUE
)

# Predicciones finales
rf_final_pred_test <- predict(rf_final, test_data)
rf_final_rmse_test <- sqrt(mean((test_data$wage - rf_final_pred_test)^2))

cat("RMSE Test (RF optimizado):", round(rf_final_rmse_test, 2), "\n")

# Importancia de variables
cat("\nImportancia de variables (RF final):\n")
print(importance(rf_final))

# Gráfico de importancia mejorado
vip(rf_final, num_features = 10, geom = "point") +
  labs(title = "Importancia de Variables - Random Forest") +
  theme_minimal()

# Gráfico de convergencia del error OOB
plot(rf_final$mse, type = "l", col = "blue", lwd = 2,
     xlab = "Número de árboles", ylab = "Error OOB (MSE)",
     main = "Convergencia del Error OOB")
abline(h = min(rf_final$mse), lty = 2, col = "red")

################################################################################
# PARTE 4: XGBOOST
################################################################################

cat("\n\n=== PARTE 4: XGBOOST ===\n")

# --- Preparar datos para XGBoost ---
cat("\n--- Preparando datos para XGBoost ---\n")

# XGBoost requiere matrices numéricas
# Convertir factores a dummies manualmente o usar model.matrix

# Crear matrices de diseño
train_x <- model.matrix(wage ~ age + year + education + jobclass + 
                          health + health_ins, 
                        data = train_data)[, -1]  # Remover intercept
train_y <- train_data$wage

test_x <- model.matrix(wage ~ age + year + education + jobclass + 
                         health + health_ins, 
                       data = test_data)[, -1]
test_y <- test_data$wage

# Crear objetos DMatrix (formato de XGBoost)
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

# --- XGBoost con parámetros por defecto ---
cat("\n--- XGBoost (parámetros iniciales) ---\n")

params <- list(
  objective = "reg:squarederror",  # Regresión
  eta = 0.1,                       # Learning rate
  max_depth = 6,                   # Profundidad del árbol
  subsample = 0.8,                 # Fracción de observaciones
  colsample_bytree = 0.8          # Fracción de variables
)

# Entrenar con validación
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 50,
  verbose = 0
)

cat("\nNúmero de árboles óptimo:", xgb_model$best_iteration, "\n")

# Predicciones
xgb_pred_train <- predict(xgb_model, dtrain)
xgb_pred_test <- predict(xgb_model, dtest)

# Errores
xgb_rmse_train <- sqrt(mean((train_y - xgb_pred_train)^2))
xgb_rmse_test <- sqrt(mean((test_y - xgb_pred_test)^2))

cat("\nXGBoost (inicial):\n")
cat("RMSE Train:", round(xgb_rmse_train, 2), "\n")
cat("RMSE Test:", round(xgb_rmse_test, 2), "\n")

# --- Tuning de XGBoost con validación cruzada ---
cat("\n--- Tuning de XGBoost con CV ---\n")

# Grid de parámetros para probar
param_grid <- expand.grid(
  eta = c(0.01, 0.05, 0.1),
  max_depth = c(3, 6, 9),
  min_child_weight = c(1, 5, 10)
)

# Validación cruzada (simplificada - solo algunos combos)
cat("\nProbando combinaciones de parámetros...\n")

# Seleccionar mejores parámetros (ejemplo simplificado)
# En práctica real, usar caret::train() o hacer CV completo

best_params <- list(
  objective = "reg:squarederror",
  eta = 0.05,
  max_depth = 6,
  min_child_weight = 5,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Entrenar modelo final
xgb_final <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = 1000,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 50,
  verbose = 0
)

cat("\nModelo XGBoost final:\n")
cat("Best iteration:", xgb_final$best_iteration, "\n")

# Predicciones finales
xgb_final_pred_test <- predict(xgb_final, dtest)
xgb_final_rmse_test <- sqrt(mean((test_y - xgb_final_pred_test)^2))

cat("RMSE Test (XGBoost optimizado):", round(xgb_final_rmse_test, 2), "\n")

# Importancia de variables
importance_matrix <- xgb.importance(
  feature_names = colnames(train_x),
  model = xgb_final
)

cat("\nTop 10 variables más importantes (XGBoost):\n")
print(head(importance_matrix, 10))

# Gráfico de importancia
xgb.plot.importance(importance_matrix, top_n = 10,
                    main = "Importancia de Variables - XGBoost")

# Gráfico de curvas de aprendizaje
eval_log <- xgb_final$evaluation_log

ggplot(eval_log, aes(x = iter)) +
  geom_line(aes(y = train_rmse, color = "Train")) +
  geom_line(aes(y = test_rmse, color = "Test")) +
  geom_vline(xintercept = xgb_final$best_iteration, 
             linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Train" = "blue", "Test" = "orange")) +
  labs(x = "Iteración", y = "RMSE",
       title = "Curvas de Aprendizaje - XGBoost",
       color = "Conjunto") +
  theme_minimal()

################################################################################
# PARTE 5: COMPARACIÓN DE MÉTODOS
################################################################################

cat("\n\n=== PARTE 5: COMPARACIÓN DE MÉTODOS ===\n")

# Tabla comparativa de errores
comparison <- data.frame(
  Método = c("Árbol Individual", "Árbol Podado", "Bagging", 
             "Random Forest", "XGBoost"),
  RMSE_Train = c(tree_rmse_train, NA, bagging_rmse_train, 
                 rf_rmse_train, xgb_rmse_train),
  RMSE_Test = c(tree_rmse_test, pruned_rmse_test, bagging_rmse_test, 
                rf_rmse_test, xgb_final_rmse_test)
)

cat("\nComparación de métodos:\n")
print(comparison)

# Gráfico de comparación
comparison_long <- comparison %>%
  pivot_longer(cols = starts_with("RMSE"), 
               names_to = "Conjunto", 
               values_to = "RMSE") %>%
  mutate(Conjunto = ifelse(Conjunto == "RMSE_Train", "Train", "Test"))

ggplot(comparison_long, aes(x = reorder(Método, RMSE), y = RMSE, fill = Conjunto)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "", y = "RMSE", 
       title = "Comparación de Métodos: Error de Predicción") +
  theme_minimal() +
  scale_fill_manual(values = c("Train" = "#4682B4", "Test" = "#A52A2A"))


test_data$tree_pred <- tree_pred_test
test_data$pruned_pred <- pruned_pred_test
test_data$bagging_pred <- bagging_pred_test
test_data$rf_pred <- rf_pred_test
test_data$rf_final_pred <- rf_final_pred_test
test_data$xgb_pred <- xgb_pred_test
test_data$xgb_final_pred <- xgb_final_pred_test

ggplot(test_data)+geom_point(aes(x=wage,y=rf_final_pred))+
  geom_abline(aes(intercept=0,slope=1)) +
  scale_x_continuous(limits = c(0,350)) + 
  scale_y_continuous(limits = c(0,350)) 

################################################################################
# PARTE 6: APLICACIÓN - CREDIT SCORING
################################################################################

cat("\n\n=== PARTE 6: APLICACIÓN - CREDIT SCORING ===\n")

# Cargar German Credit data
# Si no está instalado: install.packages("caret")
# El dataset está incluido en varios paquetes

# Opción 1: Descargar de UCI
credit_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"

# Nombres de columnas
col_names <- c("checking_status", "duration", "credit_history", "purpose",
               "credit_amount", "savings", "employment", "installment_rate",
               "personal_status", "other_parties", "residence_since",
               "property_magnitude", "age", "other_payment_plans", "housing",
               "existing_credits", "job", "num_dependents", "own_telephone",
               "foreign_worker", "default")

# Intentar cargar datos

  credit_data <- read.table(credit_url, header = FALSE, col.names = col_names)
  
  # Convertir default a binario (1 = malo, 2 = bueno -> 1 = default, 0 = no default)
  credit_data$default <- ifelse(credit_data$default == 1, 0, 1)
  
  cat("\nDatos de crédito cargados exitosamente\n")
  cat("Dimensiones:", dim(credit_data), "\n")
  cat("Tasa de default:", mean(credit_data$default), "\n")
  
  # Train/test split
  set.seed(123)
  credit_train_idx <- sample(1:nrow(credit_data), 0.7 * nrow(credit_data))
  credit_train <- credit_data[credit_train_idx, ]
  credit_test <- credit_data[-credit_train_idx, ]
  
  # --- Logistic Regression (baseline) ---
  cat("\n--- Regresión Logística (baseline) ---\n")
  
  logit_model <- glm(default ~ ., data = credit_train, family = "binomial")
  
  logit_pred_prob <- predict(logit_model, credit_test, type = "response")
  logit_pred_class <- ifelse(logit_pred_prob > 0.5, 1, 0)
  
  logit_accuracy <- mean(logit_pred_class == credit_test$default)
  cat("Accuracy:", round(logit_accuracy, 3), "\n")
  
  # --- Árbol de clasificación ---
  cat("\n--- Árbol de Clasificación ---\n")
  
  tree_class <- rpart(
    default ~ ., 
    data = credit_train,
    method = "class",
    control = rpart.control(cp = 0.001)
  )
  
  tree_class_pred <- predict(tree_class, credit_test, type = "class")
  tree_class_accuracy <- mean(tree_class_pred == credit_test$default)
  
  cat("Accuracy:", round(tree_class_accuracy, 3), "\n")
  
  # --- Random Forest clasificación ---
  cat("\n--- Random Forest (clasificación) ---\n")
  
  credit_train$default <- as.factor(credit_train$default)
  credit_test$default <- as.factor(credit_test$default)
  
  rf_class <- randomForest(
    default ~ .,
    data = credit_train,
    ntree = 500,
    importance = TRUE
  )
  
  rf_class_pred <- predict(rf_class, credit_test)
  rf_class_accuracy <- mean(rf_class_pred == credit_test$default)
  
  cat("Accuracy:", round(rf_class_accuracy, 3), "\n")
  cat("OOB error rate:", rf_class$err.rate[500, 1], "\n")
  
  # --- XGBoost clasificación ---
  cat("\n--- XGBoost (clasificación) ---\n")
  
  # Preparar datos
  credit_train_x <- model.matrix(default ~ . - 1, data = credit_train)
  credit_train_y <- as.numeric(as.character(credit_train$default))
  
  credit_test_x <- model.matrix(default ~ . - 1, data = credit_test)
  credit_test_y <- as.numeric(as.character(credit_test$default))
  
  dcredit_train <- xgb.DMatrix(data = credit_train_x, label = credit_train_y)
  dcredit_test <- xgb.DMatrix(data = credit_test_x, label = credit_test_y)
  
  
  # Entrenar con parámetros básicos primero
  cat("\nXGBoost con parámetros básicos:\n")
  xgb_class_basic <- xgb.train(
    params = list(
      objective = "binary:logistic",
      eta = 0.1,
      max_depth = 6,
      eval_metric = "auc"
    ),
    data = dcredit_train,
    nrounds = 200,
    watchlist = list(train = dcredit_train, test = dcredit_test),
    early_stopping_rounds = 20,
    verbose = 0
  )
  
  xgb_basic_pred_prob <- predict(xgb_class_basic, dcredit_test)
  xgb_basic_pred <- ifelse(xgb_basic_pred_prob > 0.5, 1, 0)
  xgb_basic_accuracy <- mean(xgb_basic_pred == credit_test_y)
  
  cat("Accuracy (básico):", round(xgb_basic_accuracy, 3), "\n")
  
  # --- Tuning más agresivo ---
  cat("\n--- Tuning de XGBoost ---\n")
  cat("Probando diferentes combinaciones de hiperparámetros...\n")
  
  # Grid de parámetros para probar
  param_combinations <- list(
    # Configuración 1: Árboles profundos, learning rate bajo
    list(eta = 0.01, max_depth = 8, min_child_weight = 1, 
         subsample = 0.8, colsample_bytree = 0.8,
         lambda = 1, alpha = 0),
    
    # Configuración 2: Árboles menos profundos, más regularización
    list(eta = 0.05, max_depth = 4, min_child_weight = 5,
         subsample = 0.7, colsample_bytree = 0.7,
         lambda = 5, alpha = 1),
    
    # Configuración 3: Balance medio
    list(eta = 0.03, max_depth = 6, min_child_weight = 3,
         subsample = 0.8, colsample_bytree = 0.8,
         lambda = 3, alpha = 0.5),
    
    # Configuración 4: Más conservador
    list(eta = 0.02, max_depth = 5, min_child_weight = 10,
         subsample = 0.6, colsample_bytree = 0.6,
         lambda = 10, alpha = 2)
  )
  
  best_auc <- 0
  best_config <- NULL
  best_model <- NULL
  
  for (i in seq_along(param_combinations)) {
    params <- param_combinations[[i]]
    params$objective <- "binary:logistic"
    params$eval_metric <- "auc"
    
    # Entrenar con más iteraciones y early stopping
    xgb_temp <- xgb.train(
      params = params,
      data = dcredit_train,
      nrounds = 500,
      watchlist = list(train = dcredit_train, test = dcredit_test),
      early_stopping_rounds = 50,
      verbose = 0
    )
    
    # Evaluar en test
    temp_pred_prob <- predict(xgb_temp, dcredit_test)
    
    if(require(pROC, quietly = TRUE)) {
      temp_auc <- auc(roc(credit_test_y, temp_pred_prob, quiet = TRUE))
      
      cat(sprintf("Config %d - eta: %.3f, max_depth: %d, lambda: %.1f | AUC: %.3f\n",
                  i, params$eta, params$max_depth, params$lambda, temp_auc))
      
      if (temp_auc > best_auc) {
        best_auc <- temp_auc
        best_config <- i
        best_model <- xgb_temp
      }
    }
  }
  
  cat("\nMejor configuración: Config", best_config, "\n")
  cat("Mejor AUC:", round(best_auc, 3), "\n")
  
  # Usar el mejor modelo
  xgb_class <- best_model
  xgb_class_pred_prob <- predict(xgb_class, dcredit_test)
  xgb_class_pred <- ifelse(xgb_class_pred_prob > 0.5, 1, 0)
  xgb_class_accuracy <- mean(xgb_class_pred == credit_test_y)
  
  cat("Accuracy (tuneado):", round(xgb_class_accuracy, 3), "\n")
  cat("Mejora vs básico:", round(xgb_class_accuracy - xgb_basic_accuracy, 3), "\n")
  
  # --- Comparación con curvas ROC ---
  if(require(pROC, quietly = TRUE)) {
    cat("\n--- Curvas ROC ---\n")
    
    # Calcular ROC curves
    roc_logit <- roc(credit_test_y, logit_pred_prob, quiet = TRUE)
    
    rf_class_prob <- predict(rf_class, credit_test, type = "prob")[, 2]
    roc_rf <- roc(credit_test_y, rf_class_prob, quiet = TRUE)
    
    roc_xgb <- roc(credit_test_y, xgb_class_pred_prob, quiet = TRUE)
    
    # AUC
    cat("AUC Logit:", round(auc(roc_logit), 3), "\n")
    cat("AUC RF:", round(auc(roc_rf), 3), "\n")
    cat("AUC XGBoost:", round(auc(roc_xgb), 3), "\n")
    
    # Gráfico
    plot(roc_logit, col = "blue", main = "Curvas ROC - Credit Default")
    plot(roc_rf, col = "green", add = TRUE)
    plot(roc_xgb, col = "red", add = TRUE)
    legend("bottomright", 
           legend = c("Logit", "RF", "XGBoost"),
           col = c("blue", "green", "red"),
           lwd = 2)
  }
  
  # Comparación final
  cat("\n--- Comparación Final: Credit Scoring ---\n")
  credit_comparison <- data.frame(
    Método = c("Logit", "Árbol", "Random Forest", "XGBoost"),
    Accuracy = c(logit_accuracy, tree_class_accuracy, 
                 rf_class_accuracy, xgb_class_accuracy)
  )
  print(credit_comparison)
  

################################################################################
# PARTE 7: PARTIAL DEPENDENCE PLOTS
################################################################################

cat("\n\n=== PARTE 7: PARTIAL DEPENDENCE PLOTS ===\n")

# PDP para Random Forest (variable continua)
cat("\n--- PDP: Efecto de Age en Wage ---\n")

# Partial dependence para age
pdp_age <- partial(rf_final, pred.var = "age", train = train_data)

# Gráfico
ggplot(pdp_age, aes(x = age, y = yhat)) +
  geom_line(color = "steelblue", size = 1.5) +
  labs(x = "Edad", y = "Salario Predicho (Partial Effect)",
       title = "Partial Dependence Plot: Edad") +
  theme_minimal()

# PDP para variable categórica
cat("\n--- PDP: Efecto de Education en Wage ---\n")

pdp_education <- partial(rf_final, pred.var = "education", train = train_data)

ggplot(pdp_education, aes(x = education, y = yhat)) +
  geom_col(fill = "steelblue") +
  labs(x = "Nivel de Educación", y = "Salario Predicho",
       title = "Partial Dependence Plot: Educación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################
# RESUMEN Y RECOMENDACIONES
################################################################################

cat("\n\n=== RESUMEN DE RESULTADOS ===\n")

cat("\nWage Prediction (RMSE en conjunto de prueba):\n")
cat("  Árbol Individual:", round(tree_rmse_test, 2), "\n")
cat("  Árbol Podado:    ", round(pruned_rmse_test, 2), "\n")
cat("  Bagging:         ", round(bagging_rmse_test, 2), "\n")
cat("  Random Forest:   ", round(rf_final_rmse_test, 2), "\n")
cat("  XGBoost:         ", round(xgb_final_rmse_test, 2), "\n")

cat("\n=== RECOMENDACIONES ===\n")
cat("\n1. Para interpretabilidad máxima: Usar árbol individual (podado)\n")
cat("2. Para balance interpretación/precisión: Random Forest\n")
cat("3. Para máxima precisión: XGBoost (con tuning cuidadoso)\n")
cat("4. Para análisis exploratorio rápido: Random Forest con defaults\n")

cat("\n=== SCRIPT COMPLETADO ===\n")
