pacman::p_load(
  tidymodels,
  ranger,
  doParallel,
  vip,
  plotly,
  MLmetrics
)

telco <- read_csv("data/Churn.csv")
glimpse(telco)

# Paso 1: Separación inicial de datos ( test, train <KFCV> )

set.seed(1234)
telco_split <- initial_split(telco, prop = .70)

telco_train <- training(telco_split)
telco_test  <- testing(telco_split)
telco_folds <- vfold_cv(telco_train)

telco_folds

# Paso 2: Pre-procesamiento e ingeniería de variables

telco_rec <- recipe(
  Churn ~ customerID + TotalCharges + MonthlyCharges + SeniorCitizen + Contract,
  data = telco_train) %>%
  update_role(customerID, new_role = "id variable") %>%
  step_mutate(Contract = as.factor(Contract)) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  prep()

telco_rec

# Paso 3: Selección de tipo de modelo con hiperparámetros iniciales

rforest_model <- rand_forest(
  mode = "classification",
  trees = 1000,
  mtry = tune(),
  min_n = tune()) %>%
  set_engine("ranger", importance = "impurity")

rforest_model

# Paso 4: Inicialización de workflow o pipeline

rforest_workflow <- workflow() %>%
  add_recipe(telco_rec) %>%
  add_model(rforest_model)

rforest_workflow

# Paso 5: Creación de grid search

set.seed(195628)
rforest_param_grid <- grid_random(
  mtry(range = c(2,5)),
  min_n(range = c(2,16)),
  size = 20
)

ctrl_grid <- control_grid(save_pred = T, verbose = T)

# Paso 6: Entrenamiento de modelos con hiperparámetros definidos

# UseCores <- detectCores() - 1
# cluster <- makeCluster(UseCores)
# registerDoParallel(cluster)
#
# rft1 <- Sys.time()
# rf_tune_result <- tune_grid(
#   rforest_workflow,
#   resamples = telco_folds,
#   grid = rforest_param_grid,
#   metrics = metric_set(roc_auc, pr_auc),
#   control = ctrl_grid
# )
# rft2 <- Sys.time(); rft2 - rft1
#
# stopCluster(cluster)
#
# rf_tune_result %>% saveRDS("models/rforest_model_cla.rds")

rf_tune_result <- readRDS("models/rforest_model_cla.rds")

# Paso 7: Análisis de métricas de error e hiperparámetros (Vuelve al paso 3, si es necesario)

collect_metrics(rf_tune_result)

autoplot(rf_tune_result, metric = "pr_auc")

autoplot(rf_tune_result, metric = "roc_auc")

show_best(rf_tune_result, n = 10, metric = "pr_auc")

# Paso 8: Selección de modelo a usar

best_rf_model_cla <- select_best(rf_tune_result, metric = "pr_auc")
best_rf_model_cla

rf_classification_best_1se_model <- rf_tune_result %>%
  select_by_one_std_err(metric = "roc_auc", "roc_auc")
rf_classification_best_1se_model


# Paso 9: Ajuste de modelo final con todos los datos (Vuelve al paso 2, si es necesario)

final_rf_model_cla <- rforest_workflow %>%
  finalize_workflow(best_rf_model_cla) %>%
  parsnip::fit(data = telco_train)


final_rf_model_cla %>%
  extract_fit_parsnip() %>%
  vip::vip() +
  ggtitle("Importancia de las variables")+
  theme_minimal()


# Paso 10: Validar poder predictivo con datos de prueba

results_cla <- predict(final_rf_model_cla, telco_test, type = 'prob') %>%
  dplyr::bind_cols(Churn = telco_test$Churn, .) %>%
  mutate(Churn = factor(Churn, levels = c('Yes', 'No'), labels = c('Yes', 'No')))

results_cla

bind_rows(
  roc_auc(results_cla, truth = Churn, estimate = .pred_Yes),
  pr_auc(results_cla, truth = Churn, estimate = .pred_Yes)
)

pr_curve_data <- pr_curve(
  results_cla,
  truth = Churn,
  estimate = .pred_Yes
  )
pr_curve_data

roc_curve_data <- roc_curve(
  results_cla,
  truth = Churn,
  estimate = .pred_Yes
  )
roc_curve_data

pr_curve_plot <- pr_curve_data %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_abline(slope = -1, intercept = 1) +
  geom_path(size = 1, colour = 'lightblue') +
  ylim(0, 1) +
  coord_equal() +
  ggtitle("Precision vs Recall")+
  theme_minimal()

pr_curve_plot

roc_curve_plot <- roc_curve_data %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(size = 1, colour = 'lightblue') +
  geom_abline() +
  coord_equal() +
  ggtitle("ROC Curve")+
  theme_minimal()

roc_curve_plot
