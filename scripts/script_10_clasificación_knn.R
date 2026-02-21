pacman::p_load(
 readr,
 tidymodels,
 kknn,
 doParallel,
 kernlab,
 vip,
 MLmetrics,
 patchwork
)

# Paso 1: Separación inicial de datos ( test, train <KFCV> )
telco <- read_csv("data/Churn.csv")

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

knn_model <- nearest_neighbor(
  mode = "classification",
  neighbors = tune("K"),
  weight_func = tune()) %>%
  set_engine("kknn")

knn_model


# Paso 4: Inicialización de workflow o pipeline

knn_workflow <- workflow() %>%
  add_recipe(telco_rec) %>%
  add_model(knn_model)

knn_workflow


# Paso 5: Creación de grid search

knn_parameters_set <- extract_parameter_set_dials(knn_workflow) %>%
  update(
    K = dials::neighbors(c(10,80)),
    weight_func = weight_func(values = c("rectangular", "inv", "gaussian", "cos"))
  )

set.seed(123)
knn_grid <- knn_parameters_set %>%
  grid_max_entropy(size = 50)

ctrl_grid <- control_grid(save_pred = T, verbose = T)


# Paso 6: Entrenamiento de modelos con hiperparámetros definidos

# UseCores <- detectCores() - 1
# cluster <- makeCluster(UseCores)
# registerDoParallel(cluster)
#
# knnt1 <- Sys.time()
# knn_tune_result <- tune_grid(
#   knn_workflow,
#   resamples = telco_folds,
#   grid = knn_grid,
#   metrics = metric_set(roc_auc, pr_auc),
#   control = ctrl_grid
# )
# knnt2 <- Sys.time(); knnt2 - knnt1
#
# stopCluster(cluster)
#
# knn_tune_result %>% saveRDS("models/knn_model_cla.rds")


knn_tune_result <- readRDS("models/knn_model_cla.rds")

# Paso 7: Análisis de métricas de error e hiperparámetros (Vuelve al paso 3, si es necesario)

collect_metrics(knn_tune_result)

knn_tune_result %>% autoplot()

autoplot(knn_tune_result, metric = "pr_auc")

autoplot(knn_tune_result, metric = "roc_auc")




# Paso 8: Selección de modelo a usar

knn_tune_result %>% show_best(n = 10, metric = "pr_auc")

knn_tune_result %>% show_best(n = 10, metric = "roc_curve")

best_knn_model_cla <- select_best(knn_tune_result, metric = "pr_auc")
best_knn_model_cla

knn_classification_best_1se_model <- knn_tune_result %>%
  select_by_one_std_err(metric = "roc_auc", "roc_auc")
knn_classification_best_1se_model


# Paso 9: Ajuste de modelo final con todos los datos (Vuelve al paso 2, si es necesario)

final_knn_model_cla <- knn_workflow %>%
  finalize_workflow(best_knn_model_cla) %>%
  parsnip::fit(data = telco_train)


# churn_importance <- final_knn_model_cla %>%
#   extract_fit_parsnip() %>%
#   vi(
#     method = "permute",
#     nsim = 30,
#     target = "Churn",
#     metric = "auc",
#     reference_class = "Yes",
#     pred_wrapper = kernlab::predict,
#     train = juice(telco_rec)
#   )
#
# churn_importance %>% saveRDS("models/vip_telco_knn.rds")

churn_importance <- readRDS("models/vip_telco_knn.rds")
churn_importance

churn_importance %>%
  mutate(Variable = forcats::fct_reorder(Variable, Importance)) %>%
  ggplot(aes(Importance, Variable, color = Variable)) +
  geom_errorbar(aes(xmin = Importance - StDev, xmax = Importance + StDev),
    alpha = 0.5, size = 1) +
  geom_point(size = 2) +
  theme(legend.position = "none") +
  ggtitle("Variable Importance Measure")



# Paso 10: Validar poder predictivo con datos de prueba

results_cla <- predict(final_knn_model_cla, telco_test, type = 'prob') %>%
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
  geom_path(size = 1, colour = 'lightblue') +
  coord_equal() +
  ggtitle("Precision vs Recall")+
  theme_minimal()

roc_curve_plot <- roc_curve_data %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(size = 1, colour = 'lightblue') +
  geom_abline() +
  coord_equal() +
  ggtitle("ROC Curve")+
  theme_minimal()

pr_curve_plot + roc_curve_plot








