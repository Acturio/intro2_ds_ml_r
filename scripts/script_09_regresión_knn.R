pacman::p_load(
 tidymodels,
 kknn,
 doParallel,
 kernlab,
 vip,
 MLmetrics
)

# Paso 1: Separación inicial de datos ( test, train <KFCV> )
data(ames)

set.seed(4595)
ames_split <- initial_split(ames, prop = 0.75)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)
ames_folds <- vfold_cv(ames_train)


# Paso 2: Pre-procesamiento e ingeniería de variables
receta_casas <- recipe(
 Sale_Price ~ Gr_Liv_Area + TotRms_AbvGrd + Exter_Cond + Bsmt_Cond +
  Year_Sold + Year_Remod_Add, 
 data = ames_train) %>%
  step_mutate(
    Age_House = Year_Sold - Year_Remod_Add,
    Exter_Cond = forcats::fct_collapse(Exter_Cond, Good = c("Typical", "Good", "Excellent"))) %>% 
  step_relevel(Exter_Cond, ref_level = "Good") %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ matches("Bsmt_Cond"):TotRms_AbvGrd) 

receta_casas


# Paso 3: Selección de tipo de modelo con hiperparámetros iniciales

knn_model <- nearest_neighbor(
  mode = "regression",
  neighbors = tune("K"),
  weight_func = tune()) %>% 
  set_engine("kknn")


# Paso 4: Inicialización de workflow o pipeline

knn_workflow <- workflow() %>% 
  add_recipe(receta_casas) %>% 
  add_model(knn_model)



# Paso 5: Creación de grid search

knn_parameters_set <- extract_parameter_set_dials(knn_workflow) %>% 
 update(
  K = dials::neighbors(c(1,80)),
  weight_func = weight_func(values = c("triangular", "inv", "gaussian"))
  )

set.seed(123)
knn_grid <- knn_parameters_set %>% 
  grid_max_entropy(size = 500)

ctrl_grid <- control_grid(save_pred = T, verbose = T)


# Paso 6: Entrenamiento de modelos con hiperparámetros definidos

UseCores <- detectCores() - 1
cluster <- makeCluster(UseCores)
registerDoParallel(cluster)

knnt1 <- Sys.time()
knn_tune_result <- tune_grid(
  knn_workflow,
  resamples = ames_folds,
  grid = knn_grid,
  metrics = metric_set(rmse, mae, mape, rsq),
  control = ctrl_grid
)
knnt2 <- Sys.time(); knnt2 - knnt1

stopCluster(cluster)
knn_tune_result %>% saveRDS("models/knn_model_reg.rds")


knn_tune_result <- readRDS("models/knn_model_reg.rds")

# Paso 7: Análisis de métricas de error e hiperparámetros (Vuelve al paso 3, si es necesario)

collect_metrics(knn_tune_result)

knn_tune_result %>% autoplot()

knn_tune_result %>% 
  autoplot(metric = "rmse")

knn_tune_result %>% 
  autoplot(metric = "mae")




# Paso 8: Selección de modelo a usar

show_best(knn_tune_result, n = 10, metric = "rmse")

knn_tune_result %>% show_best(n = 10, metric = "mape") %>% 
 arrange(K)

best_knn_model_reg <- knn_tune_result %>% select_best(metric = "rsq")
best_knn_model_reg

knn_regression_best_1se_model <- knn_tune_result %>% 
  select_by_one_std_err(metric = "mape", "mape")

knn_regression_best_1se_model

# Paso 9: Ajuste de modelo final con todos los datos (Vuelve al paso 2, si es necesario)

final_knn_model_reg <- knn_workflow %>% 
  finalize_workflow(best_knn_model_reg) %>% 
  parsnip::fit(data = ames_train)

pfun <- function(object, newdata) predict(object, new_data = newdata) %>% pull(.pred)

ames_importance <- final_knn_model_reg %>%
  extract_fit_parsnip() %>%
  vip::vi(
    method = "permute",
    nsim = 30,
    target = "Sale_Price",
    metric = "rmse",
    pred_wrapper = pfun,
    train = receta_casas %>% prep() %>% juice()
  )

# ames_importance %>% saveRDS("models/vip_ames_knn.rds")

ames_importance <- readRDS("models/vip_ames_knn.rds")
ames_importance

ames_importance %>%
  mutate(Variable = forcats::fct_reorder(Variable, Importance)) %>%
  slice_max(Importance, n = 20) %>%
  ggplot(aes(Importance, Variable, color = Variable)) +
  geom_errorbar(aes(xmin = Importance - StDev, xmax = Importance + StDev),
    alpha = 0.5, size = 1) +
  geom_point(size = 2) +
  theme(legend.position = "none") +
  ggtitle("Variable Importance Measure")



# Paso 10: Validar poder predictivo con datos de prueba

results_reg <- predict(final_knn_model_reg, ames_test) %>% 
  dplyr::bind_cols(Sale_Price = ames_test$Sale_Price, .) %>% 
  dplyr::rename(pred_knn_reg = .pred)
results_reg


multi_metric <- metric_set(mae, mape, rmse, rsq)
multi_metric(results_reg, truth = Sale_Price, estimate = pred_knn_reg) %>% 
  mutate(.estimate = round(.estimate, 2)) %>% 
  select(-.estimator)

results_reg %>% 
  ggplot(aes(x = pred_knn_reg, y = Sale_Price)) +
  geom_point() +
  geom_abline(color = "red") +
  xlab("Prediction") +
  ylab("Observation") +
  ggtitle("Comparisson")

