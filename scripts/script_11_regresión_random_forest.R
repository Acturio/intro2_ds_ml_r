pacman::p_load(
  tidymodels,
  ranger,
  doParallel,
  vip,
  plotly,
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
  step_interact(~ matches("Bsmt_Cond"):TotRms_AbvGrd) #%>%
  #prep()

receta_casas

# Paso 3: Selección de tipo de modelo con hiperparámetros iniciales

rforest_model <- rand_forest(
  mode = "regression",
  trees = 300,
  mtry = tune(),
  min_n = tune()) %>%
  set_engine("ranger", importance = "impurity")

# Paso 4: Inicialización de workflow o pipeline

rforest_workflow <- workflow() %>%
  add_model(rforest_model) %>%
  add_recipe(receta_casas)

# Paso 5: Creación de grid search

set.seed(195628)
rforest_param_grid <- grid_random(
  mtry(range = c(5,7)),
  min_n(range = c(2,32)),
  size = 200
)

ctrl_grid <- control_grid(save_pred = T, verbose = T)

# Paso 6: Entrenamiento de modelos con hiperparámetros definidos
UseCores <- detectCores() - 1
cluster <- makeCluster(UseCores)
registerDoParallel(cluster)

rft1 <- Sys.time()
rforest_tune_result <- tune_grid(
  rforest_workflow,
  resamples = ames_folds,
  grid = rforest_param_grid,
  metrics = metric_set(rmse, rsq, mae),
  control = ctrl_grid
)
rft2 <- Sys.time(); rft2 - rft1
stopCluster(cluster)

rforest_tune_result %>% saveRDS("models/random_forest_model_reg.rds")

rforest_tune_result <- readRDS("models/random_forest_model_reg.rds")

# Paso 7: Análisis de métricas de error e hiperparámetros (Vuelve al paso 3, si es necesario)

collect_metrics(rforest_tune_result)

rforest_tune_result %>% autoplot()
rforest_tune_result %>% autoplot(metric = "rsq")

multiparams_plot <- rforest_tune_result %>%
 collect_metrics() %>%
 filter(.metric == "rmse") %>%
 rename(rmse = mean) %>%
 ggplot(aes(x = mtry, y = min_n, colour = rmse)) +
 geom_point() +
 scale_color_gradientn(colours = rainbow(7)) +
 labs(
  title = "Análisis de R^2 mediante ajuste de hiperparámetros",
  x = "Número de ramas",
  y = "Mínimo de elementos por nodo"
  )

ggplotly(multiparams_plot)

# Paso 8: Selección de modelo a usar

show_best(rforest_tune_result, n = 10, metric = "rmse")

best_rforest_model <- select_best(rforest_tune_result, metric = "rmse")
best_rforest_model

# Paso 9: Ajuste de modelo final con todos los datos (Vuelve al paso 2, si es necesario)

final_rforest_model <- rforest_workflow %>%
  finalize_workflow(best_rforest_model) %>%
  fit(data = ames_train)

final_rforest_model %>%
 extract_fit_parsnip() %>%
 vip(geom = "col") +
 ggtitle("Importancia de las variables")


# Paso 10: Validar poder predictivo con datos de prueba

results <- predict(final_rforest_model, ames_test) %>%
  dplyr::bind_cols(Sale_Price = ames_test$Sale_Price) %>%
  dplyr::rename(pred_rforest_reg = .pred)

results

multi_metric <- metric_set(mae, mape, rmse, rsq, ccc)
multi_metric(results, truth = Sale_Price, estimate = pred_rforest_reg) %>%
  mutate(.estimate = round(.estimate, 2)) %>%
  select(-.estimator)

results %>%
  ggplot(aes(x = pred_rforest_reg, y = Sale_Price)) +
  geom_point() +
  geom_abline(color = "red") +
  xlab("Prediction") +
  ylab("Observation") +
  ggtitle("Comparisson")


