# Librerías

library(tidymodels)
library(MLmetrics)
library(patchwork)

# Carga y partición de datos

data(ames)

set.seed(4595)
ames_split <- initial_split(ames, prop = 0.75)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

# Pre-procesamiento

norm <- ames_train %>%
    select_if(is.numeric) %>%
    cor() %>%
    as_tibble() %>%
    mutate(vars = names(select_if(ames_train, is.numeric))) %>%
    select(vars, Sale_Price) %>%
    arrange(desc(Sale_Price))

# Gr_Liv_Area
# Garage_Cars

plot_1 <- ames_train %>%
  ggplot(aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

receta_casas <- recipe(
    Sale_Price ~ Gr_Liv_Area ,
    data = ames_train
    ) %>%
  prep()

casa_juiced <- juice(receta_casas)
casa_test_bake <- bake(receta_casas, new_data = ames_test)

modelo1 <-  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

lm_fit1 <- fit(modelo1, Sale_Price ~ ., casa_juiced)

p_test <- predict(lm_fit1, casa_test_bake) %>%
  bind_cols(ames_test) %>%
  select(Gr_Liv_Area, .pred, Sale_Price) %>%
  mutate(error = Sale_Price - .pred) %>%
  filter(.pred > 0)

p_test

# Coeficientes del modelo

lm_fit1 %>% tidy()

# Métricas de desempeño

p_test %>%
  summarise(
    MAE = MLmetrics::MAE(.pred, Sale_Price),
    MAPE = MLmetrics::MAPE(.pred, Sale_Price),
    RMSE = MLmetrics::RMSE(.pred, Sale_Price),
    R2 = MLmetrics::R2_Score(.pred, Sale_Price)
  )

pred_obs_plot <- p_test %>%
  ggplot(aes(x = .pred, y = Sale_Price)) +
  geom_point(alpha = 0.2) + geom_abline(color = "red") +
  xlab("Predicciones") + ylab("Observaciones") +
  ggtitle("Predicción vs Observación (RLS)")

error_dist <- p_test %>%
  ggplot(aes(x = error)) +
  geom_histogram(color = "white", fill = "black") +
  geom_vline(xintercept = 0, color = "red") +
  ylab("Conteos de clase") + xlab("Errores") +
  ggtitle("Distribución de error (RLS)")

#########################

receta_casas_2 <- recipe(
    Sale_Price ~ Gr_Liv_Area + Garage_Cars,
    data = ames_train
    ) %>%
  prep()

casa_juiced <- juice(receta_casas_2)
casa_test_bake <- bake(receta_casas_2, new_data = ames_test)

modelo2 <-  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

lm_fit2 <- fit(modelo2, Sale_Price ~ ., casa_juiced)

p_test_2 <- predict(lm_fit2, casa_test_bake) %>%
  bind_cols(ames_test) %>%
  select(Gr_Liv_Area, Garage_Cars, .pred, Sale_Price) %>%
  mutate(error = Sale_Price - .pred) %>%
  filter(.pred > 0)

p_test_2

# Coeficientes del modelo

lm_fit2 %>% tidy()

# Métricas de desempeño

p_test_2 %>%
  summarise(
    MAE = MLmetrics::MAE(.pred, Sale_Price),
    MAPE = MLmetrics::MAPE(.pred, Sale_Price),
    RMSE = MLmetrics::RMSE(.pred, Sale_Price),
    R2 = MLmetrics::R2_Score(.pred, Sale_Price)
  )

pred_obs_plot_2 <- p_test_2 %>%
  ggplot(aes(x = .pred, y = Sale_Price)) +
  geom_point(alpha = 0.2) + geom_abline(color = "red") +
  xlab("Predicciones") + ylab("Observaciones") +
  ggtitle("Predicción vs Observación (RLM)")

error_dist_2 <- p_test_2 %>%
  ggplot(aes(x = error)) +
  geom_histogram(color = "white", fill = "black") +
  geom_vline(xintercept = 0, color = "red") +
  ylab("Conteos de clase") + xlab("Errores") +
  ggtitle("Distribución de error (RLM)")


#####################

bind_rows(
    p_test %>% mutate(Modelo = "Simple"),
    p_test_2 %>% mutate(Modelo = "Múltiple")
    ) %>%
  group_by(Modelo) %>%
  summarise(
    MAE = MLmetrics::MAE(.pred, Sale_Price),
    MAPE = MLmetrics::MAPE(.pred, Sale_Price),
    RMSE = MLmetrics::RMSE(.pred, Sale_Price),
    R2 = MLmetrics::R2_Score(.pred, Sale_Price)
  )

pred_obs_plot + pred_obs_plot_2

error_dist / error_dist_2






