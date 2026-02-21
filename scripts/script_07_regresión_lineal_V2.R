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
#
receta_casas <- recipe(Sale_Price ~ . , data = ames_train) %>%
  step_unknown(Alley) %>%
  step_rename(Year_Remod = Year_Remod_Add) %>%
  step_rename(ThirdSsn_Porch = Three_season_porch) %>%
  step_ratio(Bedroom_AbvGr, denom = denom_vars(Gr_Liv_Area)) %>%
  step_mutate(
    Age_House = Year_Sold - Year_Remod,
    TotalSF   = Gr_Liv_Area + Total_Bsmt_SF,
    AvgRoomSF   = Gr_Liv_Area / TotRms_AbvGrd,
    Pool = if_else(Pool_Area > 0, 1, 0),
    Exter_Cond = forcats::fct_collapse(
     Exter_Cond, Good = c("Typical", "Good", "Excellent")
     )
    ) %>%
  step_relevel(Exter_Cond, ref_level = "Good") %>%
  step_normalize(all_predictors(), -all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ Second_Flr_SF:First_Flr_SF) %>%
  step_interact(~ matches("Bsmt_Cond"):TotRms_AbvGrd) %>%
  step_rm(
    First_Flr_SF, Second_Flr_SF, Year_Remod,
    Bsmt_Full_Bath, Bsmt_Half_Bath,
    Kitchen_AbvGr, BsmtFin_Type_1_Unf,
    Total_Bsmt_SF, Kitchen_AbvGr, Pool_Area,
    Gr_Liv_Area, Sale_Type_Oth, Sale_Type_VWD,
    Bsmt_Cond_Typical_x_TotRms_AbvGrd,
    Garage_Cond_No_Garage,
    Pool_QC_No_Pool,
    BsmtFin_Type_1_No_Basement,
    Exterior_2nd_PreCast,
    Exterior_1st_PreCast,
    Exterior_1st_ImStucc,
    Overall_Cond_Very_Excellent,
    Roof_Matl_Roll,
    Roof_Matl_Membran,
    Bldg_Type_Duplex,
    Condition_2_RRNn,
    Condition_2_RRAn,
    Neighborhood_Hayden_Lake,
    Alley_unknown,
    Garage_Finish_No_Garage,
    Functional_Sal
  ) %>%
  prep()

casa_juiced <- juice(receta_casas) # bake(receta_casas, new_date=NULL)
casa_juiced <- receta_casas %>% bake(new_data = NULL)
casa_test_bake <- receta_casas %>% bake(new_data = ames_test)


modelo1 <-  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

lm_fit1 <- fit(modelo1, Sale_Price ~ ., casa_juiced)

p_test <- predict(lm_fit1, casa_test_bake) %>%
  bind_cols(ames_test) %>%
  select(.pred, Sale_Price) %>%
  mutate(error = Sale_Price - .pred)

p_test

# p_test %>% summarise(
#  s2_pred = sum((.pred-mean(Sale_Price)^2)),
#  s2_obs = sum((Sale_Price-mean(Sale_Price)^2)),
#  n = n()
# ) %>% 
#  mutate(
#  r2 = s2_pred/s2_obs,
#  r2_adj = 1- (n-1)/(n-258-1)*(1-r2)
# )

# Coeficientes del modelo

lm_fit1 %>% tidy() %>% arrange(p.value)

lm_fit1 %>% tidy() %>% arrange(desc(p.value))

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
  ggtitle("Predicción vs Observación")

error_line <- p_test %>%
  ggplot(aes(x = Sale_Price, y = error)) +
  geom_line() + geom_hline(yintercept = 0, color = "red") +
  xlab("Observaciones") + ylab("Errores") +
  ggtitle("Varianza de errores")

pred_obs_plot + error_line


error_dist <- p_test %>%
  ggplot(aes(x = error)) +
  geom_histogram(color = "white", fill = "black") +
  geom_vline(xintercept = 0, color = "red") +
  ylab("Conteos de clase") + xlab("Errores") +
  ggtitle("Distribución de error")

error_qqplot <- p_test %>%
  ggplot(aes(sample = error)) +
  geom_qq(alpha = 0.3) + stat_qq_line(color = "red") +
  xlab("Distribución normal") + ylab("Distribución de errores") +
  ggtitle("QQ-Plot")








