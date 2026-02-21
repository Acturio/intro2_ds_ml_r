library(tidymodels)
library(readr)

telco <- read_csv("data/Churn.csv")

set.seed(1234)
telco_split <- initial_split(telco, prop = .7)
telco_train <- training(telco_split)
telco_test  <- testing(telco_split)

telco_rec <- recipe(
    Churn ~ Dependents + tenure + MonthlyCharges + customerID,
    data = telco_train) %>%
  update_role(customerID, new_role = "id variable") %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  prep()

telco_juiced <- juice(telco_rec)
glimpse(telco_juiced)

telco_test_bake <-  bake(telco_rec, new_data = telco_test)
glimpse(telco_test_bake)

logistic_model <-  logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

logistic_fit1 <- parsnip::fit(logistic_model, Churn ~ ., telco_juiced)

logistic_p_test <- predict(logistic_fit1, telco_test_bake) %>%
  bind_cols(telco_test_bake) %>%
  select(.pred_class, Churn)

logistic_p_test

logistic_p_test %>%
  yardstick::conf_mat(truth = Churn, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

bind_rows(
    yardstick::accuracy(logistic_p_test, Churn, .pred_class, event_level = "second"),
    yardstick::precision(logistic_p_test, Churn, .pred_class, event_level = "second"),
    yardstick::recall(logistic_p_test, Churn, .pred_class, event_level = "second"),
    yardstick::specificity(logistic_p_test, Churn, .pred_class, event_level = "second"),
    yardstick::f_meas(logistic_p_test, Churn, .pred_class, event_level = "second")
  )

logistic_p_test_prob <- predict(logistic_fit1, telco_test_bake, type = "prob") %>%
  bind_cols(telco_test_bake) %>%
  select(.pred_Yes, .pred_No, Churn)

logistic_p_test_prob

logistic_p_test_prob %>%
    ggplot(aes(x = .pred_Yes)) +
    geom_histogram(color = "white", fill = "blue") +
    labs(
        title = "Distribución de probabilidad de cancelación de servicio",
        x = "Probabilidad",
        y = "Conteo"
        )


logistic_p_test_prob <- logistic_p_test_prob %>%
  mutate(.pred_class  = as.factor(if_else ( .pred_Yes >= 0.30, 'Yes', 'No'))) %>%
  relocate(.pred_class , .after = .pred_No)

logistic_p_test_prob

bind_rows(
    yardstick::accuracy(logistic_p_test_prob, Churn, .pred_class, event_level = "second"),
    yardstick::precision(logistic_p_test_prob, Churn, .pred_class, event_level = "second"),
    yardstick::recall(logistic_p_test_prob, Churn, .pred_class, event_level = "second"),
    yardstick::specificity(logistic_p_test_prob, Churn, .pred_class, event_level = "second"),
    yardstick::f_meas(logistic_p_test_prob, Churn, .pred_class, event_level = "second")
  )


