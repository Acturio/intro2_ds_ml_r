pacman::p_load(
 tidymodels
 )

tidymodels_prefer()

data(ames)


lm(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type, data = ames)

# Recetas

simple_ames <- recipe(
  Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
  data = ames) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())

simple_ames

## Pasos y estructura de recetas

prep <- prep(simple_ames) 
prep


simple_ames %>% 
  prep() %>% 
  bake(new_data = NULL) %>% 
  glimpse()

## Normalizar columnas numéricas


ames %>% 
 select(Sale_Price, Neighborhood, Gr_Liv_Area, Year_Built, Bldg_Type) %>% 
 head(5)

simple_ames <- recipe(Sale_Price ~ ., data = ames) %>%
  step_normalize(all_numeric_predictors())

simple_ames

simple_ames %>% 
  prep() %>% 
  bake(new_data = NULL) %>% 
  select(Sale_Price, Neighborhood, Gr_Liv_Area, Year_Built, Bldg_Type) %>% 
  head(5)


## Dicotomización de categorías

ames %>% select(Sale_Price, Bldg_Type) %>% head(5)

ames %>% select(Bldg_Type) %>% distinct() %>% pull()

simple_ames <- recipe(Sale_Price ~ Bldg_Type, data = ames) %>%
  step_dummy(all_nominal_predictors()) %>% 
  prep()

simple_ames

simple_ames %>% bake(new_data = NULL) %>% head(5)

## Codificación de datos cualitativos nuevos o faltantes

ggplot(data = ames, aes(y = Neighborhood)) + 
  geom_bar() + 
  labs(y = NULL)


simple_ames <- recipe(
  Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
  data = ames) %>%
  step_other(Neighborhood, threshold = 0.01, other = "Otros") %>% 
  prep()

ejemplo <- juice(simple_ames)

ggplot(ejemplo, aes(y = Neighborhood)) + 
  geom_bar() + 
  labs(y = NULL)

## Imputaciones

ames_na <- ames
ames_na[sample(nrow(ames), 5), c("Gr_Liv_Area", "Lot_Area", "Neighborhood", "Bldg_Type")] <- NA

ames_na %>% filter(is.na(Gr_Liv_Area) | is.na(Lot_Area)) %>% 
  select(Sale_Price, Gr_Liv_Area, Lot_Area, Neighborhood, Bldg_Type)

simple_ames <- recipe(Sale_Price ~ Gr_Liv_Area + Lot_Area + Neighborhood + Bldg_Type, data = ames_na) %>%
  step_impute_mean(Gr_Liv_Area) %>% 
  step_impute_median(Lot_Area) %>% 
  step_unknown(Neighborhood) %>% 
  prep()

bake(simple_ames, new_data = ames_na) %>% 
  filter(is.na(Gr_Liv_Area) | is.na(Lot_Area))

bake(simple_ames, new_data = ames_na) %>% 
 filter(is.na(Bldg_Type) | is.na(Lot_Area))

## Agregar o modificar columnas

ejemplo <- recipe(
  Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Year_Remod_Add,
  data = ames) %>%
  step_mutate(
    Sale_Price_Peso = Sale_Price * 19.87,
    Last_Inversion = Year_Remod_Add - Year_Built
    ) %>% 
  step_arrange(desc(Last_Inversion)) %>% 
  prep()

ejemplo

ejemplo %>% bake(new_data = NULL) %>% 
  select(Sale_Price, Sale_Price_Peso, Year_Remod_Add, Year_Built, Last_Inversion)


## Interacciones

ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price)) + 
  geom_point(alpha = .2) +
  facet_wrap(~ Bldg_Type) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, col = "red") + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Gross Living Area", y = "Sale Price (USD)")


simple_ames <- recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames) %>%
  step_other(Neighborhood, threshold = 0.05) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  prep()

simple_ames %>% bake(new_data = NULL) %>% glimpse()



















