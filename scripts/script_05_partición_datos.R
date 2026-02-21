pacman::p_load(
 tidymodels
 )

tidymodels_prefer()

data(ames)

# Fijar un número aleatorio con para que los resultados puedan ser reproducibles 
# Partición 80/20 de los datos
set.seed(123)
ames_split <- initial_split(ames, prop = 0.80)
ames_split

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)

set.seed(123)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

set.seed(12)
val_set <- validation_split(ames_train, prop = 3/4, strata = NULL)
val_set #val_set contiene el conjunto de entrenamiento y validación.


set.seed(55)
ames_loo <- loo_cv(ames_train)
ames_loo


set.seed(55)
ames_folds <- vfold_cv(ames_train, v = 10)
ames_folds

ames_train %>% 
 mutate(id_fold = sample(1:10, 2342, replace = T)) %>% 
 relocate(id_fold, .before = MS_SubClass) %>% 
 pull(id_fold) %>% 
 table()






# Primer bloque
ames_folds$splits[[1]] %>%
  analysis() %>% # O assessment()
  head(7)


ames_folds$splits[[1]] %>%
 assessment()




















