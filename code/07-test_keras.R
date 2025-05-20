library(tidyverse)
library(tidymodels)
library(keras)
library(butcher)

## For the First Time, make sure you have a python installation
## reticulate::install_python(version = "Prefered Version")
## Run install_keras() to install Tensorflow/Keras Python Libraries

train <- sample_n(read_rds("data/train.rds")[[1]], 40000)

train <- train %>% mutate(weights = if_else(
  donate == "Yes",
  10 / sqrt(table(donate)[2]),
  10 / sqrt(nrow(train) - table(donate)[2])
),
weights = importance_weights(weights))

formula <- donate ~ state + male + race + educ + marriage +
  ideology + votereg + union + employ + party + parent +
  recognition + pol_activities + contacted + ran_for_office +
  age + immigrant + mil + vote_prior + vote_intent + investor +
  pol_interest + religion + relig_imp + own_home + news_activities +
  income

nn <- mlp(hidden_units = tune(), epochs = tune())%>%
  set_engine("keras") %>%
  set_mode("classification")

rec <- recipe(formula, data = train) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_center(all_predictors()) %>% step_zv() %>% step_scale(all_predictors())
  


wf <- workflow() %>%
  add_model(nn) %>%
  add_recipe(rec)

params <-extract_parameter_set_dials(nn)

params <- update(params, hidden_units = hidden_units(range = c(10,500)),
                 epochs = epochs(range = c(10,100)))
search <- grid_space_filling(params, size = 5)

folds <- vfold_cv(train, v = 5)
start <- Sys.time()
tuned_results <- tune_grid(
  wf,
  resamples = folds,
  grid = search,
  metrics = metric_set(f_meas),
  control = control_grid(
    verbose = TRUE,
    event_level = "second"))

bayes <- tune_bayes(
  wf,
  resamples = folds,
  iter = 10,
  metrics = metric_set(f_meas),
  initial = tuned_results,
  param_info = params,
  control = control_bayes(
    verbose = TRUE,
    verbose_iter = TRUE,
    no_improve = 5,
    seed = 21434,
    event_level = "second"))

end <- Sys.time()
time <- end - start

metrics <- collect_metrics(bayes)


