library(withr)
library(tidyr)
library(broom)
library(conflicted)
library(purrr)
library(dials)
library(hardhat)
library(parsnip)
library(recipes)
library(rsample)
library(workflows)
library(workflowsets)
library(tune)
library(butcher)
library(readr)
library(forcats)
library(dplyr)
library(reticulate)
library(tensorflow)
library(keras)
library(themis)
library(backports)
library(doParallel)
library(future)
library(batchtools)
library(future.batchtools)
library(furrr)
library(yardstick)


train <- read_rds("data/train.rds")[[1]] %>% select(-year)


model <- mlp(mode = "classification", hidden_units = tune(), epochs = tune(), dropout = tune(),
             activation = tune()) %>%
  set_engine("keras")

rec <- recipe(donate ~., data = train) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_center(all_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_scale(all_predictors()) %>% 
  step_downsample(under_ratio = 2) %>%
  step_upsample(over_ratio = 1)
  

wf <- workflow() %>%
  add_model(model) %>%
  add_recipe(rec)



params <-extract_parameter_set_dials(model) %>%
  update(hidden_units = hidden_units(range = c(10, 1000)),
         epochs = epochs(range = c(10, 1000)),
         activation = activation(c("relu", "tanh", "elu", "selu",
                                     "swish", "tanh", "gelu", "softplus")))

use_virtualenv("/home/no9857a-hpc/.virtualenvs/r-tensorflow", required = TRUE)
         
set.seed(554321)
search <- grid_random(params, size = 50)
set.seed(52352)
folds <- vfold_cv(train, v = 10, repeats = 5, strat = donate)
set.seed(5598182)
print("tune start")
tuned_results <- tune_grid(
  wf,
  resamples = folds,
  grid = search,
  metrics = metric_set(f_meas),
  control = control_grid(
      verbose = TRUE,
      allow_par = TRUE,
      event_level = "second",
      parallel_over = "everything"))
set.seed(6523)
print("bayes start")
bayes <- tune_bayes(
  wf,
  resamples = folds,
  iter = 50,
  metrics = metric_set(f_meas),
  initial = tuned_results,
  param_info = params,
  control = control_bayes(
    verbose = TRUE,
    verbose_iter = TRUE,
    no_improve = 15,
    seed = 098712,
    event_level = "second",
    allow_par = TRUE,
    parallel_over = "everything"))


metrics <- collect_metrics(bayes)

write_rds(metrics, "params/mlp_hyperparams.rds")

best <- select_best(bayes)

print("final fit")
fit <- fit(finalize_workflow(wf, best), train)

fit <- butcher(fit)

write_rds(fit, "models/mlp_model.rds")





