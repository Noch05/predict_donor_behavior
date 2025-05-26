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
library(xgboost)
library(backports)
library(doParallel)
library(future)
library(batchtools)
library(future.batchtools)
library(furrr)
library(yardstick)


train <- read_rds("data/train.rds")[[1]]

train <- train %>% mutate(
  weights = if_else(donate == "Yes",
                    nrow(train)/ (2*(table(donate)[[2]])),
                    nrow(train)/ (2* (table(donate)[[1]]))
  ),
  weights = importance_weights(weights)) %>%
  select(-year)

xg <- boost_tree(trees = tune(), mtry = tune(), learn_rate = tune(),
                 loss_reduction = tune(), sample_size = tune(), min_n = tune(),
                 tree_depth = tune(), stop_iter = tune()) %>%
  set_engine("xgboost", booster = "gbtree",
              verbose = 2, objective = "binary:logistic", 
             tree_method = "hist", event_level = "second",
             validation = 0.1, nthread = 30) %>%
  set_mode("classification")

rec <- recipe(donate ~., data = train) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)


wf <- workflow() %>%
  add_model(xg) %>%
  add_recipe(rec)

params <-extract_parameter_set_dials(xg) %>%
  update(mtry = mtry(range = c(1,158)),
                trees = trees(range = c(100, 2500)),
                min_n = min_n(range = c(1, 500)),
                tree_depth = tree_depth(range = c(5, 35)),
         stop_iter = stop_iter(range = c(20, 100)))

set.seed(583274)
search <- grid_random(params, size = 50)

set.seed(983712)
folds <- vfold_cv(train, v = 10, repeats = 5, strat = donate)

print("tune start")
set.seed(56832)
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
set.seed(9184)
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
    seed = 582374,
    event_level = "second",
    allow_par = TRUE,
    parallel_over = "everything"
  ))


metrics <- collect_metrics(bayes)
write_rds(metrics, "params/xgboost_hyperparams.rds")

best <- select_best(bayes)

print("final fit")
fit <- fit(finalize_workflow(wf, best), train)

fit <- butcher(fit)

write_rds(fit, "models/xgboost_model.rds")






