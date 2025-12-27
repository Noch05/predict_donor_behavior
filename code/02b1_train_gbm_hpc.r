# Seeds are set to unique numbers, to allow for clear replication
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
library(dplyr)
library(lightgbm)
library(bonsai)
library(backports)
library(yardstick)

train <- read_rds("data/train_data.rds")

gbm <- boost_tree(
  trees = tune(),
  mtry = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  min_n = tune(),
  tree_depth = tune()
) |>
  set_engine(
    "lightgbm",
    boosting_type = "gbdt",
    objective = "binary",
    metric = "binary_logloss",
    num_thread = 48,
    deterministic = TRUE
  ) |>
  set_mode("classification")

formula <- recipe(donate ~ ., data = train)

wf <- workflow() |>
  add_model(gbm) |>
  add_recipe(formula) |>
  add_case_weights(weights)

max_try <- ncol(train) - 2 ## 27, 29 columns, minus weights and outcome.

params <- extract_parameter_set_dials(gbm) |>
  recipes::update(
    mtry = mtry(range = c(1, max_try)),
    trees = trees(range = c(100, 3000)),
    min_n = min_n(range = c(1, 1000)),
    tree_depth = tree_depth(range = c(3, 25))
  )
## Choosing a more coarse grid of min_n then tidy models gives back
min_n_vals <- c(1, 5, 10, 25, 50, 100, 200, 500, 1000)
set.seed(524832)
tuning_grid <- grid_random(
  x = params,
  size = 150
) |>
  mutate(
    trees = round(trees, -2),
    min_n = sample(min_n_vals, size = 150, replace = TRUE)
  )
tuning_time <- system.time({
  set.seed(9549823)
  cv <- vfold_cv(train, v = 10, strat = donate)
  set.seed(90658412)
  first <- tune_grid(
    wf,
    resamples = cv,
    metrics = metric_set(f_meas),
    grid = tuning_grid,
    control = control_grid(
      verbose = TRUE,
      event_level = "second",
      allow_par = TRUE,
      parallel_over = "everything",
      save_workflow = TRUE
    )
  )
  second <- tune_bayes(
    wf,
    resamples = cv,
    iter = 50,
    metrics = metric_set(f_meas),
    initial = first,
    param_info = params,
    control = control_bayes(
      verbose = TRUE,
      verbose_iter = TRUE,
      no_improve = 25,
      seed = 684340,
      event_level = "second",
      allow_par = TRUE,
      parallel_over = "everything",
      save_workflow = TRUE
    )
  )
})

metrics <- collect_metrics(second)

list(
  results = metrics,
  time = tuning_time
) |>
  write_rds(file = "models/gbm_tuning.rds")
