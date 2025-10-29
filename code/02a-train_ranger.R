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
library(ranger)
library(backports)
library(yardstick)

train <- read_rds("data/train_data.rds")

model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) |>
  set_engine("ranger",
    seed = 97841983, verbose = TRUE, importance = "none",
    num.threads = 48
  ) |>
  set_mode("classification")

formula <- recipe(donate ~ ., data = train)

wf <- workflow() |>
  add_model(model) |>
  add_recipe(formula) |>
  add_case_weights(weights)

max_try <- ncol(train) - 2 ## 27, 29 columns, minus weights and outcome.


params <- extract_parameter_set_dials(model) |>
  recipes::update(
    mtry = mtry(range = c(1, max_try)),
    trees = trees(range = c(100, 3000)),
    min_n = min_n(range = c(1, 1000))
  )

min_n_vals <- c(1, 5, 10, 25, 50, 100, 200, 500, 1000)
set.seed(91248)
tuning_grid <-
  grid_random(
    x = params,
    size = 100
  ) |>
  mutate(
    trees = round(trees, -2),
    min_n = sample(min_n_vals, size = 100, replace = TRUE)
  )
tuning_time <- system.time({
  set.seed(01894964)
  cv <- vfold_cv(train, v = 10, strat = donate)
  set.seed(9128476)
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
      seed = 559275493,
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
  write_rds(file = "models/rf_tuning.rds")

best_rf <- slice_max(metrics, order_by = mean)
rf <- rand_forest(
  trees = best_rf$trees, mtry = best_rf$mtry, min_n = best_rf$min_n,
) |>
  set_engine("ranger",
    num.threads = 12,
    seed = 97841983,
    verbose = TRUE, importance = "none"
  ) |>
  set_mode("classification")

wf <- workflow() |>
  add_model(rf) |>
  add_recipe(formula) |>
  add_case_weights(weights)

fit(wf, train) |>
  butcher() |>
  write_rds(file = "models/rf.rds")
