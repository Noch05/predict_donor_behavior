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
metrics <- read_rds("models/gbm_tuning.rds")$results
formula <- recipe(donate ~ ., data = train)


best_gbm <- slice_max(metrics, order_by = mean)
gbm <- boost_tree(
  trees = best_gbm$trees,
  mtry = best_gbm$mtry,
  learn_rate = best_gbm$learn_rate,
  loss_reduction = best_gbm$loss_reduction,
  min_n = best_gbm$min_n,
  tree_depth = best_gbm$tree_depth
) |>
  set_engine(
    "lightgbm",
    boosting_type = "gbdt",
    objective = "binary",
    metric = "binary_logloss",
    num_thread = 12,
    deterministic = TRUE #Ensures Reproducibility
  ) |>
  set_mode("classification")
wf <- workflow() |>
  add_model(gbm) |>
  add_recipe(formula) |>
  add_case_weights(weights)

fit(wf, train) |>
  butcher() |>
  write_rds(file = "models/gbm.rds")
