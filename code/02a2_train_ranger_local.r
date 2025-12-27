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
metrics <- read_rds("models/rf_tuning.rds")$results
formula <- recipe(donate ~ ., data = train)


best_rf <- slice_max(metrics, order_by = mean)
rf <- rand_forest(
  trees = best_rf$trees,
  mtry = best_rf$mtry,
  min_n = best_rf$min_n,
) |>
  set_engine(
    "ranger",
    num.threads = 12,
    seed = 97841983, # Same seed as HPC training
    verbose = TRUE,
    importance = "none"
  ) |>
  set_mode("classification")

wf <- workflow() |>
  add_model(rf) |>
  add_recipe(formula) |>
  add_case_weights(weights)

fit(wf, train) |>
  butcher() |>
  write_rds(file = "models/rf.rds")
