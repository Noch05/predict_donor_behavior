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
library(ranger)
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

rf <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
  set_engine("ranger", seed = 581284, verbose = TRUE, importance = "none",
             num.threads = 30) %>%
  set_mode("classification")

rec <- recipe(donate ~., data = train)

wf <- workflow() %>%
  add_model(rf) %>%
  add_recipe(rec)


params <- extract_parameter_set_dials(rf) %>%
  update(
    mtry = mtry(range = c(1, 27)),
    trees = trees(range = c(500, 2500)),
    min_n = min_n(range = c(1, 500))
  )
set.seed(53425)
tuning_grid <- grid_random(
  params,
  size = 50)

set.seed(512854)
cv <- vfold_cv(train, v = 10, repeats= 5, strat = donate)
print("tune start")
set.seed(65941)
first <- tune_grid(
  wf,
  resamples = cv,
  metrics = metric_set(f_meas),
  grid = tuning_grid,
  control = control_grid(
    verbose = TRUE,
    event_level = "second",
    allow_par = TRUE,
    parallel_over = "everything"))
set.seed(5592)
print("bayes start")
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
    no_improve = 15,
    seed = 95682,
    event_level = "second",
    allow_par = TRUE,
    parallel_over = "everything"
))

metrics <- collect_metrics(second)

write_rds(metrics, "params/rf_hyperparams.rds")

best <- select_best(second)

print("final fit")
fit <- fit(finalize_workflow(wf, best), train)

fit <- butcher(fit)

write_rds(fit, "models/rf_model.rds")
















