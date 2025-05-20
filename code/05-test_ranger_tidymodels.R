library(tidyverse)
library(tidymodels)
library(future)
library(ranger)
library(butcher)
library(themis)

plan(multisession, workers = 12)
train <- read_rds("data/train.rds")[[1]]

train <- train %>% mutate(weights = if_else(
  donate == "Yes",
  nrow(train) /(2 *table(donate)[[2]]),
  nrow(train) / (2*(nrow(train) - table(donate)[[2]]))
),
weights = importance_weights(weights))

formula <- donate ~ state + male + race + educ + marriage +
  ideology + votereg + union + employ + party + parent +
  recognition + pol_activities + contacted + ran_for_office +
  age + immigrant + mil + vote_prior + vote_intent + investor +
  pol_interest + religion + relig_imp + own_home + news_activities +
  income + weights

rf <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
  set_engine("ranger", seed = 52348, verbose = TRUE, importance = "none") %>%
  set_mode("classification")

rec <- recipe(formula, data = train) %>%
  step_downsample(donate)

wf <- workflow() %>%
  add_model(rf) %>%
  add_recipe(rec)



params <- extract_parameter_set_dials(rf) %>%
  update(
    mtry = mtry(range = c(1, 27)),
    trees = trees(range = c(100, 2000)),
    min_n = min_n(range = c(1, 1000))
  )
tuning_grid <- grid_space_filling(
  params,
  size = 2)

start <- Sys.time()

set.seed(50423)
cv <- vfold_cv(train, v = 5, strat = donate)
set.seed(50423)
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

second <- tune_bayes(
  wf,
  resamples = cv,
  iter = 2,
  metrics = metric_set(f_meas),
  initial = first,
  param_info = params,
  control = control_bayes(
    verbose = TRUE,
    verbose_iter = TRUE,
    no_improve = 2,
    seed = 21434,
    event_level = "second",
    allow_par = TRUE,
    parallel_over = "everything"
))

end <- Sys.time()
time <- end - start

met2 <- collect_metrics(second)

best <- select_best(second)

rf_final <- rand_forest(mode = "classification", mtry = best$mtry, trees=best$trees,
                         min_n = best$min_n) %>%
  set_engine("ranger", num.threads = 8, importance = "none")

wf_final <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_final)

fit <- fit(wf_final, train)














