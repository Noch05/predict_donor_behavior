library(tidyverse)
library(tidymodels)
library(xgboost)
library(butcher)
library(future)

plan(multisession, workers = 12)
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
  income + weights

xg <- boost_tree(trees = tune(), mtry = tune(), learn_rate = tune(),
                 loss_reduction = tune(), sample_size = tune(), min_n = tune())%>%
  set_engine("xgboost", booster = "gbtree",
             num_parallel_tree = 1, verbose = 2, objective = "binary:logistic", 
             tree_method = "hist") %>%
  set_mode("classification")

rec <- recipe(formula, data = train) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)


wf <- workflow() %>%
  add_model(xg) %>%
  add_recipe(rec)

params <-extract_parameter_set_dials(xg)

params <- update(params, mtry = mtry(range = c(1,158)),
                trees = trees(range = c(100, 3000)),
                min_n = min_n(range = c(1,1000)))
search <- grid_space_filling(params, size =10)


folds <- vfold_cv(train, v = 5)
start <- Sys.time()
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

bayes <- tune_bayes(
  wf,
  resamples = folds,
  iter = 30,
  metrics = metric_set(f_meas),
  initial = tuned_results,
  param_info = params,
  control = control_bayes(
    verbose = TRUE,
    verbose_iter = TRUE,
    no_improve = 5,
    seed = 21434,
    event_level = "second",
    allow_par = TRUE,
    parallel_over = "everything"
  ))

end <- Sys.time()
time <- end - start

metrics <- collect_metrics(bayes)

best <- select_best(bayes)


