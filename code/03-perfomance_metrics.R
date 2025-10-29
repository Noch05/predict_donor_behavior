library(lightgbm)
library(ranger)
library(rstatix)
library(tidymodels)
library(tidyverse)
library(vip)


train <- read_rds("data/train_data.rds")
test <- read_rds("data/test_data.rds") |> select(-year)
logit <- read_rds("models/logit.rds")
gbm <- read_rds("models/gbm.rds")
rf <- read_rds("models/rf.rds")
rf_params <- read_rds("models/rf_tuning.rds")
gbm_params <- read_rds("models/gbm_tuning.rds")


models <- list(logit = logit, gbm = gbm, rf = rf)
set.seed(54234)

# Predictions and Metrics
predictions <- imap(
  models,
  ~ {
    prob <- predict(.x, test, type = "prob")
    raw <- factor(
      ifelse(prob$.pred_Yes >= .5, "Yes", "No"),
      levels = c("No", "Yes")
    )
    tibble(test$donate, prob, raw) |>
      rename(
        truth = "test$donate",
        prob_yes = ".pred_Yes",
        pred_yes = "raw"
      ) |>
      select(truth, prob_yes, pred_yes) |>
      mutate(correct = truth == pred_yes)
  }
)

conf_mats <- map(
  predictions,
  ~ {
    conf_mat(data = .x, truth = truth, estimate = pred_yes)
  }
)

metrics <- pmap(
  list(predictions, names(predictions), conf_mats),
  ~ {
    tibble(
      model = ..2,
      accuracy = accuracy(
        ..1,
        truth = truth,
        estimate = pred_yes,
        event_level = "second"
      )$.estimate,
      F1 = f_meas(
        ..1,
        truth = truth,
        estimate = pred_yes,
        event_level = "second"
      )$.estimate,
      recall = recall(
        ..1,
        truth = truth,
        estimate = pred_yes,
        event_level = "second"
      )$.estimate,
      precision = precision(
        ..1,
        truth = truth,
        estimate = pred_yes,
        event_level = "second"
      )$.estimate,
      roc = roc_auc(
        ..1,
        truth = truth,
        prob_yes,
        event_level = "second"
      )$.estimate,
      TP = ..3$table["Yes", "Yes"],
      TN = ..3$table["No", "No"],
      FP = ..3$table["Yes", "No"],
      FN = ..3$table["No", "Yes"],
      correct = TP + TN,
      wrong = FP + FN,
      total = sum(TP, TN, FP, FN)
    )
  }
) |>
  bind_rows() |>
  pivot_longer(cols = (!model), names_to = "Metric") |>
  pivot_wider(names_from = model)


rocs <- map(
  predictions,
  ~ {
    roc_curve(.x, truth = truth, prob_yes, event_level = "second")
  }
)

# Across Classification Thresholds
thresholds <- imap(predictions, \(prediction, name) {
  thresholds <- seq(0, 1, 0.01)
  map_df(
    thresholds,
    ~ {
      pred <- factor(
        if_else(prediction$prob_yes < .x, "No", "Yes"),
        levels = c("No", "Yes")
      )
      tibble(
        threshold = .x,
        model = name,
        F1 = f_meas_vec(
          truth = prediction$truth,
          estimate = pred,
          event_level = "second"
        ),
        recall = recall_vec(
          truth = prediction$truth,
          estimate = pred,
          event_level = "second"
        ),
        precision = precision_vec(
          truth = prediction$truth,
          estimate = pred,
          event_level = "second"
        )
      )
    }
  )
}) |>
  bind_rows()


best_F1 <- thresholds |>
  group_by(model) |>
  slice_max(order_by = F1, n = 1, with_ties = FALSE) |>
  select(model, threshold, F1)

## Variable Importance
## Memory Intensive and Time Consuming with Random Forest Included
var_imp <- imap(
  models,
  ~ {
    vip::vi(
      .x,
      method = "permute",
      train = test,
      target = "donate",
      metric = "accuracy",
      pred_wrapper = \(object, newdata) {
        predict(object, new_data = newdata, )$.pred_class
      }
    ) |>
      mutate(N_Importance = nrow(test) * Importance)
  }
)

## Cochran Q

all_correct <- imap(
  predictions,
  ~ {
    .x |>
      mutate(model = .y, id = row_number()) |>
      select(correct, model, id)
  }
) |>
  bind_rows()

CochranQ <- cochran_qtest(all_correct, correct ~ model | id)
McNemar <- pairwise_mcnemar_test(all_correct, correct ~ model | id)

## Exacting Important Items
training_time <- map_dbl(
  models,
  ~ {
    as.numeric(.x$fit$fit$elapsed$elapsed["elapsed"])
  }
)
tuning_time <- map_dbl(
  list(rf = rf_params, gbm = gbm_params),
  ~ {
    as.numeric(.x$time["elapsed"])
  }
)
hyperparams <- map(
  list(rf = rf_params, gbm = gbm_params),
  ~ {
    .x$results
  }
)

rm(
  all_correct,
  gbm,
  logit,
  rf,
  models,
  gbm_params,
  rf_params
)

save.image("envs/model_metrics.RData")
