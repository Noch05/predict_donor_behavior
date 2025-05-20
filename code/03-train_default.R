pathpack <- # Path to Libraries
library(dplyr, lib.loc = pathpack)
library(stringr, lib.loc = pathpack)
library(forcats, lib.loc = pathpack)
library(withr,  lib.loc = pathpack)
library(ggplot2,  lib.loc = pathpack)
library(tzdb, lib.loc = pathpack)
library(readr, lib.loc = pathpack)
library(tibble, lib.loc = pathpack)
library(tidyr, lib.loc = pathpack)
library(purrr, lib.loc = pathpack)
library(lubridate, lib.loc = pathpack)
library(glmnet, lib.loc = pathpack)
library(ranger, lib.loc = pathpack)
library(caret, lib.loc = pathpack)
library(here, lib.loc = pathpack)
library(pROC, lib.loc = pathpack)
library(backports, lib.loc = pathpack)
library(doParallel, lib.loc = pathpack)
library(future, lib.loc = pathpack)
library(future.batchtools, lib.loc = pathpack)
library(furrr, lib.loc = pathpack)
library(nloptr, lib.loc = pathpack)
library(lme4, lib.loc = pathpack)
library(jomo, lib.loc = pathpack)
library(mitml, lib.loc = pathpack)
library(mice, lib.loc = pathpack)
library(DescTools, lib.loc = pathpack)


test <- readRDS("test.rds")
train <- readRDS("train.rds")

formula <- donate ~ state + male + race + educ + marriage +
  ideology + votereg + union + employ + party + parent +
  recognition + pol_activities + contacted + ran_for_office +
  age + immigrant + mil + vote_prior + vote_intent + investor +
  pol_interest + religion + relig_imp + own_home + news_activities +
  income

train$data <- train$data %>% mutate(weights = if_else(
  donate == "Yes",
  10 / sqrt(table(donate)[2]),
  10 / sqrt(nrow(train$data) - table(donate)[2])
))

train$data$union <- if_else(train$data$union =="Yes", 1,0)
test$data$union <- if_else(test$data$union =="Yes", 1,0)

set.seed(23342)
start <- Sys.time()
rf_default <- ranger(formula, train$data, num.trees = 1500,
                     probability = TRUE, importance = "permutation",
                     case.weights = train$data$weights, verbose = TRUE
                     )


saveRDS(rf_default, "rf_def.rds")
end <- Sys.time()

timerf_def <- end - start
saveRDS(timerf_def, "timerf_def.rds")

set.seed(53)
start2 <- Sys.time()
log_default <- glm(formula, data = train$data, weights = train$data$weights, 
                   family = "binomial")
end2 <- Sys.time()
timelog_def <- end2-start2
saveRDS(timelog_def, "timelog_def.rds")
saveRDS(log_default, "log_def.rds")




results <- function(model) {
  predictions <- predict(model, test$data, type = "prob")
  
  thresholds <- seq(0.1, 0.9, by = 0.1)
  
  threshold_results <- lapply(thresholds, function(threshold) {
    pred <- factor(if_else(predictions$Yes > threshold, "Yes", "No"), levels = c("No", "Yes"))
    
    actual <- test$data$donate
    
    confusionMatrix(pred, actual, positive = "Yes")
  })
  
  ## Extract scores
  scores <- tibble(
    Accuracy = sapply(threshold_results, function(cm) cm$overall["Accuracy"]),
    Recall = sapply(threshold_results, function(cm) cm$byClass["Recall"]),
    Specificity = sapply(threshold_results, function(cm) cm$byClass["Specificity"]),
    Precision = sapply(threshold_results, function(cm) cm$byClass["Precision"]),
    F1 = sapply(threshold_results, function(cm) cm$byClass["F1"]),
    row.names = paste0(thresholds * 100, "%")
  )
  tables <- lapply(threshold_results, function(cm) cm$table)
  names(tables) <- paste0(thresholds*100, "%")
  
   #Extract ROC for GGplot
  
  roc <- roc(test$data$donate, predictions$Yes, direction = "<",
             smooth = TRUE)
  ## CF
  raw <- predict(model, test$data, type = "raw")
  cf <- confusionMatrix(data = raw, 
                        reference =test$data$donate,
                       positive = "Yes")
  return(list(
    scores = scores,
    predictions = predictions,
    tables = tables,
    cf = cf,
    roc = roc
  ))
}

log_def_rs <- results(log_default)
saveRDS(log_def_rs, "log_def_rs")
rf_def_rs <- results(rf_default)
saveRDS(rf_def_rs, "ref_def_rs")



       