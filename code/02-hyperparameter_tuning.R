
pathpack <- # Set Package Directory

## Run this first if needed, highly recommended to install packages before running script.

#install.packages(c("tidyverse", "caret", "future.batchtools",nloptr", "lme4", 
#"jomo", "mitml", "mice", "glmnet", "ranger", "pROC", "here", "doParallel", 
#"future", "furrr", "DescTools"), lib= pathpack, repos="http://cran.us.r-project.org")



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


train <- readRDS("train.rds")
test <- readRDS("test.rds")

# Weights
train$data <- train$data %>% mutate(weights = if_else(
  donate == "Yes",
  10 / sqrt(table(donate)[2]),
  10 / sqrt(nrow(train$data) - table(donate)[2])
))

print("weights done")


# Seeds
seed_f <- function(k, r, t) {
  seed_list <- vector("list", k + 1)
  for (i in 1:(k * r + 1)) {
    seed_list[[i]] <- round(runif(t, min = 0, max = 10000), 0)
  }
  return(seed_list)
}
set.seed(945743)
seeds_rf <- seed_f(k=10 , r = 5, t= 50)
set.seed(58546)
seeds_glm <- seed_f(k=10 , r = 5, t= 500)
print("seeds done")
# Custom Summary

f1_sum <- function(data, lev = NULL, model = NULL) {
  # confusion matrix
  cf <- confusionMatrix(data$obs, data$pred, positive = "Yes")
  
  TP <- cf$table[2, 2]
  FP <- cf$table[2, 1]
  TN <- cf$table[1, 1]
  FN <- cf$table[1, 2]
  
  # Calculations
  accuracy <- (TP+TN)/ (TN+FN+TP+FP)
  recall <- TP / (TP + FN) 
  specificity <- TN / (TN + FP)
  precision <- TP / (TP + FP)
  F1 <- (2*TP)/((2*TP)+FP+FN)
  
  # returning
  out <- c(Accuracy = accuracy,
           Recall = recall,
           Specificity = specificity,
           Precision = precision,
           F1 = F1)
  
  return(out)
}
print("f1 done")
# Model Evaluation Extraction 

results <- function(model) {
  predictions <- predict(model, test$data, type = "prob")
  
  thresholds <- seq(0.1, 0.9, by = 0.1)
  
  threshold_results <- lapply(thresholds, function(threshold) {
    pred <- factor(if_else(predictions$Yes > threshold, "Yes", "No"), levels = c("No", "Yes"))
    
    actual <- factor(test$data$donate, levels = c("No", "Yes"))
    
    confusionMatrix(pred, actual, positive = "Yes")
  })
  
  # Extract scores
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
  
  # Extract ROC for GGplot
  
  roc <- roc(test$data$donate, predictions$Yes, direction = "<",
             smooth = TRUE)
  ## CF
  raw <-  predictions <- predict(model, test$data, type = "raw")
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
gc()
print("training start")
# Train
## Lasso Log
cl <- makeForkCluster(24)
registerDoParallel(cl)
start <- Sys.time()
set.seed(39922)
model_log <- train(
  formula,
  data = train$data,
  method = "glmnet",
  family = "binomial",
  maximize = TRUE,
  metric = "F1",
  tuneLength = 500,
  weights = train$data$weights,
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    verboseIter = TRUE,
    returnResamp = "all",
    search = "random",
    savePredictions = "final",
    classProbs = TRUE,
    summaryFunction = f1_sum,
    seeds = seeds_glm,
    allowParallel = TRUE,
  )
)

write_rds(model_log, "modellog.rds")
time_log <- Sys.time()- start
write_rds(time_log, "timelog.rds")
print("glm done")
gc()
## RF
print("rf start")
start <- Sys.time()
set.seed(39922)
model_rf <- train(
  formula,
  data = train$data,
  method = "ranger",
  num.tree = 1500,
  maximize = TRUE,
  metric = "F1",
  importance = "permutation",
  tuneLength = 50,
  weights = train$data$weights,
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    verboseIter = TRUE,
    returnResamp = "all",
    search = "random",
    savePredictions = "final",
    classProbs = TRUE,
    summaryFunction = f1_sum,
    seeds = seeds_rf,
    allowParallel = TRUE,
  )
)

write_rds(model_rf, "modelrf.rds")
time_rf <- Sys.time()- start
write_rds(time_rf, "timerf.rds")
print("rf done")
stopCluster(cl)
gc()

# Compiling Results and Saving Environment

model_log <- readRDS("modellog.rds")
time_log <- readRDS("timelog.rds")

log_results <- results(model_log)
write_rds(log_results, "logresults.rds")

print("glm compiled")

rf_results <- results(model_rf)
write_rds(rf_results, "rfresults.rds")

print("rf compiled")
full_model <- list(train = train, test = test, 
                   mlog = model_log, rlog = log_results,
                   mrf = model_rf, rrf = rf_results, tlog = time_log,
                   trf = time_rf)

write_rds(full_model, "full_model.rds")

print("fully complete")



                   