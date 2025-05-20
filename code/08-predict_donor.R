## -----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(grid)
library(gridExtra)
library(glmnet)
library(ranger)
library(tidymodels)
library(caret)
library(naniar)
library(pROC) 
library(doParallel)
library(future)
library(furrr)
library(mice)
library(DescTools)
library(plotly)
library(stargazer)
library(rpart)
library(rpart.plot)
library(BSDA)
options(scipen=999)



## -----------------------------------------------------------------------------------------------------------
original <- read_rds("data/cleaned_data.rds")
clean_names <- function(x) {
  x <- gsub("state", "State: ", x)
  x <- gsub("employ", "Employ: ", x)
  x <- gsub("income", "Income Range  ", x)
  x <- gsub("Yes", "", x)
  x <- gsub("parentYes", "Parent", x)
  x <- gsub("race", "Race: ", x)
  x <- gsub("party", "Party ID: ",x)
  x <- gsub("educ", "Education: ", x)
  x <- gsub("ideology", "Ideology: ", x)
  x <- gsub("pol", "Political", x)
  x <- gsub("religion", "Religion: ",
            x)
  x <- gsub("relig_imp", "Religious Importance: ",x)
  x <- gsub("immigrant", "", x)
  x <- gsub("marriage", "Marital Status:", x)
  x <- gsub("interest", "interest: ", x)
  x <- gsub("_", " ", x)
  x <- gsub("\\.", "", x)
  x <- gsub("mil", "Military Affiliation", x)
  x <- gsub("voteprior", "Prior Voter", x)
  x <- gsub("politicalinterestet", "Political Interest ", x)
  x <- gsub("politicalactivities", "Political Activicites", x)
  x <- gsub("votereg", "Voter Registration", x)
  x <- StrCap(x, method = "word")
  return(x)
  }


## -----------------------------------------------------------------------------------------------------------


dummy <- dummyVars(~., data = original)
orig_dum <- as.data.frame(predict(dummy, newdata = original))
names(orig_dum) <- clean_names(names(orig_dum))
names(orig_dum)[84] <- "Employ Unemployed"
names(orig_dum)[143] <- "Income Range Over 150k"
names(orig_dum)[148] <- "Income Range Over 500k"


## -----------------------------------------------------------------------------------------------------------


cors <- cor(select(orig_dum, -Year),
            use = "na.or.complete")

cor_long <- as.data.frame(cors) %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation") %>%
  filter(Var1 !=Var2) %>%
  mutate(var = map2(Var1, Var2, ~ paste(sort(c(.x,.y)), sep = "_"))) %>%
  distinct(var, .keep_all = TRUE) %>%
  select(Var1, Var2, Correlation) %>%
  arrange(desc(abs(Correlation)))

factor_groups <- list(
  State = names(orig_dum)[1:52],
  Race = names(orig_dum)[53:59],
  Education = names(orig_dum)[60:65],
  `Marital Status` = names(orig_dum)[66:71],
  Ideology = names(orig_dum)[72:77],
  Employ = names(orig_dum)[80:88],
  `Party ID` = names(orig_dum)[89:96],
  Citizen = names(orig_dum)[103:105],
  `Political Interest` = names(orig_dum)[110:113],
  Religion = names(orig_dum)[114:125],
  `Religious Importance` = names(orig_dum)[126:129],
  Income = names(orig_dum)[132:148]
)

find_group <- function(x) {
  lookup <- unlist(lapply(names(factor_groups), function(g) {
      setNames(rep(g, length(factor_groups[[g]])), factor_groups[[g]])
    }))
  
  
return(lookup[as.character(x)])
}

cor_long_filtered <- cor_long %>%
  mutate(group1 = find_group(Var1),
         group2 = find_group(Var2)) %>%
  filter(group1 != group2) %>%
  select(Var1, Var2, Correlation)


## -----------------------------------------------------------------------------------------------------------
missing_vars <- miss_var_summary(original)
names(missing_vars) <- c("Variable", "Number", "Percentage")
missing_vars$Variable <- c("News Activites", "Contacted", "Donated", 
                           "Political Activities", "Income", 
                           "Ran for Office", "Investor", "Parent", 
                           "Political Interest", "Recognition", "Religion",
                           "Past Voter", "Home Owner", "Ideology",
                           "Party", "Marital Status", "Immigration Status",
                           "Intent to Vote", "Religion Importance",
                           "Employment Status", "Voter Registration",
                           "Military", "Education", "Race", "Year", "State",
                           "Gender", "Union", "Age")
missing_vars$Percentage <- as.numeric(missing_vars$Percentage)
missing_cases <- miss_case_summary(original) 
names(missing_cases) <- c("Row Number", "Number", "Percentage")






## -----------------------------------------------------------------------------------------------------------
train <- read_rds("data/train_small.rds")
test <- read_rds("data/test.rds")


## -----------------------------------------------------------------------------------------------------------
imputetime <- read_rds("data/timeimpute.rds")
logtime <- read_rds("data/timelog.rds")
logdeftime <- read_rds("data/timelog_def.rds")
rfdeftime <- read_rds("data/timerf_def.rds")
ml_rf_time <- read_rds("data/timerf.rds")

times <- list(
  impute = imputetime,
  ml_log = logtime,
  def_log = logdeftime,
  def_rf = rfdeftime,
  ml_rf = ml_rf_time
)
rm(imputetime, logtime, logdeftime, rfdeftime, ml_rf_time)


## -----------------------------------------------------------------------------------------------------------
ml_log <- read_rds("models/ml_log.rds")
ml_log_tune <- ml_log$bestTune
ml_log_coef <- coef(ml_log$finalModel, lambda = ml_log$finalModel$lambdaOpt)
raw_ml_log <- predict(ml_log, test$data, type = "raw")

results <- function(model) {
  predictions <- predict(model, test$data, type = "prob")
  
  thresholds <- seq(0.1, 0.9, by = 0.1)
  
  threshold_results <- lapply(thresholds, function(threshold) {
    pred <- factor(if_else(predictions$Yes > threshold, "Yes", "No"), levels = c("No", "Yes"))
    
    actual <- test$data$donate
    
    confusionMatrix(pred, actual, positive = "Yes")
  })
  
  # Extract scores
  scores <- tibble(
    Accuracy = sapply(threshold_results, function(cm) cm$overall ["Accuracy"]),
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


## -----------------------------------------------------------------------------------------------------------
ml_log_rs <- results(ml_log)
log_prams <- tibble(alpha = ml_log$results$alpha, lambda = ml_log$results$lambda,
     F1 = ml_log$results$F1)


 hyp<- plot_ly(log_prams, x = ~alpha, y = ~lambda, z = ~F1, type = "scatter3d",
        mode = "markers", color = ~F1, marker = list(size=5.5)) %>%
  layout(
    scene = list(
      camera = list(eye = list(x=2, y=0.85, z =0.4),
                    projection = "perspective"),
    xaxis = list(title = "Alpha"),
    title = list(text = "Hyperparameter Combination Performance"),
    yaxis = list(
      title = "Lambda",
      range = c(0, 0.5)
    )
  ))
 
 # Requires Plot.ly orca CLI tool, must install separately
 orca(hyp, "figs/log_hp_plot3d.png")
 rm(hyp, ml_log, log_prams)


## -----------------------------------------------------------------------------------------------------------
#| cache-lazy: FALSE
ml_rf <- read_rds("models/caret_modelrf.rds")
rf_tune <- ml_rf$bestTune
rf_tune$mtry <- (rf_tune$mtry/135)*27
rf_prams <- tibble(mtry = round((ml_rf$results$mtry/135)*27,0),
                   minnodesize = ml_rf$results$min.node.size,
                   splittrule = ml_rf$results$splitrule,
                   F1 = ml_rf$results$F1)
   hyp <- plot_ly(rf_prams, x = ~mtry, y = ~minnodesize, z= ~F1,
               type = "scatter3d", mode ="markers",
               color = ~splittrule, marker = list(size=6)) %>%
  layout(
    scene = list(
      camera = list(eye = list(x=2, y=1.5, z =0.8),
                    up = list(x = 0, y = 0, z = 1.5)),
    xaxis = list(title = "Mtry"),
    title = list(text = "Hyperparameter Combination Performance"),
    yaxis = list(
      title = "Min Node Size",
      range = c(0, 20)
    )
  ))
  # Requires Plot.ly orca CLI tool, must install separately 
orca(hyp, "figs/rf_hp_plot3d.png")
 rm(hyp, rf_prams)


## -----------------------------------------------------------------------------------------------------------
#| cache-lazy: FALSE
ml_rf <- read_rds("models/ml_rf.rds")
ml_rf_imp <- importance(ml_rf)
set.seed(1585)
raw_ml_rf <- predict(ml_rf, test$data, predict.all = FALSE,
                            num.trees = 1500, type = "response")
prob_ml_rf <- as.tibble(raw_ml_rf$predictions)

rf_roc <- roc(test$data$donate, prob_ml_rf$Yes, direction = "<",
              smooth = TRUE)
raw_ml_rf <- factor(if_else(as.vector(prob_ml_rf$Yes) >0.5,"Yes","No"), 
                    levels = c("No", "Yes"))

thresholds <- seq(0.1,0.9, 0.1)

 threshold_results <- lapply(thresholds, function(threshold) {
    pred <- factor(if_else(prob_ml_rf$Yes > threshold, "Yes", "No"), levels = c("No", "Yes"))
    
    actual <- test$data$donate
    
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



rf_cf <- confusionMatrix(raw_ml_rf, reference = test$data$donate,
                         positive = "Yes")
ml_rf_rs <- list(roc = rf_roc,
                 cf = rf_cf,
                 predictions = prob_ml_rf,
                 scores = scores,
                 tables = tables)
rm(rf_cf, rf_roc, prob_ml_rf, ml_rf, scores, tables)



## -----------------------------------------------------------------------------------------------------------
def_log <- read_rds("models/log_def.rds")
def_log_coef <- coef(def_log)
test$data$union <- if_else(test$data$union =="Yes", 1,0)

log_pred <- as.tibble(predict(def_log, test$data, type = "response"))

log_roc <- roc(test$data$donate, log_pred$value, direction = "<",
               smooth = TRUE)
raw_def_log <- factor(if_else(log_pred$value > 0.5, "Yes", "No"),
                      levels = c("No", "Yes"))

thresholds <- seq(0.1,0.9, 0.1)

 threshold_results <- lapply(thresholds, function(threshold) {
    pred <- factor(if_else(log_pred$value > threshold, "Yes", "No"), levels = c("No", "Yes"))
    
    actual <- test$data$donate
    
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



log_cf <- confusionMatrix(raw_def_log, reference = test$data$donate,
                         positive = "Yes")
def_log_rs <- list(roc = log_roc,
                 cf = log_cf,
                 predictions = log_pred$value,
                 scores = scores,
                 tables = tables)
rm(log_cf, log_roc, def_log, scores, tables)



## -----------------------------------------------------------------------------------------------------------
def_rf <- read_rds("models/rf_def.rds")
def_rf_imp <- importance(def_rf)
set.seed(1585)
pred_def_rf <- predict(def_rf, test$data, predict.all = FALSE,
                            num.trees = 1500, type = "response")
prob_def_rf <- as.tibble(pred_def_rf$predictions)

def_rf_roc <- roc(test$data$donate, prob_def_rf$Yes, direction = "<",
              smooth = TRUE)

raw_def_rf <- factor(if_else(as.vector(prob_def_rf$Yes) >0.5,"Yes","No"), 
                    levels = c("No", "Yes"))

thresholds <- seq(0.1,0.9, 0.1)

 threshold_results <- lapply(thresholds, function(threshold) {
    pred <- factor(if_else(prob_def_rf$Yes > threshold, "Yes", "No"), levels = c("No", "Yes"))
    
    actual <- test$data$donate
    
    confusionMatrix(pred, actual, positive = "Yes")
  })
  
  # Extract scores
  def_rf_scores <- tibble(
    Accuracy = sapply(threshold_results, function(cm) cm$overall["Accuracy"]),
    Recall = sapply(threshold_results, function(cm) cm$byClass["Recall"]),
    Specificity = sapply(threshold_results, function(cm) cm$byClass["Specificity"]),
    Precision = sapply(threshold_results, function(cm) cm$byClass["Precision"]),
    F1 = sapply(threshold_results, function(cm) cm$byClass["F1"]),
    row.names = paste0(thresholds * 100, "%")
  )
  def_rf_tables <- lapply(threshold_results, function(cm) cm$table)
  names(def_rf_tables) <- paste0(thresholds*100, "%")


def_rf_cf <- confusionMatrix(raw_def_rf, reference = test$data$donate,
                         positive = "Yes")
def_rf_rs <- list(roc = def_rf_roc,
                 cf = def_rf_cf,
                 predictions = prob_def_rf,
                 scores = def_rf_scores,
                 tables = def_rf_tables)
rm(def_rf_cf, def_rf_roc, prob_def_rf, def_rf, def_rf_scores, def_rf_tables,
   pred_def_rf)




## -----------------------------------------------------------------------------------------------------------
rocs <- c(0.8472,0.847, 0.8545, 0.8532)
names(rocs) <- c("Logit", "Tuned Logit", "Stock Random Forest",
                 "Tuned Random Forest")


## -----------------------------------------------------------------------------------------------------------
#for Mcnemar Test, 4 quantities, 
# Number of items correctly classified by both (x)
# number of items misclassified by both (y)
# number of items classified correctly by A but Not B (z)
# number of items classified correctly by B but not A  (q)
# H_0: z=q
# Chisq stat is (((z-q)-1)^2)/(q+z)

log_log <- tibble(data = test$data$donate, def_log = raw_def_log,
                  ml_log = raw_ml_log) %>%
  mutate(both_correct = if_else(data==def_log & data==ml_log, 1,0),
         both_wrong = if_else(data!=def_log & data!=ml_log,1,0),
         def_correct = if_else(data==def_log & data!=ml_log,1,0),
         ml_correct = if_else(data!=def_log & data==ml_log,1,0))


log_log_mat <- matrix(c(sum(log_log$both_correct),
                         sum(log_log$def_correct),
                         sum(log_log$ml_correct),
                         sum(log_log$both_wrong)),
                       ncol=2, byrow = TRUE, 
                       dimnames = list(c("Logit Correct", "Logit Incorrect"),
                                       c("Tuned Logit Correct","Tuned Logit Incorrect")))
log_log_test <- mcnemar.test(log_log_mat)
log_log_test <- tidy(log_log_test)


rm(log_log)


## -----------------------------------------------------------------------------------------------------------
ml_log_rf <- tibble(data = test$data$donate, ml_log = raw_ml_log,
                 ml_rf = raw_ml_rf) %>%
  mutate(both_correct = if_else(data==ml_log & data==ml_rf, 1,0),
         both_wrong = if_else(data != ml_log & data!= ml_rf, 1,0),
         log_correct = if_else(data==ml_log & data!=ml_rf, 1,0),
         rf_correct = if_else(data!= ml_log & data==ml_rf, 1,0))



log_rf_mat <- matrix(c(sum(ml_log_rf$both_correct),
                       sum(ml_log_rf$log_correct),
                       sum(ml_log_rf$rf_correct),
                       sum(ml_log_rf$both_wrong)), ncol=2, byrow = TRUE, 
                     dimnames = list(c(" RF Correct", "RF Incorrect"),
                                     c("Logit Correct", "Logit Incorrect")))
ml_log_rf_test <- tidy(mcnemar.test(log_rf_mat))



## -----------------------------------------------------------------------------------------------------------
def_log_rf <- tibble(data = test$data$donate, def_log = raw_def_log,
                 def_rf = raw_def_rf) %>%
  mutate(both_correct = if_else(data==def_log & data==def_rf, 1,0),
         both_wrong = if_else(data != def_log & data!= def_rf, 1,0),
         log_correct = if_else(data==def_log & data!=def_rf, 1,0),
         rf_correct = if_else(data!= def_log & data==def_rf, 1,0))



def_log_rf_mat <- matrix(c(sum(def_log_rf$both_correct),
                       sum(def_log_rf$log_correct),
                       sum(def_log_rf$rf_correct),
                       sum(def_log_rf$both_wrong)), ncol=2, byrow = TRUE, 
                     dimnames = list(c(" RF Correct", "RF Incorrect"),
                                     c("Logit Correct", "Logit Incorrect")))
def_log_rf_test <- tidy(mcnemar.test(def_log_rf_mat))



## -----------------------------------------------------------------------------------------------------------
rf_rf <- tibble(data = test$data$donate, ml = raw_ml_rf,
                def = raw_def_rf) %>%
    mutate(both_correct = if_else(data==ml & data==def, 1,0),
         both_wrong = if_else(data != ml & data!= def, 1,0),
         ml_correct = if_else(data==ml & data!=def, 1,0),
         def_correct = if_else(data!= ml & data==def, 1,0))
rm(raw_def_rf, raw_ml_rf,raw_def_log,raw_ml_log)

rf_rf_mat <- matrix(c(sum(rf_rf$both_correct),
                       sum(rf_rf$ml_correct),
                       sum(rf_rf$def_correct),
                       sum(rf_rf$both_wrong)), ncol=2, byrow = TRUE, 
                     dimnames = list(c("Tuned Correct", "Tuned Incorrect"),
                                     c("Stock Correct", "Stock Incorrect")))
rf_rf_test <- tidy(mcnemar.test(rf_rf_mat))


## -----------------------------------------------------------------------------------------------------------
tests <- rbind(log_log_test, rf_rf_test, ml_log_rf_test, def_log_rf_test)
tests <- tests %>% mutate(comp = c("Log-Log", "Rf-Rf", "ML-Log-RF", "Def-Log-RF"))



## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Glimpse of Missing Variables"
#| label: tbl-gliv
stargazer(as.data.frame(slice(missing_vars, c(1:5,24,26:29))), summary=FALSE, digits =2,
          header=FALSE,float = FALSE, font.size = "small", notes = "Top 5 and Bottom 5 Variables Sorted by Number of NA Values",
          notes.align = "l",
          table.placement = "H")


## -----------------------------------------------------------------------------------------------------------
#| output: TRUE
#| fig-cap: "Example Decision Tree"
#| label: fig-t
#| fig-width: 4
#| fig-height: 2.5
set.seed(5)
ex_tree_data <- sample_n(train$data, 1000)
set.seed(5)
ex_tree <- rpart(donate ~ state+age+recognition, data = ex_tree_data)
 rpart.plot(ex_tree, type = 0)


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Glimpse of Correlations within Predictors"
#| label: tbl-cor
stargazer(slice(as.data.frame(cor_long_filtered), 1:10), summary = FALSE, header = FALSE, 
          float = FALSE, digits = 2, notes = "Top 10 pairwise correlations among predictors",
          notes.align = "l", font.size = "small")


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Test Set Performance"
#| label: tbl-test

model <- c("Logit", "Tuned Logit", "Stock Random Forest", "Tuned Random Forest")

TP <- c(def_log_rs$cf$table[2,2], ml_log_rs$cf$table[2,2], def_rf_rs$cf$table[2,2], ml_rf_rs$cf$table[2,2])
TN <- c(def_log_rs$cf$table[1,1], ml_log_rs$cf$table[1,1], def_rf_rs$cf$table[1,1], ml_rf_rs$cf$table[1,1])
FP <- c(def_log_rs$cf$table[2,1], ml_log_rs$cf$table[2,1], def_rf_rs$cf$table[2,1], ml_rf_rs$cf$table[2,1])
FN <- c(def_log_rs$cf$table[1,2], ml_log_rs$cf$table[1,2], def_rf_rs$cf$table[1,2], ml_rf_rs$cf$table[1,2])

confusion <- tibble(Model = model, TP = TP, TN =TN, FP =FP, FN = FN,
                    Correct = TP+TN, Wrong = FN+FP, Total = nrow(test$data),
                    ROC = as.numeric(rocs))

confusion <- confusion %>% mutate(Accuracy = Correct/(Correct+Wrong),
                    Recall = TP/(FN+TP), 
                    Precision = (TP/(FP+TP)), 
                    F1 = (2*TP/(2*TP+FP+FN)))

confusion_long <- pivot_longer(confusion, cols = c(F1, ROC, Recall,
                                                   Precision,
                                                   Accuracy,
                                                   TP,TN,FP,FN,Correct,Wrong, Total),
                               names_to = "Metric") %>% 
  pivot_wider(names_from = Model)


stargazer(as.data.frame(confusion_long), summary = FALSE,
          digits = 4, header = FALSE, float = FALSE, font.size = "small",
          notes = "Performance Summary: F1, ROC, Recall, Precision, Accuracy are [0,1]. Others are counts",
          notes.align = "l")


## -----------------------------------------------------------------------------------------------------------
# Confidence Intervals for Accuracy Rates
CI <- tibble(Model = c("Logit", "Tuned Logit", "Stock Random Forest",
                       "Tuned Random Forest"),
             Accuracy = confusion$Accuracy,
             Lower = Accuracy-1.96*(sqrt(((confusion$Accuracy)*(1-(confusion$Accuracy))/confusion$Total))),
             Upper = Accuracy+1.96*(sqrt(((confusion$Accuracy)*(1-(confusion$Accuracy))/confusion$Total))))



## -----------------------------------------------------------------------------------------------------------
pvalues <- c(tests$p.value, 2*def_log_rs$cf$overall[["AccuracyPValue"]], 2*ml_log_rs$cf$overall[["AccuracyPValue"]],
             2*def_rf_rs$cf$overall[["AccuracyPValue"]], 2*ml_rf_rs$cf$overall[["AccuracyPValue"]])

names(pvalues) <- c(tests$comp, "Log Acc", "Mlog Acc", "Rf Acc", "MRF Acc")

pvalues <- p.adjust(pvalues, method = "holm")


## -----------------------------------------------------------------------------------------------------------
#| output: TRUE
#| fig-cap: "ROC Curves for All Models: Models Improve as ROC curve trends to left corner"
#| label: fig-roc
p1 <- ggroc(def_log_rs$roc)+
  theme_minimal()+
  geom_abline(slope = 1, intercept = 1, color = "blue", linetype = "dashed")+
  labs(x = "True Negative Rate",  y = "Recall",
       title = "Default Logit")+
  theme(text = element_text(family = "serif"))
p2 <- ggroc(ml_log_rs$roc)+ theme_minimal()+
   geom_abline(slope = 1, intercept = 1, color = "blue", linetype = "dashed")+
  labs(x = "True Negative Rate",  y = "Recall",
       title = "Tuned Logit")+
  theme(text = element_text(family = "serif"))
p3 <- ggroc(def_rf_rs$roc)+
  theme_minimal()+
  geom_abline(slope = 1, intercept = 1, color = "blue", linetype = "dashed")+
  labs(x = "True Negative Rate",  y = "Recall",
       title = "Stock Random Forest")+
  theme(text = element_text(family = "serif"))
p4 <- ggroc(ml_rf_rs$roc)+
    theme_minimal()+
  geom_abline(slope = 1, intercept = 1, color = "blue", linetype = "dashed")+
  labs(x = "True Negative Rate",  y = "Recall",
       title = "Tuned Random Forest")+
  theme(text =element_text(family = "serif"))
grid.arrange(p1,p2,p3,p4, ncol=2)


## -----------------------------------------------------------------------------------------------------------
rm(p1,p2,p3,p4)


## -----------------------------------------------------------------------------------------------------------
m <- matrix(c("A","B","C","D"), ncol = 2, byrow = TRUE,
            dimnames = list(c("Classifier 1 Correct", "Classifier 1 Wrong"),
                            c("Classifier 2 Correct", "Classifier 2 Wrong")))

s <- stargazer(m, summary = FALSE, header = FALSE, font.size = "small", float = FALSE)


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
cat("\\begin{center}", s, "\\begin{gather*}", "\\chi^2 = \\frac{(|B-C|-1)^2}{B+C}", "\\end{gather*}\\end{center}")



## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Top 15 Logit Coefficients"
#| label: tbl-cdl
def_log_coef <- stack(def_log_coef)
def_log_coef <- tibble(Variable = as.character(def_log_coef[,2]), Coefficient = def_log_coef[,1]) %>%
  mutate(`Odds Ratio` = exp(Coefficient), 
         `10%/90%` =0.1*0.9*Coefficient,
         `25%/75%` = 0.2*0.8*Coefficient, 
         `50%` = 0.5*0.5*Coefficient)
def_log_coef$Variable <- clean_names(def_log_coef$Variable)
def_log_coef$Variable[3] <- "Employ Unemployed"
def_log_coef$Variable[107] <- "Income Over 150k"
def_log_coef$Variable[106] <- "Income Over 500k"

def_log_coef <- def_log_coef %>%
  arrange(desc(abs(def_log_coef$`50%`)))
stargazer(as.data.frame(def_log_coef[2:16,c(1, 4:6)]), summary = FALSE, header = FALSE,
          digits = 4, font.size = "footnotesize", float = FALSE)


## -----------------------------------------------------------------------------------------------------------
def_rf_imp2 <- stack(def_rf_imp)
def_rf_imp <- tibble(Variable = as.character(def_rf_imp2$ind), Importance = def_rf_imp2$values) %>%
  arrange(desc(Importance))
def_rf_imp$Variable <- c("Political Activities", "Age", "Income Range", "Contacted",
                        "News Activities", "Political Interest", "Party ID",
                        "Recognition", "Education", "Investor", "Ideology", "Employment",
                        "Religion", "Vote Intent", "Religious Importance", "Marital Status",
                        "Homeowner", "Parent", "Male","Race", "State",
                        "Military Affiliation", "Voter Registration", "Ran for Office",
                        "Immigration Status", "Union", "Vote Prior")
def_rf_imp <- def_rf_imp %>%
  mutate(Number = round(nrow(train$data)*Importance, 0))


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Top 5 Stock Random Forest Importance"
#| label: tbl-idf


stargazer(as.data.frame(slice(def_rf_imp, 1:5)), summary = FALSE, header = FALSE,
          digits = 4, font.size = "small", float = FALSE)


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Full Summary Statistics Table"
#| label: tbl-sum
stargazer(as.data.frame(orig_dum[,53:length(orig_dum)]), summary = TRUE, header = FALSE, float = FALSE, notes = "Means for binary variables are proportions; Exludes State", font.size = "tiny")



## -----------------------------------------------------------------------------------------------------------
#| output: True
#| fig-cap: "Map of Survey Respondents"
#| label: fig-map
library(usmap)

states <- summary(original$state) %>% stack() %>%
  rename(state = ind)

plot_usmap(regions = "states", data = states)+
  scale_fill_gradient(high = "#24008c", low = "#ffffff")+
  theme(legend.position = "top",
        text = element_text(family = "serif"))+ labs(fill = "Number of Respondents")+
  guides(fill = guide_colorbar(barwidth = unit(10, "cm")))


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "All Missing Variables"
#| label: tbl-miss
stargazer(as.data.frame(missing_vars), summary=FALSE, digits =2,
          header=FALSE,float = FALSE, font.size = "small")



## -----------------------------------------------------------------------------------------------------------
#| output: TRUE
#| fig-cap: "Example Logit: Blue Lines represent probability thresholds, and red lines showcase the classification of each point at that threshold"
#| label: fig-l
#| fig-height: 3
#| fig-width: 3.5
set.seed(5)
x <- rnorm(100, mean= 0, sd =3)
y <- LogitInv(x, min =0, max =1)
z <- if_else(x> 0.7|x< -3.5, 1,0)
                      
 ggplot(,aes(x = x, y = y))+
  geom_line()+
  geom_point(aes(x =x, y =z))+
  scale_y_continuous(limits = c(0,1))+
  theme_minimal()+
  labs (x = "Example Predictor", y = "Example Response", title = "Example Logit Classifier")+
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue")+
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "blue")+
   geom_hline(yintercept = 0.75, linetype = "dashed", color = "blue")+
   geom_vline(xintercept= 0, linetype = "dashed", color = "red")+
   geom_vline(xintercept= -1.1, linetype = "dashed", color = "red")+
   geom_vline(xintercept= 1.1, linetype = "dashed", color = "red")+
   theme(
    text = element_text(family = 'serif'))
   

  



## -----------------------------------------------------------------------------------------------------------
#| output: TRUE
#| fig-cap: ""
#| label: fig-thresh

def_log_rs$scores$Model <- rep("Logit",nrow(def_log_rs$scores))
ml_log_rs$scores$Model <- rep("Tuned Logit",nrow(def_log_rs$scores))
def_rf_rs$scores$Model <- rep("Stock Random Forest",nrow(def_log_rs$scores))
ml_rf_rs$scores$Model <- rep("Tuned Random Forest",nrow(def_log_rs$scores))

all_scores <- rbind(def_log_rs$scores, ml_log_rs$scores, def_rf_rs$scores, ml_rf_rs$scores)
all_scores <- all_scores %>% 
  pivot_longer(cols = c(Recall, Precision, F1),
               values_to = "value", names_to = "Metric")
all_scores <- all_scores %>% 
  mutate(thresh = parse_double(paste(0,".", substr(row.names, 1,1), sep = "")))


ggplot(all_scores, aes(x = thresh, y = value, color = Metric))+
  geom_point()+
  facet_wrap(~Model)+
  labs(title = "Model Metrics Across Decision Thresholds", x = "Probability Threshold", y = "Metric",
       color = "Model")+
  theme_minimal()+
  theme(text = element_text(family = "serif"))




## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Logit Confusion Matrix"
#| label: tbl-lcf
cf_log <- as_tibble(def_log_rs$cf$table)
cf_log <- matrix(c(
    cf_log$n[4], cf_log$n[2], cf_log$n[3], cf_log$n[1])
    , byrow = TRUE, ncol=2, dimnames = list(
    c("Predict Yes", "Predict No"),
    c("Reference Yes", "Reference No")
  ))

stargazer(cf_log, summary = FALSE, header=FALSE, float = FALSE,
          font.size = "small")


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Tuned Logit Confusion Matrix"
#| label: tbl-mcf
cf_mlog <- as_tibble(ml_log_rs$cf$table)
cf_mlog <- matrix(c(
    cf_mlog$n[4], cf_mlog$n[2], cf_mlog$n[3], cf_mlog$n[1])
    , byrow = TRUE, ncol=2, dimnames = list(
    c("Predict Yes", "Predict No"),
    c("Reference Yes", "Reference No")
  ))
stargazer(cf_mlog, summary = FALSE, header=FALSE, float = FALSE,
          font.size = "small")




## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Stock Random Forest Confusion Matrix"
#| label: tbl-dcf
cf_drf <- as_tibble(def_rf_rs$cf$table)
cf_drf <- matrix(c(
    cf_drf$n[4], cf_drf$n[2], cf_drf$n[3], cf_drf$n[1])
    , byrow = TRUE, ncol=2, dimnames = list(
    c("Predict Yes", "Predict No"),
    c("Reference Yes", "Reference No")
  ))
stargazer(cf_drf, summary = FALSE, header=FALSE,float = FALSE,
          font.size = "small")


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Tuned Random Forest Confusion Matrix"
#| label: tbl-rcf
cf_mrf <- as_tibble(ml_rf_rs$cf$table)
cf_mrf <- matrix(c(
    cf_mrf$n[4], cf_mrf$n[2], cf_mrf$n[3], cf_mrf$n[1])
    , byrow = TRUE, ncol=2, dimnames = list(
    c("Predict Yes", "Predict No"),
    c("Reference Yes", "Reference No")
  ))
stargazer(cf_mrf, summary = FALSE, header=FALSE,float = FALSE)


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Logit and Tuned Logit Confusion Matrix"
#| label: tbl-h1

stargazer(log_log_mat, summary = FALSE, header=FALSE, float = FALSE,
          font.size = "small")


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Random Forest and Tuned Random Forest Confusion Matrix"
#| label: tbl-h2

stargazer(rf_rf_mat, summary = FALSE, header=FALSE, float = FALSE,
          font.size = "small")


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Default Logit and Random Forest Confusion Matrix"
#| label: tbl-h3

stargazer(def_log_rf_mat, summary = FALSE, header=FALSE, float = FALSE,
          font.size = "small")


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Tuned Logit and Random Forest Confusion Matrix"
#| label: tbl-h4

stargazer(def_log_rf_mat, summary = FALSE, header=FALSE, float = FALSE,
          font.size = "small")


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Stock Random Forest Importance"
#| label: tbl-idf2
stargazer(as.data.frame(def_rf_imp), summary = FALSE, header = FALSE,
          digits = 4, font.size = "small", float = FALSE)
      



## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Tuned Random Forest Importance"
#| label: tbl-imf
ml_rf_imp2 <- stack(ml_rf_imp)
ml_rf_imp <- tibble(Variable = as.character(ml_rf_imp2$ind),
                    Importance = ml_rf_imp2$values) %>%
  arrange(desc(Importance))
ml_rf_imp$Variable <- c("Political Activities", "Age", "Income Range", "Contacted",
                        "News Activities", "Political Interest", "Party ID",
                        "Recognition", "Education", "Investor", "Ideology", "Employment",
                        "Religion", "Religious Importance", "Marital Status",
                        "Homeowner", "Vote Intent", "Parent", "Male", "State", "Race",
                        "Military Affiliation", "Voter Registration", "Ran for Office",
                        "Immigration Status", "Union", "Vote Prior")

ml_rf_imp <- ml_rf_imp %>%
  mutate(Number = round(nrow(train$data)*Importance, 0))

stargazer(as.data.frame(ml_rf_imp), summary = FALSE, header = FALSE,
          digits = 4, font.size = "small", float = FALSE)
 


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Logit Marginal Effects"
#| label: tbl-cdl2
stargazer(as.data.frame(def_log_coef[, c(1, 4:6)]), summary = FALSE, header = FALSE,
          digits = 4, font.size = "tiny", float = FALSE,notes = "All Ommitted Variables are 0; Arranged by Absolute Value",
          notes.align = "l")


## -----------------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Tuned Logit Marginal Effects"
#| label: tbl-cml
ml_log_coef <- stack(as.matrix(ml_log_coef)[,78])
ml_log_coef <- tibble(Variable = as.character(ml_log_coef[,2]), Coefficient = ml_log_coef[,1]) %>%
  mutate(`Odds Ratio` = exp(Coefficient),
         `10%/90%` =0.1*0.9*Coefficient,
         `25%/75%` = 0.2*0.8*Coefficient, 
         `50%` = 0.5*0.5*Coefficient) %>%
  arrange(cols = `50%`, descending = TRUE)

ml_log_coef$Variable <- clean_names(ml_log_coef$Variable)
ml_log_coef$Variable[3] <- "Employ Unemployed"
ml_log_coef$Variable[136] <- "Income Over 150k"
ml_log_coef$Variable[135] <- "Income Over 500k"

ml_log_coef <- ml_log_coef %>%
  arrange(desc(abs(ml_log_coef$`50%`)))
stargazer(as.data.frame(ml_log_coef[1:99,c(1, 4:6)]), summary = FALSE, header = FALSE,
          digits = 4, font.size = "tiny", float = FALSE, notes = "All Ommitted Variables are 0; Arranged by Absolute Value;",
          notes.align = "l")



