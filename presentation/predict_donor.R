## ----------------------------------------------------------------------------------------------
library(tidyverse)
library(grid)
library(gridExtra)
library(ranger)
library(tidymodels)
library(naniar)
library(mice)
library(DescTools)
library(stargazer)
library(rpart)
library(rpart.plot)
library(BSDA)
library(rstatix)
library(vip)
library(here)
library(usmap)
library(kableExtra)
library(knitr)
library(xtable)
library(huxtable)

options(digits = 4)


## ----------------------------------------------------------------------------------------------
original <- read_rds(here("data/cleaned_data.rds")) |> select(-state, -year)
original <- map_dfr(
  original,
  ~ {
    if ("ordered" %in% class(.x)) {
      lvls <- levels(.x)
      .x <- factor(.x, ordered = FALSE)
      levels(.x) <- (lvls)
    }
    .x
  }
)


## ----------------------------------------------------------------------------------------------
dummys <- recipe(donate ~ ., data = original) |>
  step_unknown(all_nominal()) |>
  step_dummy(all_nominal(), naming = dummy_names) |>
  prep(data = original) |>
  bake(new_data = original) |>
  select(!contains("unknown"))

clean_names <- function(x) {
  x <- gsub("^state", "State: ", x)
  x <- gsub(" ^employ", "Employ: ", x)
  x <- gsub("^income", "Income Range  ", x)
  x <- gsub("^race", "Race: ", x)
  x <- gsub("^party", "Party ID: ", x)
  x <- gsub("^educ", "Education: ", x)
  x <- gsub("^ideology", "Ideology: ", x)
  x <- gsub("^pol", "Political ", x)
  x <- gsub("^religion", "Religion: ", x)
  x <- gsub("^relig_imp", "Religious Importance: ", x)
  x <- gsub("immigrant", "", x)
  x <- gsub("marriage", "Marital Status:", x)
  x <- gsub("interest", "interest: ", x)
  x <- gsub("_", " ", x)
  x <- gsub("\\.", ":", x)
  x <- gsub("mil", "Military Affiliation", x)
  x <- gsub("voteprior", "Prior Voter", x)
  x <- gsub("votereg", "Voter Registration", x)
  x <- gsub("X", "\\$", x)
  x <- str_replace(
    x,
    pattern = "(Income Range \\d+k)\\s+(\\d+k)",
    replacement = "\\1<\\2"
  )
  x <- StrCap(x, method = "word")
  x <- gsub("^ ", "", x)
  return(x)
}
names(dummys) <- clean_names(names(dummys))
names(dummys)[81] <- "Income Range Over 150k"
names(dummys)[85] <- "Income Range Over 500k"
names(dummys)[14] <- "Home Owner"


## ----------------------------------------------------------------------------------------------

cors <- cor(dummys, use = "na.or.complete", method = "spearman")

cor_long <- as.data.frame(cors) %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation") %>%
  filter(Var1 != Var2) %>%
  mutate(var = map2(Var1, Var2, ~ paste(sort(c(.x, .y)), sep = "_"))) %>%
  distinct(var, .keep_all = TRUE) %>%
  select(Var1, Var2, Correlation) %>%
  arrange(desc(abs(Correlation)))

groups <- c(
  "Race",
  "Education",
  "Marital Status",
  "Ideology",
  "Employ",
  "Party ID",
  "Citizen",
  "Political Interest",
  "Religion",
  "Religious Importance",
  "Income"
)
factor_groups <- map(
  groups,
  ~ {
    idx <- str_detect(names(dummys), .x)
    names <- names(dummys)[idx]
  }
)
names(factor_groups) <- groups


find_group <- function(x) {
  lookup <- unlist(
    map(
      names(factor_groups),
      ~ {
        setNames(rep(.x, length(factor_groups[[.x]])), factor_groups[[.x]])
      }
    )
  )
  group <- lookup[x]
  return(group)
}

cor_long_filtered <- cor_long %>%
  mutate(group1 = find_group(Var1), group2 = find_group(Var2)) %>%
  dplyr::filter(group1 != group2) %>%
  select(Var1, Var2, Correlation)


## ----------------------------------------------------------------------------------------------
missing_vars <- miss_var_summary(original)
names(missing_vars) <- c("Variable", "Number", "Percentage")
missing_vars$Variable <- c(
  "News Activites",
  "Contacted",
  "Donated",
  "Political Activities",
  "Income",
  "Ran for Office",
  "Investor",
  "Parent",
  "Political Interest",
  "Recognition",
  "Religion",
  "Past Voter",
  "Home Owner",
  "Ideology",
  "Party",
  "Marital Status",
  "Immigration Status",
  "Intent to Vote",
  "Religion Importance",
  "Employment Status",
  "Voter Registration",
  "Military",
  "Education",
  "Race",
  "Gender",
  "Union",
  "Age"
)
missing_vars$Percentage <- as.numeric(missing_vars$Percentage)
missing_cases <- miss_case_summary(original)
names(missing_cases) <- c("Row Number", "Number", "Percentage")


## ----------------------------------------------------------------------------------------------
load(here("envs/model_metrics.RData"))


## ----------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Glimpse of Missing Variables"
#| label: tbl-gliv
gliv <- as.data.frame(rbind(head(missing_vars, 5), tail(missing_vars, 5)))
stargazer(
  gliv,
  summary = FALSE,
  digits = 2,
  header = FALSE,
  float = FALSE,
  font.size = "small",
  notes = "Top 5 and Bottom 5 Variables Sorted by Number of NA Values",
  notes.align = "l",
  table.placement = "H"
)


## ----------------------------------------------------------------------------------------------
donor_pct <- round((sum(train$donate == "Yes") / nrow(train)) * 100, 2)
donor_weight <- as.numeric(round(train$weights[train$donate == "Yes"][[1]], 4))
non_donor_weight <- as.numeric(round(
  train$weights[train$donate == "No"][[1]],
  4
))


## ----------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Test Set Performance"
#| label: tbl-test

metrics$Metric[1:4] <- str_to_title(metrics$Metric)[1:4]
metrics$Metric[5] <- "ROC-AUC"
metrics$Metric[10:12] <- str_to_title(metrics$Metric)[10:12]
metrics <- metrics |>
  rename(
    "Logistic Regression" = logit,
    "Gradient Boosted Trees" = gbm,
    "Random Forest" = rf
  )


stargazer(
  as.data.frame(metrics),
  summary = FALSE,
  digits = 4,
  header = FALSE,
  float = FALSE,
  font.size = "small",
  notes = "Performance Summary: F1, ROC, Recall, Precision, Accuracy are [0,1]. Others are counts",
  notes.align = "l"
)

x <- rep("No", nrow(test)) |> as.factor()
levels(x) <- c("No", "Yes")
no_info <- accuracy_vec(test$donate, x)


## ----------------------------------------------------------------------------------------------
#| output: TRUE
#| fig-cap: "ROC Curves for All Models: Models Improve as ROC Curve Trend to left Corner"
#| fig-cap-location: top
#| label: fig-roc

grocs <- imap(
  rocs,
  ~ {
    .x |> mutate(model = .y)
  }
) |>
  bind_rows() |>
  mutate(selectivity = 1 - specificity) |>
  ggplot(aes(x = selectivity, y = sensitivity)) +
  geom_line(aes(color = model)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "dodgerblue4",
    linetype = "longdash",
    alpha = .6
  ) +
  labs(x = "Selectivity (1 - TNR)", y = "Recall (TPR)") +
  facet_grid(
    ~model,
    labeller = labeller(
      model = c(
        logit = "Logistic Regression",
        rf = "Random Forest",
        gbm = "Gradient Boosted Trees"
      )
    ),
    switch = "x"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.placement = "outside",
    axis.title = element_text(size = 11),
  )
grocs


## ----------------------------------------------------------------------------------------------
var_imp2 <- map2(
  var_imp,
  c("Logit", "Random Forest", "Gradient Boosted Trees"),
  ~ {
    .x$Variable[.x$Variable == "immigrant"] <- "Immigration Status"
    .x |>
      mutate(Variable = clean_names(Variable), Model = .y) |>
      rename(N = N_Importance)
  }
)
var_imp2 <- full_join(var_imp2$logit, var_imp2$rf, by = "Variable") |>
  full_join(var_imp2$gbm, by = "Variable") |>
  rename(
    "Importance Logit" = Importance.x,
    "Importance Random Forest" = Importance.y,
    "Importance Gradient Boosted Trees" = Importance
  ) |>
  select(Variable, starts_with("Importance"))


## ----------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Permutation Variable Importance by Model"
#| label: tbl-imp
stargazer(
  as.data.frame(var_imp2),
  summary = FALSE,
  header = FALSE,
  digits = 4,
  font.size = "footnotesize",
  float = FALSE
)


## ----------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Summary Statistics For Selected Varaibles, Excluding State"
#| label: tbl-sum
summ <- imap(
  dummys,
  ~ {
    tibble(
      Variable = .y,
      N = sum(!is.na(.x)),
      Mean = mean(.x, na.rm = TRUE),
      `St. Dev.` = sd(.x, na.rm = TRUE),
      Min = min(.x, na.rm = TRUE),
      Max = max(.x, na.rm = TRUE)
    )
  }
) |>
  bind_rows()

kable(summ) |> kable_styling(font_size = 5)


## ----------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "All Missing Variables"
#| label: tbl-miss
stargazer(
  as.data.frame(missing_vars),
  summary = FALSE,
  digits = 2,
  header = FALSE,
  float = FALSE,
  font.size = "small"
)


## ----------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Top 10 Pairwise Correlations within Predictors"
#| label: tbl-cor
cor <- as.data.frame(head(cor_long_filtered, 10))
stargazer(
  cor,
  summary = FALSE,
  header = FALSE,
  float = FALSE,
  digits = 2,
  notes.align = "l",
  font.size = "small"
)


## ----------------------------------------------------------------------------------------------
#| output: TRUE
#| fig-cap: "Example Decision Tree: Whether or not a student passes their class based on study hours per week and class attendance."
#| fig-subcap: "The tree makes several splits attempting to separate those who fail from those who passed. At the end each node shows the predicted class, the probability of the class being 'Pass', and the percentage of the data at each node."
#| label: fig-t
#| fig-cap-location: top
#| fig-width: 4
#| fig-height: 2.5
set.seed(5)
Study_Hours <- round(runif(1000, 0, 12)) + 1
Attends_Class <- rbinom(1000, 1, 0.6) == 1
Passed_Exam <- case_when(
  Attends_Class & Study_Hours >= 12 ~ rbinom(1000, 1, 0.9),
  Attends_Class & Study_Hours >= 6 & Study_Hours < 12 ~ rbinom(1000, 1, 0.7),
  Attends_Class & Study_Hours < 6 ~ rbinom(1000, 1, 0.4),
  !Attends_Class & Study_Hours >= 12 ~ rbinom(1000, 1, 0.6),
  !Attends_Class & Study_Hours >= 6 & Study_Hours < 12 ~ rbinom(1000, 1, 0.3),
  !Attends_Class & Study_Hours < 6 ~ rbinom(1000, 1, 0.1)
) ==
  1

ex_tree_data <- tibble(
  `Study Hours` = Study_Hours,
  `Attends Class` = Attends_Class,
  `Passed Class` = if_else(Passed_Exam, "Yes", "No")
)

ex_tree <- rpart(
  `Passed Class` ~ `Study Hours` + `Attends Class`,
  data = ex_tree_data,
  method = "class"
)
rpart.plot(ex_tree)


## ----------------------------------------------------------------------------------------------
#| output: 'asis'
#| label: tbl-param
#| tbl-cap: "Hyperparameter Ranges Tested"
params <- tibble(
  Model = c("Both", "Both", "Both", "LightGbm", "LightGbm", "LightGbm"),
  Hyperparameter = c(
    "Mtry",
    "Trees",
    "Min Node Size",
    "Tree Depth",
    "Learn Rate",
    "Loss Reduction"
  ),
  Range = c(
    "1 -- 27",
    "100 -- 3000",
    "1 -- 1000",
    "3 -- 25",
    "$10^{-10}$ -- $10^{−1}$",
    "$10^{-10}$  -- $10^{1.5}$"
  )
)

kable(params, escape = FALSE)


## ----------------------------------------------------------------------------------------------
#| output: TRUE
#| fig-cap: "Random Forest Hyperparameters Against F1 Score"
#| fig-subcap: "F1 score tends to increase as the minimum node size and mtry decrease, but has no clear relationship with the number of trees used. The optimal values with the highest F1-score are an mtry of 3, 898 trees, and a minimum node size of 8, occuring on iteration 2 of the Bayesian optimization procedure."
#| fig-cap-location: top
#| label: fig-rf
hyperparams$rf |>
  select(mtry:min_n, mean) |>
  pivot_longer(cols = !mean, names_to = "Hyperparameter") |>
  ggplot(aes(x = value, y = mean)) +
  geom_jitter(aes(color = Hyperparameter), alpha = 0.6) +
  facet_grid(
    ~Hyperparameter,
    scales = "free_x",
    switch = "x",
    labeller = labeller(
      Hyperparameter = c(min_n = "Min N", mtry = "Mtry", trees = "Trees")
    )
  ) +
  guides(color = "none") +
  labs(x = NULL, y = "F1") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.placement = "outside",
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(size = 12)
  )
best <- hyperparams$rf |> slice_max(order_by = mean, n = 1)


## ----------------------------------------------------------------------------------------------
#| output: TRUE
#| fig-cap: "Gradient Boost Trees Hyperparameters Against F1 Score"
#| fig-cap-location: top
#| fig-subcap: "The F1 score sharply increases with the learning rate, but otherwise has no clear relationship with any of the other hyperparameters. This is likely because the true relationship depends on an interaction of 2 or more hyperparameters which cannot be showcased in the plot. The optimal values are 25 for mtry, 2992 trees/boosting iterations, minimum node size of 346, tree depth of 24, learning rate of 0.0425, and a loss reduction of $2*10^{-10}$, occuring at iteration 47 of the Bayesian optimization procedure."
#| label: fig-gbm

hyperparams$gbm |>
  select(mtry:loss_reduction, mean) |>
  pivot_longer(cols = !mean, names_to = "Hyperparameter") |>
  ggplot(aes(x = value, y = mean)) +
  geom_jitter(aes(color = Hyperparameter), alpha = 0.6) +
  facet_wrap(
    ~Hyperparameter,
    scales = "free_x",
    nrow = 2,
    labeller = labeller(
      Hyperparameter = c(
        min_n = "Min N",
        mtry = "Mtry",
        trees = "Trees",
        tree_depth = "Tree Depth",
        learn_rate = "Learn Rate",
        loss_reduction = "Loss Reduction"
      )
    )
  ) +
  guides(color = "none") +
  labs(x = NULL, y = "F1") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.placement = "outside",
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(size = 12)
  )


## ----------------------------------------------------------------------------------------------
best <- hyperparams$gbm |> slice_max(order_by = mean, n = 1)


## ----------------------------------------------------------------------------------------------
#| output: TRUE
#| fig-cap: "Recall, Precision, and F1 Score Across Prediction Thresholds"
#| fig-subcap: "Recall and Precision show monotonic relationships with the prediction threshold. As the threshold lowers, predictions become more liberal, capturing more donors, but also many false positives. As the threshold increases, predictions become more selective capturing less donors, but also less false positives. F1 on the other hand shows a parabolic path, with it reaching the maximum around the point the recall and precision curves meet, showcasing the balance of the two other metrics."
#| label: fig-thresh
#| fig-cap-location: top
#| fig-width: 6.5
thresholds |>
  rename(Precision = precision, Recall = recall) |>
  pivot_longer(cols = F1:Precision, names_to = "metric", values_to = "value") |>
  ggplot(aes(x = threshold, y = value, color = metric)) +
  geom_line(linewidth = 1.5, alpha = 0.6) +
  facet_grid(
    ~model,
    switch = "x",
    labeller = labeller(
      model = c(
        logit = "Logitistic Regression",
        gbm = "Gradient Boosted Trees",
        rf = "Random Forest"
      )
    )
  ) +
  labs(x = "Probability Threshold", y = "Metric", color = "Metric") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    axis.title = element_text(size = 11),
    panel.spacing = unit(1, "lines")
  )


## ----------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Logit Confusion Matrix"
#| label: tbl-lcf
#| tbl-pos: "H"

cf_log <- as_tibble(conf_mats$logit$table)
cf_log <- matrix(
  c(
    cf_log$n[4],
    cf_log$n[2],
    cf_log$n[3],
    cf_log$n[1]
  ),
  byrow = TRUE,
  ncol = 2,
  dimnames = list(
    c("Predict Yes", "Predict No"),
    c("Truth Yes", "Truth No")
  )
)

stargazer(
  cf_log,
  summary = FALSE,
  header = FALSE,
  float = FALSE,
  font.size = "small",
  table.placement = "H"
)


## ----------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Random Forest Confusion Matrix"
#| label: tbl-dcf
#| tbl-pos: "H"
cf_drf <- as_tibble(conf_mats$rf$table)
cf_drf <- matrix(
  c(
    cf_drf$n[4],
    cf_drf$n[2],
    cf_drf$n[3],
    cf_drf$n[1]
  ),
  byrow = TRUE,
  ncol = 2,
  dimnames = list(
    c("Predict Yes", "Predict No"),
    c("Truth Yes", "Truth No")
  )
)
stargazer(
  cf_drf,
  summary = FALSE,
  header = FALSE,
  float = FALSE,
  font.size = "small",
  table.placement = "H"
)


## ----------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Gradient Boosted Trees Confusion Matrix"
#| label: tbl-gcf
#| tbl-pos: "H"
cf_mrf <- as_tibble(conf_mats$gbm$table)
cf_mrf <- matrix(
  c(
    cf_mrf$n[4],
    cf_mrf$n[2],
    cf_mrf$n[3],
    cf_mrf$n[1]
  ),
  byrow = TRUE,
  ncol = 2,
  dimnames = list(
    c("Predict Yes", "Predict No"),
    c("Truth Yes", "Truth No")
  )
)
stargazer(
  cf_mrf,
  summary = FALSE,
  header = FALSE,
  float = FALSE,
  table.placement = "H",
  font.size = "small"
)

save.image(file = here("envs/presentation_env.RData"))
