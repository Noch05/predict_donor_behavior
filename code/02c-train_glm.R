library(butcher)
library(tidymodels)
library(tidyverse)

train <- read_rds("data/train_data.rds")

model <- logistic_reg() |>
  set_engine(engine = "glm", family = "binomial") |>
  set_mode("classification")

formula <- recipe(donate ~ ., data = train)


wf <- workflow() |>
  add_model(model) |>
  add_recipe(formula) |>
  add_case_weights(weights)

fit <- fit(wf, train) |>
  butcher()

write_rds(fit, file = "models/logit.rds")
