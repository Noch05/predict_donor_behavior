
library(dplyr)
library(stringr)
library(forcats)
library(withr)
library(ggplot2)
library(tzdb)
library(readr)
library(tibble)
library(tidyr)
library(purrr)
library(lubridate)
library(backports)
library(doParallel)
library(future)
library(future.batchtools)
library(furrr)
library(nloptr)
library(lme4)
library(jomo)
library(mitml)
library(mice)
library(DescTools)

plan(batchtools_lsf, workers = 30)

data <- read_rds("data/cleaned_data.rds")

data <- data %>% filter(!is.na(donate))
data <- data %>% mutate(donate = factor(donate, levels = c(0, 1),
                                        labels = c("No", "Yes")),
                        male = factor(male, levels = c(0,1), labels = c("No", "Yes")),
                        votereg = factor(votereg, levels = c(0,1), labels = c("No", "Yes")),
                        union = factor(union, levels = c(0,1), labels = c("No", "Yes")),
                        parent = factor(parent, levels = c(0,1), labels = c("No", "Yes")),
                        contacted = factor(contacted, levels = c(0,1), labels = c("No", "Yes")),
                        ran_for_office = factor(ran_for_office, levels = c(0,1), labels = c("No", "Yes")),
                        vote_prior = factor(vote_prior, levels = c(0,1), labels = c("No", "Yes")),
                        vote_intent = factor(vote_intent, levels = c(0,1), labels = c("No", "Yes")),
                        investor = factor(investor, levels = c(0,1), labels = c("No", "Yes")),
                        own_home = factor(own_home, levels = c(0,1), labels = c("No", "Yes")),
                        mil = factor(mil, levels = c(0,1), labels = c("No", "Yes")),
)
print("Data Cleaned")

impute <- function(data, m) {
  mices <- futuremice(data, m, parallelseed= 23490, nnet.MaxNWts = 3000)
  
  imputed <- complete(mices, action = "long")
  
  where <- as_tibble(mices$where)[rep(seq_len(nrow(mices$where)), times = m), ]
  
  names(where) <- paste0(names(data),".log")
  
  imputed <- bind_cols(imputed, where)
  
  for (var in names(data)) {
    cv <- sym(var)
    
    cv_log <- sym(paste0(var,".log"))
    
    missing <- imputed %>% filter(!!cv_log == TRUE)
    
    if (nrow(missing) > 0) {
      missing <- missing %>%
        group_by(.id) %>%
        summarize(value = Mode(!!cv)
        )
      
      for (i in missing$.id) {
        data[i, cv] <- as.list(missing$value[missing$.id == i])[[1]]
      }
    }
    else {}
  }
  return(list(
    data = data,
    model = mices
  ))
}

set.seed(582373)
train <- sample_n(data, size = 0.8*nrow(data))
test <- setdiff(data, train)


print("Start imputing")
set.seed(52345)
train <- impute(train, m = 50)
write_rds(train, "train.rds")

print("train done")
set.seed(52345)
test <- impute(test, m = 50)
write_rds(test, "test.rds")