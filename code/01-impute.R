# Set Proper Package Directory, if on a Personal System Delete
.libPaths("/home/no9857a-hpc/R-packages")
library(dplyr, lib.loc = "R-packages")
library(stringr, lib.loc = "R-packages")
library(forcats, lib.loc = "R-packages")
library(withr,  lib.loc = "R-packages")
library(ggplot2,  lib.loc = "R-packages")
library(tzdb, lib.loc = "R-packages")
library(readr, lib.loc = "R-packages")
library(tibble, lib.loc = "R-packages")
library(tidyr, lib.loc = "R-packages")
library(purrr, lib.loc = "R-packages")
library(lubridate, lib.loc = "R-packages")
library(backports, lib.loc = "R-packages")
library(doParallel, lib.loc = "R-packages")
library(future, lib.loc = "R-packages")
library(future.batchtools, lib.loc = "R-packages")
library(furrr, lib.loc = "R-packages")
library(nloptr, lib.loc = "R-packages")
library(lme4, lib.loc = "R-packages")
library(jomo, lib.loc = "R-packages")
library(mitml, lib.loc = "R-packages")
library(mice, lib.loc = "R-packages")
library(DescTools, lib.loc = "R-packages")

plan(batchtools_lsf, workers = 30)

data <- read_rds("cleaned_data.rds")

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

impute <- function(data, m, cores) {
  mices <- futuremice(data, m, future.plan = "cluster",
                      n.cores = cores, parallelseed= 23490, nnet.MaxNWts = 3000)
  
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
train <- impute(train, m = 50, cores = 30)
write_rds(train, "train.rds")

print("train done")
test <- impute(test, m = 50, cores = 30)
write_rds(test, "test.rds")