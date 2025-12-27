## Predicting Donor Behavior  
This is a GitHub Repository for my research project: "Predicting Donor Behavior: Applying Machine Learning to Donor Classification"  
This repo houses all the code that produced the my final product.

The goal of this project was to build off of existing literature on causal relationships between various factors
and propensity to donate, by using machine learning to predict the outcome instead. I tested simple a logistic regression, a random forest, and LightGBM boosted trees.
The random forst and LightGBM models underwent hyperparameter tuning, while the logistic regression did not (having none to tune).
Ultimately I find for modes performance across all models, (F1 $\approx 0.6$), and I conclude that the noisy data and issues with survey responses hinder the models.

## Data

All the data in this project is pulled directly from each year of the Cooperative Election Study (CES),
and is pulled directly using the `Dataverse` API from Harvard's Dataverse Database system. 

## Code Files

* `00-data_cleaning_424.R` gathers the data from the `Dataverse` API, cleans it, and saves it as .rds and .csv files
* `01-impute.R` imputes missing data via `mice`.
* `02a1-train_ranger_hpc.R` finds the optimal random forest model, and saves the results of the hyperparameter tuning so the optimal model can be trained locally. 
* `02a2-train_ranger_local.R` trains the optimal random forest and saves it to disk.
* `02b1-train_gbm.R`finds the optimal gradient boosted tree model, and saves the results of the hyperparameter tuning so the optimal model can be trained locally.
* `02b2-train_gbm_local.R` trains the optimal gradient boosted tree and saves it to disk.
* `02c-train_glm.R` trains and saves the logistic regression model to disk.
* `03-performance_metrics.R` uses the model objects to calculate important summary statistics, like F1, precision, recall, etc., which are used in the paper.

## Replication

To replicate the project, each of these files needs to be executed in order based on their number label, so all of the `02` files can be run immediately preceding the completion of `01`, though `02a1` and `02b1` need to be run before `02a2` and `02b2` respectively. The files `02a1` and `02b1` labelled with `_hpc` are computationally intensive, so might not run effectively on a personal machine. For reference, I used a High Performance Computer to run those scripts, utilizing a high number of threads to accelerate computations. 

To replicate the remainder of the project, without running `02a1` and `02b1`, the `models/` directory holds `.rds` files containing the results of the hyperparameter tuning, which can be used to train the optimal models in `02a2` and `02b2` respectively. Then, `03` can be run to gather all of the important statistics about each model. However, I also provide the output of `03` in the `envs/` directory as an `.RData` file, so it can easily be loaded in `R` to explore the results as well. 

## License

The code for this project is licensed under GPLv3, see [License](LICENSE). Feel free to use it as a starting point for your own projects, with proper attribution to myself and all the creators of the packages used.


