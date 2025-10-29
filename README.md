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
* `02a-train_ranger.R` trains and saves the optimal random forest model, along with the results of hyperparameter tuning to disk.
* `02b-train_gbm.R`trains and saves the optimal gradient boosted tree model, along with the results of hyperparameter tuning to disk.
* `02c-train_glm.R` trains and saves the logistic regression model to disk.
* `03-performance_metrics.R` uses the model objects to calculate important summary statistics, like F1, precision, recall, etc., which are used in the paper.

To replicate the project each of these files needs to be executed in order, (the 02 files can all be run directly after completing 01). However, 02b, and 02a, are computationally intensive, so they might not run effectively on your personal machine. For reference, I used a High Performance Computer to run those scripts, each taking around a day.

## License

The code for this project is licensed under GPLv3, see [License](LICENSE). Feel free to use it as a starting point for your own projects, with proper attribution to myself and all the creators of the packages used.


