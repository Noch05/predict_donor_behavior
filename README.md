## Predicting Donor Behavior  
This is a GitHub Repository for my research project: "Predicting Donor Behavior: Applying Machine Learning to Donor Classification"  
This repo houses all the code that produced the my final product.

The goal of this project was to build off of existing literature on causal relationships between various factors
and propensity to donate, by using machine learning to predict the outcome instead. I tested simple logistic regression
and random forest models, 1 from each class was regularized and tuned, while 1 maintained the default hyper parameters.
Ultimately I find for modes performance across all models, (F1 $\approx 0.6$), and I conclude that the noisy data and issues with survey responses hinder the models.

## Data

All the data in this project is pulled directly from each year of the Cooperative Election Study (CES),
and is pulled directly using the `Dataverse` API from Harvard's Dataverse Database system.  

## Code Files

* `00-data_cleaning_424.R` gathers the data from the `Dataverse` API, cleans it, and saves it as .rds and .csv files
* `01-impute.R` is a script which imputes missing data via `mice`.
* `02-hyperparameter_tuning.R` tunes the hyper parameters of each model using `caret`. 
* `03-train_default.R` is a script with trains default versions of each model with not tuning, written for HPC but can be executed on a typical computer with modifications.  
* `04-train_optimal_rf.R` takes the optimal random forest hyperparameters from `training.R` and retrains the model in `Ranger` to take advantage of package features.  
* `05-train_ranger_tidymodels.R` is a script that experiments with training random forests from the `tidymodels` framework.
* `06-train_xgboost.R` is a script that experiments with training boosted decision trees using `xgboost`.
* `07-train_mlp.R` is a script that uses a subset of data to train multi-layer perceptron using `tidymodels`, `Keras` and `TensorFlow`.
* `08-predict_donor.R` is an accompaniment to the final paper, hosts all the code used in the final aspects of the analysis, such as creating figures and tables.

## Paper

The full paper text is not available at this time, looking for a place to publish. However, if you have any questions or working on something similar feel free to reach email me at <noahochital@icloud.com>

The framework in `R` I used for this project is `caret` but to anyone starting any machine learning project with `R`, I would reccomend the use of `tidymodels` instead.
It's created by the same developer, is easier to use, while `caret` no longer gets updates.

Code files `05` through `07` are testing files, and they did not influence the results of my analysis or paper.
Prior to embarking on this project, I knew nothing of machine learning but by the end I wanted to test out more complicated models.
The `xboost` and `keras` scripts test these algorithms by assessing a random grid search, along with a Bayesian optimization routine after for fine tuning. Ultimately they don't provide enough of a performance increase to be worth taking the time and resources to redo the re-write project.
Although adding them would allow for a more holistic view of different types of machine learning models, but I simply do not have the time to fully rework the project, for what is likely to be no significant change in my final conclusions.

## Important Notes for Replication

Many of the code files are specifically written for LSF scheduled HPC machines, so getting them to work properly on other ones may require tweaking.
Additionally this also means don't try to run the files, with the full set of data, on a personal machine either, unless you have the resources to.
Training outright is not too expensive, but the Cross-Validation and hyperparameter tuning ballooned the number of models trained.

If you simply want to see the results free to reach out to me at <noahochital@icloud.com>, I have the finished model objects but they are far to large to put up in GitHub, along with the plots/figures.


## License

The code for this project is licensed under GPLv3, see [License](LICENSE). Feel free to use it as a starting point for your own projects, with proper attribution to myself and all the creators of the packages used.


