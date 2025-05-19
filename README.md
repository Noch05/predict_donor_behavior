## Predicting Donor Behavior  
This is a GitHub Repository for my research project: "Predicting Donor Behavior: Applying Machine Learning to Donor Classification"  
This repo houses all the code that produced the final paper, the paper, and the figures presented.

## Data

All the data in this project is pulled directly from each year of the Cooperative Election Study (CES),
and is pulled directly using the `Dataverse` API from Harvard's Dataverse Database system.  

## Code Files

* `data_cleaning_424.R` gathers the data from the `Dataverse` API, cleans it, and saves it as .rds and .csv files
* `training.R` is a script which imputes missing data via `mice`, and trains and tunes the hyperparameters of each model. The script is written for an LSF scheduled HPC, and took 17 days to complete.   
* `comp_train.R` is a script with trains default versions of each model with not tuning, written for HPC but can be executed on a typical computer.  
* `rf_final.R` takes the optimal random forest hyperparameters from `training.R` and retrains the model in `Ranger` to take advantage of package features.  

## Figs

Contains all figures as `.png` files, and all tables as `.tex` which can be compiled in LaTeX, and `.txt` for easy viewing.

## Paper

Contains the `.qmd` file with the paper, a `.bib` file with citation information, and a `.R` file which will generate all figures used.  

## Important Notes for Replication

For all the training procedures, seeds have explicitly been set to ensure reproducibility, however, running the scripts still requires a HPC environment with a large amount of memory. 

Additionally, the model objects from which the figures and tables in the paper were created from are very large objects, that cannot be uploaded to GitHub, the `.qmd` file still has all the text for the paper, and the figs folder has all of the accompanying material if necessary. The model files will be made available upon individual request.

## Experimentation

`test_nn.R` and `test_xgboost.R` are experimental files, that are designed to run on personal machine. They sample from the larger dataset and test running the various models.
This is apart of expanding the project after the initial completion of the paper. Neural Networks will likely be added to a future analysis.
