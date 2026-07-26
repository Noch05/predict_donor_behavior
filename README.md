# Project Status
This project was initially conducted for American University's GOVT-424 Campaign Finance course.
From there, it was presented at the Southern Political Science Association's (SPSA) 2026 conference
in New Orleans. Journal submission was considered, but after reflecting on the numerous
methodological and scope issues and my change in research focus, I decided it would be more effort
than it's worth.

Despite this, I still believe this project was extremely important to my growth as a student and a
researcher. It was well received at SPSA, and many thought the topic was impressive, but I can't, in
good faith, place it on arXiv or submit it to a journal, given the issues present. Therefore, I've
decided to leave it here, with all of its issues intact and documented, to demonstrate my
understanding of them.


## Methodological Issues
As of now, I know of three distinct methodological issues that damage the credibility of the paper.
First, there is a data processing issue in file `00`. Faulty recoding results in `NA` values for data
that is actually present, which by my estimation reduces the effective size of the dataset by ~50k
respondents (after removing NA values from the dependent variable donate, the paper uses ~450k
observations when it should use ~500k). This likely also means that many more values were imputed
than necessary, although the original paper reported that the absolute volume of missing values was
small.


Second, Multiple Imputation was performed incorrectly. Multiple Imputation (MI) requires generating each imputation multiple times, creating $m$ datasets, estimating the model on each dataset separately, and then pooling the results using Rubin's Rules to account for variation from both estimation and imputation jointly.
This paper, by contrast, simply takes the median of the distribution of imputed values for each
missing value, creating a computationally expensive but effectively single (median) imputation. This
approach was chosen on computational grounds, but that justification doesn't really hold up. I used
50 imputations, but had MI been implemented correctly, I could have used only 5–10 imputations
instead, as it would have been perfectly defensible to hyperparameter tune on just one of the
imputed datasets and then use Breiman's 1-SE rule to choose the final model. Pooling variances also
isn't an issue even for machine learning metrics that lack typical standard error formulas, since
these could be bootstrapped.

This approach also hinges on the data being Missing at Random (MAR), which, given the non-response
bias present in surveys and the distribution of missingness by question in this dataset, is
implausible. A more defensible approach, given that most of the data is categorical, would be to
simply code NA values as their own "Missing" category, allowing the models to learn meaningful
variation from missingness (In fact, the `ranger` and `lightGBM` implementations actually support
`NA` values, but imputation was chosen to keep the logit on a fair playing field given that
any non-categorical missing variables that could not be coded as missing would have to be dropped) itself rather than relying on poor imputations.


Third, survey weights are not used when training the model, which matters given the systematic under- and oversampling in the survey sample. Since the goal of the paper is to produce a useful classifier, these weights should be used so the model is trained to reflect the true population (Americans), rather than the sample as collected.

## Scope Issues


Overall, the questions used from each survey are limited in scope and don't capture many of the
variables that would seem, from a common-sense perspective, important for predicting donations. I
claim in the paper that it is limited by which questions were asked in every wave of the survey,
which is why many policy-related questions had to be dropped. That characterization is only
partially true. It's true that many policy-related and roll-call vote questions vary by time period
and differ across survey waves, but it's still possible to work with this. I initially set these
aside due to time constraints while completing the paper for a course, given my other obligations
and coursework, and I don't pursue it further now simply because it isn't worth my time.

The approach would be: since the surveys ask many questions about policy preferences, roll-call votes,
and even ask respondents to guess their representative's, senator's, or governor's roll-call votes
(and who they are) on certain measures; these responses could be collapsed into lower-dimensional representations using
Item Response Theory, an autoencoder, or some form of PCA, capturing quantities like "political
knowledge", "political engagement" and "policy preferences" potentially better than the
self-reported versions. This would also allow them to be standardized across
survey waves. These low-dimensional representations would still be flawed due to all the biases
present in surveys but
would likely provide more signal about a person's policy preferences and general propensity to
donate than self-reported ideology or political activity measures alone. There exists a combined CES dataset,
with all years 2006 - 2025 ([Kuriwaki 2026](https://doi.org/10.7910/DVN/II2DB6)), but it does not aggregate all of the variables discussed here.

Additionally, the data could be merged with electoral data from House, Senate, presidential, and
state elections to construct electoral competitiveness measures that would provide useful additional
context.

## Main Conclusions

Despite all of these issues, I don't believe any of these changes would materially change the
results (F1 $\approx 0.6$ across all models), which is why I don't pursue them. Performance across all models is substantively the same,
even if statistically different, because of survey non-response (respondents cannot be forced to
answer questions) and the well-documented inadequacy
of self-reported responses for gauging real political preferences (There is a host of other biases
at play in the surveys; respondents can easily lie, lack the understanding to answer a question
properly, etc.).

The above scope changes and methodological fixes would make the work
more defensible, but in my view they simply aren't worth the time and effort required (AI could make
some of this trivial, but I wouldn't feel comfortable having it do all the work at this stage) given
that I think these initial results will not be changed by that much (or at least there will not be
some sudden divergence in each model's performance).




## Predicting Donor Behavior
This is a GitHub Repository for my research project: "Predicting Donor Behavior: Applying Machine Learning to Donor Classification"
This repo houses all the code that produced my final product.

The goal of this project was to build off of existing literature on causal relationships between various factors
and propensity to donate, by using machine learning to predict the outcome instead. I tested a simple logistic regression, a random forest, and LightGBM boosted trees.
The random forest and LightGBM models underwent hyperparameter tuning, while the logistic regression did not (having none to tune).
Ultimately, I find model performance across all models (F1 $\approx 0.6$), and I conclude that the noisy data and issues with survey responses hinder the models.

## Data

All the data in this project is pulled directly from each year of the Cooperative Election Study (CES),
and is pulled directly using the `Dataverse` API from Harvard's Dataverse Database system.

## Code Files

* `00-data_cleaning_424.R` gathers the data from the `Dataverse` API, cleans it, and saves it as .rds and .csv files
* `01-impute.R` imputes missing data via `mice`.
* `02a1-train_ranger_hpc.R` finds the optimal random forest model and saves the results of the hyperparameter tuning so the optimal model can be trained locally.
* `02a2-train_ranger_local.R` trains the optimal random forest and saves it to disk.
* `02b1-train_gbm.R`finds the optimal gradient-boosted tree model and saves the results of the hyperparameter tuning so the optimal model can be trained locally.
* `02b2-train_gbm_local.R` trains the optimal gradient-boosted tree and saves it to disk.
* `02c-train_glm.R` trains and saves the logistic regression model to disk.
* `03-performance_metrics.R` uses the model objects to calculate important summary statistics, like F1, precision, recall, etc., which are used in the paper.

## Replication

To replicate the project, each of these files needs to be executed in order based on their number label, so all of the `02` files can be run immediately preceding the completion of `01`, though `02a1` and `02b1` need to be run before `02a2` and `02b2`, respectively. The files `02a1` and `02b1` labelled with `_hpc` are computationally intensive, so might not run effectively on a personal machine. For reference, I used a High Performance Computer to run those scripts, utilizing a high number of threads to accelerate computations.

To replicate the remainder of the project, without running `02a1` and `02b1`, the `models/` directory holds `.rds` files containing the results of the hyperparameter tuning, which can be used to train the optimal models in `02a2` and `02b2`, respectively. Then, `03` can be run to gather all of the important statistics about each model. However, I also provide the output of `03` in the `envs/` directory as an `.RData` file, so it can easily be loaded in `R` to explore the results as well.

## License

The code for this project is licensed under GPLv3; see [License](LICENSE). Feel free to use it as a starting point for your own projects, with proper attribution to myself and all the creators of the packages used.
