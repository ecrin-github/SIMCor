# SIMCor
SIMCor aims to establish a computational platform for in-silico development, validation and regulatory approval of cardiovascular implantable devices.

The R-Statistical Environment will consist of two major moduls:
a) Validation of virtual cohorts
b) Application of validated cohorts

Validation of virtual cohorts
For validation of virtual cohorts three approaches are foreseen:

a) Re-classification
Here the test and training set are identical. This type of validation gives usually biased results.

b) Cross-validation
For cross-validation a real data set is splitted into a training set and a test set. The tool to be developed should allow different rates of splitting (e.g., 70% training, 30% validation; 90% training, 10% validation) as well a replicability of splitting with the same rates. The model building is performed on the training set. Then virtual cohorts are generated from the model. The virtual cohorts generated are then validated on the test set.
The process is described in the figure:

![Picture1](https://github.com/ecrin-github/SIMCor/assets/131688360/f74d21d9-1f1b-4514-9ee4-5d4bec661aff)

In SIMCor this approach corresponds to self-validation. In the DOW it is stated: In this step a self-validation is performed based on data of two subsets of a real medical device clinical trial. The first subset (about 90% of data) is used to develop a virtual cohort, whereas the other 10% (i.e., the validation set) is used to evaluate whether the model-generated cohort is able to accurately reproduce the observed intervention statistics. The validation set is needed to avoid that simulations of the virtual cohort produce good statistics due to overfitting. When the virtual cohort cannot reproduce the validation set statistics, the virtual cohort can be improved based on the training set and then re-evaluated on the validation set (DOW, p.13).

c) independent testing
In this approach an independent dataset not used in model development is used for validation. In principle, the process is similar to cross validation now with the difference that the validation set has no link to the data set that was used for training (not a subsample). Critical for this approach is that the training set and the independent test set should have the same structure and variables, which needs to be taken into consideration in the development of this module.
The output of the validation size is a summary of the training set as well as the comparison between the model prediction and the test set.
In SIMCor this approach corresponds to cross-validation. In the DOW it is stated “In this cross-validation step, the effect measure of a virtual cohort that consists of statistically similar patients (e.g., age, comorbidities) is simulated and compared with the effect measure of the truly observed effect of the real clinical trial.” (DOW, p.13-14).
In addition, SIMCor uses patient-specific validation, answering the question whether the model captures the actual patient dynamics? In this step the model-derived metrics are validated on a patient-specific level to demonstrate that virtual simulations mimic real patient features. This kind of validation is not considered in the R-statistical environment.


Application of validated cohorts
In a second step validated cohorts can be applied in different scenarios:

a) One-group testing
Here the validated virtual cohort is analysed with respect to basic descriptive statistics. This covers different types of variables, such as prognostic factors, interventions, and outcomes. Possibilities for specific analyses of prognostic factors/risk factors should be included in the model (e.g., relative risk). Again, this module should foresee the assessment of variability when there are replications of different virtual cohorts or random sampling from one specific cohort. 

b) two-group comparison
The major use case for a two-group comparison is an in-silico trial comparing two medical devices. For the two-group design two validated cohorts with the same structure and variables are needed, only differing by the type of intervention. For the analysis it should be possible to specify interesting outcome variables. The analysis should include the usual descriptive statistics but also different statistical tests for comparing the two groups. Similar to the one-group testing assessment of variability should be possible by subsampling from the validated cohorts.

