
![image](https://github.com/ecrin-github/SIMCor/assets/131688360/1843df53-556f-4523-9b96-13e594e6f0a1)





1.	General aspects

Validation and application of virtual cohorts is related to the Context of Use (CuO) and the Question of Interest (QoI). Definitions for these terms are taken from the FDA Guidance on “Assessing the credibility of computational modelling and simulation in medical device submissions”.

COU: “a statement that defines the specific role and scope of the computational model used to address the question of interest”
QoI: “the specific question, decision, or concern that is being addressed”

Another term of relevance in this context is Quantity of interest (QoI):
Quantity of interest: “the calculated or measured result from a computational model or comparator, respectively. 
Example:
COi: “Combine computational modeling predictions and empirical fatigue testing observations to estimate device fatigue safety factors under anticipated worst-case radial loading conditions,”
QoI: “Is the device resistant to fatigue fracture under anticipated worst case radial loading conditions?”
For models used in silico device testing or in silico clinical trials, the COU should describe how the model will be used in a simulation study to address the QoI. The QoI defines the specific and concrete question related to the CoU. As such CoU and CoI are prerequisites for any kind of validation or application activity directed at virtual cohorts or in silico trials. 

CoU and QoI belong to the metadata of a virtual cohort and should be stored with other metadata in the VRE. So, the CoU and the QoL are already predefined when importing the virtual cohort dataset into R-statistical environment. CoU and QoI are essential elements for the scope and role of the computational model and the specific question addressed. This information is recorded as text and linked to the metadata of the virtual cohort. If no information is documented for CoU and QoI, this will be marked as “missing” in the R-statistical environment. The analytical techniques to validate and apply specific virtual cohorts are  not directly linked to the CoU and the QoI  but this information is included at the beginning of the statistical report to allow proper interpretation of the statistical results.   

2.	Validation of virtual cohorts

Validation is the “the process of determining the degree to which a model or a simulation is an accurate representation of the real world. This is different from verification, which is defined as “the process of determining that a computational model accurately represents the underlying mathematical model and its solution from the perspective of the intended uses of modelling and simulation”. (1) 
The R-statistical environment does not deal with verification and is only concerned with validation and applicability of virtual cohorts.
Validation is generally demonstrated by comparing the computational model predictions with the results from the comparator (2). Here, the comparator is defined as the test data that are used for validation, which may be data from bench testing or in vivo studies. In the R-statistical environment only in vivo studies (clinical studies or animal experiments) will be covered. 
Model calibration evidence is not validation evidence because it is not testing of the final model against data independent of model development; instead, model parameters are calibrated (whether optimized or manually tuned) to minimize the discrepancy between model results and data (1). 
Validation in the R-statistical environment covers different analytical techniques of comparison between real and synthetic data. Prerequisite for any validation activity is similarity of the structure of the virtual cohort and the real data set.
The measured QoIs of the validation activities are not always identical to the QoIs for the CoU because the QoIs for the CoU are not always directly measurable, might not be measured without unduly perturbing the intended test conditions, and/or might not be obtained within acceptable ranges of uncertainty and error. Therefore, the measured QoIs of the validation activities may be surrogates for the QoIs for the CoU, with varying degree of applicability (2). 

Therefore, the QoI for the validation activities may be different from the general QoI. The QoI for the validation activities must be selected from the variables characterising the imported dataset (one or more variables are possible) and has links to some of the statistical techniques to be applied in the R-statistical environment. It is documented together with the statistical analysis scripts and the results of the analysis and is included in the report. 

2.1 	Univariate (marginal) distributions of the variables between real and synthetic data

From the imported data sets all, several or one specific variable are selected and separately for the virtual and real datasets descriptive statistics are calculated and presented:
•	Mean value, median value, standard deviation, interquartile range, range for quantitative variables
•	Frequencies for qualitative variables
•	Scatter plots of combinations of variables
The results are presented as tables with variables as rows and virtual and real data metrics as columns. In addition, the results are shown as box plots.
2.2 	Bivariate correlations between variables of real and synthetic data
Here, separately for the real and synthetic dataset, bivariate correlations between the variables are calculated. The idea is to compare the correlations within the two cohorts. 

•	Spearman correlation coefficients between all features (separately for real and virtual data)

The results are graphically displayed as so-called heatmaps. Correlation heatmaps are a type of plot that visualize the strength of relationships between numerical variables. Correlation plots are used to understand which variables are related to each other and the strength of this relationship (6).
2.3	Multivariate comparison of variables characterising the real and synthetic data
To evaluate the compatibility of the virtual cohort with the real data, a multivariate comparison between the n-dimensional distributions of the features of both cohorts can be performed in the R-statistical environment. The following test will be used:

•	Quantile-Quantile plot between the synthetic and the real data after multivariate standardization of each data sets. Multivariate standarization is performed by 1) subtracting the vector of means to each vector data point and 2) scaling by using the inverse of the variance covariance matrix. The resulting standardized quantity is a quadratic form. 

2.3 	Analytical techniques taking uncertainties into consideration

In this validation approach, the stochastic results of the model (virtual dataset) and experiment (real dataset) are plotted together on a cumulative density function (CDF) plot for a variable of interest (3). The uncertainties in the model (due to input uncertainties and numerical uncertainties) and experiment (due to measurement system uncertainty and specimen-to-specimen variability) are represented in the two distributions. Any discrepancy in the two curves is therefore considered to be a manifestation of model form uncertainty. The uncertainty of the model is represented as an area around the cumulative density function and compared with the cumulative density function of the test data set. The area metric is defined by the area between the model (and its uncertainty) and experimental results from the real dataset.
In the R-statistical environment the generation of bootstrap samples to get an estimate of the variability and uncertainty is proposed. This is done via resampling of the virtual data and by comparing the distributions generated with the real data set. Here, 95% confidence bounds for the density function of the virtual data are calculated. A bootstrap p − value can be calculated from the number of times that the density of the real data is out of the 95% confidence bounds of the bootstrap analysis. The results are graphically displayed as (cumulative) density functions. 

2.4 	Predictive model performance

The development of predictive models plays a major role in the application of virtual cohorts. Example: A manufacturer develops a computational model-based tool that predicts if a patient will respond positively to proposed therapy and validates the predictive capability of the tool by performing a clinical trial and computing adequate statistical measures. To support the development of such models, a dependent variable from the list of all variables in the dataset must be specified and a predictive model based on independent variables from the dataset is constructed. Here different techniques can be used, such as logistic regression or multiple linear regression. In R it is possible to cover different techniques with one function (generalized linear models: glm() function R). The development of predictive models is not a task delt within the R-statistical environment but is performed by virtual cohort designers. 

Of interest in the R-statistical environment is the validation of predictive models. This aspect deals with individual-level comparisons between model predictions and an independent clinical dataset (1). In the first version of the R-statistical environment this kind of analysis is not included. If additional resources will be available, the extension of the package to this kind of analysis will be taken into consideration.   


3.	Application of validated cohorts in in silico clinical trials

Applicability is defined as “ the relevance of a credibility assessment activity (e.g., validation activities) to support the use of the computational model for a context of use” (1). The applicability of the validation activities is governed by two factors: the relevance of the QoI used in the validation  to the QoIs of the CoU, and the relevance of the validation conditions relative to those of the CoU (2). 

The applicability is given, provided the relevance of the QoI as well as the relevance of the validation activities on the CoU has been shown (4) or in other words, whether the validation evidence provided are relevant within the CoU of the model (5). Applicability is a prerequisite for application of validated cohorts in silico clinical trials.

But even when there are enough data to achieve sufficient statistical power, a more general problem of applicability remains. To be useful, a model should be able to make predictions for input values that are different from those used to assess its accuracy; but we do not know the predictive accuracy of the model for those new inputs. Considerations on the general regularity of physical quantities, and about the assumption that model accuracy should degrade smoothly in the sense that predictions made for similar inputs should present similar predictive accuracy, allow to assume that a degree of extrapolation is possible, in the sense that the model can be considered reliable even when used to predict for inputs different from those observed in the clinical validation cohort.

The following analytical techniques are applied in the R-statistical environment: 

3.1	One-group assessment

Here only one validated virtual cohort is analysed with respect to univariate descriptive statistics. This covers different types of variables, such as prognostic factors, interventions, and outcomes. From the imported virtual cohort all, several or one specific variable are selected, and the following descriptive statistics are calculated and presented:
•	Mean value, median value, standard deviation, interquartile range, range for quantitative variables
•	Confidence intervals for individual variables
•	Frequencies for qualitative variables
The results are presented as tables with variables as rows and descriptive statistics in columns. In addition, the results are shown as box plots.
In addition, an assessment of the variability will be performed  by enabling random sub-sampling from the virtual cohort. The results from this assessment are presented separately for each subsample and are summarised in a graphical display.
Bivariate dependencies between certain variables in the virtual cohort can be analysed with the following methods:
•	Spearman correlation coefficients between selected features
•	Relative risk (for certain outcomes)
Optionally, if resources are available, this will be completed by the possibility to perform multivariate analysis for prediction of outcome variables.
Here the "generalized linear model" glm() function R will be used. This function covers, binary outcomes (logistic regression), continues outcomes (multiple linear regression), counting outcomes (Poisson regression) and so on. So, several scenarios are covered with one function. In addition, step-wise variable selection can be implemented (e.g. function stepAIC() in R).

3.2	Two-group comparison

The major use case for a two-group comparison of validated virtual cohorts is an in-silico trial comparing two medical devices. For the two-group design two validated virtual cohorts with the same structure and variables are needed, only differing by the type of intervention. For the analysis it should be possible to specify interesting outcome variables. The analysis should include the usual descriptive statistics but also different statistical tests for comparing the two groups. 
With respect to descriptive statistics, the following metrics are foreseen:
•	Mean value, median value, standard deviation, interquartile range, range for quantitative variables
•	Confidence intervals for individual variables
•	Frequencies for qualitative variables
The results are presented as tables with variables as rows and descriptive statistics in columns. Descriptive statistics is performed, and the results displayed separately per group. In addition, the univariate results are shown as box plots, again separately for both groups.

Furthermore, different statistical tests for comparing the two groups are included:
•	T-test, Wilcoxon-test for quantitative variables
•	Chi-square, Fisher-test for qualitative variables
The application should also provide possibilities to perform: 
•	Power considerations
•	Analysis of effect sizes
Dependent on the available resources, the following options for selection of clinical trial alternatives are considered:
•	Analysis method (superiority, non-inferiority, equivalence)
•	Hypothesis (2-tailed, 1-tailed)
•	Type 1 error
There are more aspects of clinical trials, which could also be of interest for in-silico clinical trials. These aspects must be discussed more in detail before an implementation in the R-statistical environment can be recommended and considered. These aspects cover:  
•	Trial design (fixed, adaptive, sequential)
•	Drop-out rate
•	Interim analysis/stop criteria
•	Analysis (intention to treat, per protocol)



References
1.	FDA: Assessing the credibility of computational modeling and simulation in medical device dubmissions. Draft guidance for industry (December 2021)
2.	ASME V&V 40-2018: Assessing credibility of computational modeling through verification and validation: Application to medical devices (2018)
3.	Bodner, J., Kaul, V.: A framework for in silico clinical trials for medical devices using concepts from model verification, validation, and uncertainty quantification (VVUQ). Proceedings of the ASME 2021, May 19-20, 2021)
4.	Aldieri A., Curelli, C., Szyszko, j. A., La Mattina, A.A., Viceconti, M.: Credibility assessment of computational models according to ASME V&V40: Application to the Bologna Biomechanical Computed Tomography solution. Computer methods and Program 240: 107727 (2023)
5.	Visconti, M., Juarez, M.A., Curelli, C. Pennisi, M.,Russo, G., Pappalardo, F.: Position paper: Credibiility of in silico trial technologies: A theoretical framing. IEEE Journal of Biomedical and Health Informatics 24, 2020
6.	Verstraeten, S., Hoeijmakersb, M., Pim Tonino, P.,  c, Jan Brüning, J., Capelli, C., van de
Vosse, F., Huberts, W.: Generation of synthetic aortic valve stenosis geometries for in silico trials (submitted, 2023)

