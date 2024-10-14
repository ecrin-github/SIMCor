Get Started with SIMCor Shiny App

Prerequisites
Before you begin, ensure you have met the following requirements:
R and RStudio: You need to have R and RStudio installed on your computer. If you haven’t installed them yet, you can download R here and RStudio here.
Shiny Package: This app is built using the Shiny framework for R. Install Shiny in RStudio by running install.packages("shiny").

Installation
To run the SIMCor Shiny app locally, follow these steps:

Clone the repository: Clone this GitHub repository to your local machine or download it as a ZIP file and extract it.
Open the project: Open the RStudio project file (if provided) or simply open the app.R file in RStudio.
Install Dependencies: Run the following command in RStudio to install all the required packages:
install.packages(c("shiny", "readxl", "data.table", "ggplot2", "plotly", "reshape2", "corrplot", "GGally", "boot", "dplyr", "shinydashboard"))
Load the app: In RStudio, open the server.R and ui.R files (or the app.R file if the application is contained in a single file).

Running the App
To start the app, click on the 'Run App' button in RStudio's top right corner of the script editor, or run shiny::runApp() in the console.

Data Requirements
This app requires specific datasets to function correctly:
Virtual and Real Datasets: The app analyzes and compares virtual and real datasets. Ensure your datasets are in CSV format.
Dataset Structure: The datasets should have a specific structure. Include a sample or template CSV file for reference, if possible.

Using the App
Navigation: The app has multiple modules for validation, analysis, and comparison of virtual and real datasets. Navigate through these modules using the tabs and buttons provided.
Uploading Data: Use the file upload inputs to load your datasets into the app.
      Reference: In a study by Verstraeten et al. (2023), a virtual cohort generator was developed, generating anatomically plausible, synthetic aortic valve stenosis geometries for in silico TAVI trials ((https://pubmed.ncbi.nlm.nih.gov/37961993/).  To build the generator, a combination of non-parametrical statistical shape modelling and sampling from a copula distribution was used. The developed virtual cohort generator successfully generated 500 synthetic aortic valve stenosis geometries that were compared with a real cohort of 97 patients, resulting in validation of the virtual cohort. The dataset underlying the example is openly available at 4TU.ResearchData under the link https://data.4tu.nl/datasets/3f6a3788-96e6-4b81-b37b-f07eeec85965.

      
Interactive Analysis: Once your data is uploaded, you can interact with various plots and statistical tools to analyze and compare the datasets.




General Information:

· Name: SIMCor (In-Silico testing and validation of Cardiovascular Implantable devices) (to be fixed later)

· Creator: T. Khorchani (ECRIN), P.E. Verde (Heinrich Heine University and University Hospital Düsseldorf), C. Ohmann ((ECRIN)

· Version: v.0.1.0

· Release Date: [2023]


Overview:

SIMCor is an advanced Shiny application designed for statistical analysis, especially in validating and applying virtual cohorts. It is particularly useful in fields requiring rigorous data analysis and visualization, such as biomedical research, statistical modeling, and data science.

User Interface (UI):

· Styling: The UI adopts a design, enhanced with 'Poppins' Google font for improved readability and aesthetic appeal.

· Main Modules:

· Validation of Virtual Cohorts: Enables users to validate virtual cohort data: the process of determining the degree to which a model or a simulation is an accurate representation of the real world.

· Application of Validated Cohorts: Dedicated to applying validated cohort data in practical scenarios.

· Analysis: Provides comprehensive analysis tools, including univariate, bivariate, and multivariate comparisons, along with variability assessments.This work aims to assess the potential impact of the use of virtual cohorts and computer simulations on the “real world”, i.e., on the innovation process, the medical devices industry and the healthcare system.
Validation of virtual cohorts is related to the Context of Use (CuO) and the Question of Interest (QoI). Definitions for these terms are taken from the FDA Guidance on “Assessing the credibility of computational modelling and simulation in medical device submissions”.

COU: “a statement that defines the specific role and scope of the computational model used to address the question of interest”

QoI: “the specific question, decision, or concern that is being addressed”

CoU and QoI belong to the metadata of a virtual cohort and should be stored with other metadata in the VRE. So, the CoU and the QoL are already predefined when importing the virtual cohort dataset into R-statistical environment. CoU and QoI are essential elements for the scope and role of the computational model and the specific question addressed. This information is recorded as text and linked to the metadata of the virtual cohort. If no information is documented for CoU and QoI, this will be marked as “missing” in the R-statistical environment. The analytical techniques to validate and apply specific virtual cohorts are not directly linked to the CoU and the QoI but this information is included at the beginning of the statistical report to allow proper interpretation of the statistical results.

Functionalities:

· Data Import and Validation: Users can upload and validate both virtual and real datasets in CSV format.

· Data Analysis Tools: Includes tools for statistical analysis such as summary statistics, boxplots, heatmaps, and scatter plots.

· Interactive Visualizations: Leveraging ggplot2 and plotly for dynamic and informative visualizations.

· Report Generation: Capability to download various analytical reports in PDF format.

· Bootstrap Analysis: For advanced variability assessments and comparisons.

Usage:

· Intended Audience: Researchers, data analysts, and academicians in fields requiring data validation and analysis.

· Application Flow: Users start by uploading datasets, followed by choosing specific modules for data analysis and visualization.

· Output: Visual representations of data analysis, downloadable reports, and insights into the datasets.



References:

· Include references to different statistical methods, algorithms, or publications used within the application. For example:

· Verstraeten S, Hoeijmakers M, Tonino P, et al. Generation of synthetic aortic valvestenosis geometries for in silico trials.Int J Numer Meth Biomed Engng. 2023;e3778. doi:10.1002/cnm.3778VERSTRAETENET AL.21 of 21.

· Johnson R. A. and Wichern, D.W. (1992). Applied Multivariate Statistical Analysis. Prentice-Hall International Editions.

· Efron, B. & Tibshirani, R. (1993). An Introduction to the Bootstrap. Chapman & Hall.

· Tarek Haddad, Adam Himes, Laura Thompson, Telba Irony, Rajesh Nair & on Behalf of MDIC Computer Modeling and Simulation Working Group Participants (2017). Incorporation of stochastic engineering models as prior information in Bayesian medical device trials, Journal of Biopharmaceutical Statistics, 27:6, 1089-1103, DOI: 10.1080/10543406.2017.1300907.

Contact and Support:

· For user support, feedback, and further inquiries, please contact: takoua.khorchani@ecrin.org - takouakhorchani@gmail.com

Additional Notes:

· The application is user-friendly across different devices and screen sizes.

Conclusion:

SIMCor is built with the aim of providing a robust and user-friendly platform for data analysis and visualization. As we continue to improve and update the application, we welcome feedback and suggestions from our user community.

![image](https://github.com/ecrin-github/SIMCor/assets/131688360/1843df53-556f-4523-9b96-13e594e6f0a1)

![image](https://github.com/ecrin-github/SIMCor/assets/131688360/d498ce6e-c322-463c-8f0c-0eb8b852abcd)

![image](https://github.com/ecrin-github/SIMCor/assets/131688360/19f101fe-d1b3-4170-aca1-278766d27f66)

![image](https://github.com/ecrin-github/SIMCor/assets/131688360/6c36b545-1a49-4228-aaaf-6ce57caf6430)

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
•	Mean value, standard deviation, Max and Min
•	Scatter plots of combinations of variables
The results are presented as tables with variables as rows and virtual and real data metrics as columns. In addition, the results are shown as box plots.

2.2 	Bivariate correlations between variables of real and synthetic data
Here, separately for the real and synthetic dataset, bivariate correlations between the variables are calculated. The idea is to compare the correlations within the two cohorts. 

•	Spearman correlation coefficients between all features (separately for real and virtual data)

The results are graphically displayed as so-called heatmaps. Correlation heatmaps are a type of plot that visualize the strength of relationships between numerical variables. Correlation plots are used to understand which variables are related to each other and the strength of this relationship.

2.3	Multivariate comparison of variables characterising the real and synthetic data
To evaluate the compatibility of the virtual cohort with the real data, a multivariate comparison between the n-dimensional distributions of the features of both cohorts can be performed in the R-statistical environment. The following test will be used:

•	Quantile-Quantile plot between the synthetic and the real data after multivariate standardization of each data sets. Multivariate standarization is performed by 1) subtracting the vector of means to each vector data point and 2) scaling by using the inverse of the variance covariance matrix. The resulting standardized quantity is a quadratic form. 

2.3 	Analytical techniques taking uncertainties into consideration

In this validation approach, the stochastic results of the model (virtual dataset) and experiment (real dataset) are plotted together on a cumulative density function (CDF) plot for a variable of interest (3). The uncertainties in the model (due to input uncertainties and numerical uncertainties) and experiment (due to measurement system uncertainty and specimen-to-specimen variability) are represented in the two distributions. Any discrepancy in the two curves is therefore considered to be a manifestation of model form uncertainty. The uncertainty of the model is represented as an area around the cumulative density function and compared with the cumulative density function of the test data set. The area metric is defined by the area between the model (and its uncertainty) and experimental results from the real dataset.
In the R-statistical environment the generation of bootstrap samples to get an estimate of the variability and uncertainty is proposed. This is done via resampling of the virtual data and by comparing the distributions generated with the real data set. Here, 95% confidence bounds for the density function of the virtual data are calculated. A bootstrap p − value can be calculated from the number of times that the density of the real data is out of the 95% confidence bounds of the bootstrap analysis. The results are graphically displayed as (cumulative) density functions. 

2.4 	Predictive model performance

The development of predictive models plays a major role in the application of virtual cohorts. Example: A manufacturer develops a computational model-based tool that predicts if a patient will respond positively to proposed therapy and validates the predictive capability of the tool by performing a clinical trial and computing adequate statistical measures. To support the development of such models, a dependent variable from the list of all variables in the dataset must be specified and a predictive model based on independent variables from the dataset is constructed. Here different techniques can be used, such as logistic regression or multiple linear regression. In R it is possible to cover different techniques with one function (generalized linear models: glm() function R). The development of predictive models is not a task delt within the R-statistical environment but is performed by virtual cohort designers. 

Of interest in the R-statistical environment is the validation of predictive models. This aspect deals with individual-level comparisons between model predictions and an independent clinical dataset . In the first version of the R-statistical environment this kind of analysis is not included. If additional resources will be available, the extension of the package to this kind of analysis will be taken into consideration.   


3.	Application of validated cohorts in in silico clinical trials

Applicability is defined as “ the relevance of a credibility assessment activity (e.g., validation activities) to support the use of the computational model for a context of use” (1). The applicability of the validation activities is governed by two factors: the relevance of the QoI used in the validation  to the QoIs of the CoU, and the relevance of the validation conditions relative to those of the CoU. 

The applicability is given, provided the relevance of the QoI as well as the relevance of the validation activities on the CoU has been shown or in other words, whether the validation evidence provided are relevant within the CoU of the model . Applicability is a prerequisite for application of validated cohorts in silico clinical trials.

But even when there are enough data to achieve sufficient statistical power, a more general problem of applicability remains. To be useful, a model should be able to make predictions for input values that are different from those used to assess its accuracy; but we do not know the predictive accuracy of the model for those new inputs. Considerations on the general regularity of physical quantities, and about the assumption that model accuracy should degrade smoothly in the sense that predictions made for similar inputs should present similar predictive accuracy, allow to assume that a degree of extrapolation is possible, in the sense that the model can be considered reliable even when used to predict for inputs different from those observed in the clinical validation cohort.

The following analytical techniques are applied in the R-statistical environment: 

3.1	One-group assessment

If “One- and two-group assessment” is selected, first the type of trial needs to be defined by selecting “1-group design” or “2-group design”.
For the 1-group design (i.e. only one validated cohort and no control), the dataset for the analysis should be imported as, currently, CSV files. For the import, any accessible computer can be browsed, and a specific file selected. Then the variable of interest should be specified by selecting from the list of all variables of the dataset imported. In the next step, the type of variable needs to be specified (discrete, continuous, time to event).
If “perform analysis” is clicked, then there are 3 options available:
a)	Data (default)
b)	Plot
c)	Analysis
Under “Data” the individual records belonging to the dataset can be browsed.
“Plot” provides a figure reporting frequency and a chi-square test for a discrete variable, a boxplot for a continuous variable and a Kaplan-Meier curve for a time-to-event variable. 


3.2	Two-group comparison

For the 2-group design, the two datasets for the analysis should be imported as, currently, CSV files. For the import, any accessible computer can be browsed, and two datasets should be selected sequentially. The structure of the two datasets should be the same. Again, and like the 1-group design, the variable of interest should be specified by selecting it from the list of all variables of the datasets imported. In the next step the type of variable needs to be specified (discrete, continuous, time-to-event).
If “perform analysis” is clicked, three options are available:
a)	Data (default)
b)	Plot
c)	Analysis 
Under “data” (default option), the individual datasets can be browsed. 
For a discrete variable, “plot” provides a figure reporting frequencies for the two datasets. For a continuous variable, boxplots of the two datasets are presented. For a time-to-event variable, the two Kaplan-Meier curves are shown in one figure.
The function “analysis” covers a chi-square test for a discrete variable, comparing the two datasets. For a continuous variable, a t-test is presented and for a time-to-event variable a Kaplan-Meier test.



References
1.	FDA: Assessing the credibility of computational modeling and simulation in medical device dubmissions. Draft guidance for industry (December 2021)
2.	ASME V&V 40-2018: Assessing credibility of computational modeling through verification and validation: Application to medical devices (2018)
3.	Bodner, J., Kaul, V.: A framework for in silico clinical trials for medical devices using concepts from model verification, validation, and uncertainty quantification (VVUQ). Proceedings of the ASME 2021, May 19-20, 2021)
4.	Aldieri A., Curelli, C., Szyszko, j. A., La Mattina, A.A., Viceconti, M.: Credibility assessment of computational models according to ASME V&V40: Application to the Bologna Biomechanical Computed Tomography solution. Computer methods and Program 240: 107727 (2023)
5.	Visconti, M., Juarez, M.A., Curelli, C. Pennisi, M.,Russo, G., Pappalardo, F.: Position paper: Credibiility of in silico trial technologies: A theoretical framing. IEEE Journal of Biomedical and Health Informatics 24, 2020
6.	Verstraeten, S., Hoeijmakersb, M., Pim Tonino, P.,  c, Jan Brüning, J., Capelli, C., van de
Vosse, F., Huberts, W.: Generation of synthetic aortic valve stenosis geometries for in silico trials (submitted, 2023)

