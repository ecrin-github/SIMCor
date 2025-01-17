# SIMCor Shiny App

## Overview
**SIMCor** is an advanced Shiny application for statistical analysis, with a focus on validating and applying virtual cohorts. This tool is designed for researchers, data analysts, and academicians who require rigorous data analysis and visualization in fields like biomedical research, statistical modeling, and data science.

---

## Key Features
- **Data Import and Validation:** Upload and validate virtual and real datasets in CSV format.
- **Advanced Statistical Analysis:** Includes univariate, bivariate, and multivariate comparisons, along with variability assessments.
- **Interactive Visualizations:** Dynamic plots using `ggplot2` and `plotly`.
- **Report Generation:** Export analysis reports in PDF format.
- **Bootstrap Analysis:** For variability assessments and comparisons.

---

## Prerequisites
Before starting, ensure you have the following installed:

1. **R and RStudio:** Download [R](https://cran.r-project.org/) and [RStudio](https://www.rstudio.com/).
2. **Shiny Package:** Install the Shiny framework by running the following in R:

   ```R
   install.packages("shiny")
   ```

---

## Installation
To set up and run the SIMCor Shiny app locally:

### 1. Clone the Repository
Clone this repository to your local machine or download it as a ZIP file and extract it.

### 2. Open the Project
Open the RStudio project file (if provided) or the `app.R` file in RStudio.

### 3. Install Dependencies
Run the following command in RStudio to install the required packages:

```R
install.packages(c("shiny", "readxl", "data.table", "ggplot2", "plotly", "reshape2", 
                   "corrplot", "GGally", "boot", "dplyr", "shinydashboard"))
```

### 4. Load the App
Open the `server.R` and `ui.R` files (or `app.R`) and click the **Run App** button in RStudio, or use:

```R
shiny::runApp()
```

---

## Data Requirements

### Dataset Specifications
- **Format:** CSV files.
- **Structure:** Datasets should follow the specific structure provided in the sample/template files.

### Example Dataset
This app supports the analysis and comparison of virtual and real datasets. For example:

> A study by Verstraeten et al. (2023) validated synthetic aortic valve stenosis geometries using in vivo data. Datasets from this study are available at [4TU.ResearchData](https://data.4tu.nl/).

---

## Using the App

### Navigation
The app contains modules for validation, analysis, and comparison of datasets. Use the tabs and buttons to navigate through the interface.

### Uploading Data
Upload your datasets through the file upload inputs provided.

### Interactive Analysis
Analyze data interactively with:
- **Statistical Tools:** Summary statistics, correlations, variability assessments.
- **Visualization Options:** Heatmaps, scatter plots, and more.

### Reporting
Generate and download PDF reports summarizing your analysis.

---

## Functionalities

### Validation and Application of Virtual Cohorts
- **Context of Use (CoU):** Describes the role and scope of the computational model.
- **Question of Interest (QoI):** Defines the specific question or decision being addressed.
- These elements are stored as metadata and included in statistical reports.

### Validation Techniques
#### 1. **Univariate Distributions**
- Calculate mean, standard deviation, max, and min values.
- Visualize with scatter and box plots.

#### 2. **Bivariate Correlations**
- Compare Spearman correlation coefficients with heatmaps.

#### 3. **Multivariate Analysis**
- Compare variability metrics across datasets.

---

## General Information
- **Name:** SIMCor (In-Silico Testing and Validation of Cardiovascular Implantable Devices).
- **Version:** v0.1.0
- **Release Date:** 2023
- **Creators:**
  - T. Khorchani (ECRIN)
  - P.E. Verde (Heinrich Heine University and University Hospital Düsseldorf)
  - C. Ohmann (ECRIN)

---

## References
1. Verstraeten S, Hoeijmakers M, Tonino P, et al. "Generation of synthetic aortic valve stenosis geometries for in silico trials." *Int J Numer Meth Biomed Engng*. 2023. doi:10.1002/cnm.3778.
2. Efron, B. & Tibshirani, R. (1993). *An Introduction to the Bootstrap*. Chapman & Hall.
3. Haddad T., Himes A., Thompson L., et al. (2017). "Incorporation of stochastic engineering models as prior information in Bayesian medical device trials." *Journal of Biopharmaceutical Statistics*, 27:6, 1089-1103.

---

## Contact
For support or inquiries, contact:  
**Takoua Khorchani**  
📧 takouakhorchani@gmail.com
