# VTPEH 6270 - Check Point 06  
## Statistical Analyses

**Author:** Xibiao Wang

## Overview

This project examines whether meeting the aerobic physical activity recommendation is associated with lower prevalence and lower odds of self-reported hypertension among adults in the 2023 New York State BRFSS dataset.

## Research Question

Is meeting the aerobic physical activity recommendation associated with lower prevalence of self-reported hypertension among adults in the 2023 New York State BRFSS?

## Data Source

The analysis uses the 2023 New York State BRFSS dataset and focuses on the following variables:

- `_PAINDX3`: Aerobic Physical Activity Index  
- `_RFHYPE6`: Hypertension awareness  
- `_AGE65YR`: Age group  
- `_SEX`: Sex  

## Methods

The report includes data cleaning, descriptive statistics, a Pearson chi-square test, multivariable logistic regression, and comparative visualization. The logistic regression model adjusts for age group and sex.

## Key Result

Meeting aerobic physical activity recommendations was associated with lower odds of self-reported hypertension after adjustment for age group and sex.

## Repository Contents

- `VTPEH6270_CP06.Rmd` – R Markdown source file  
- `VTPEH6270_CP06.pdf` – final report  
- `Hypertension_PA_plot.pdf` – figure output  
- `README.md` – project overview  

## Reproducibility

The dataset is downloaded directly within the R Markdown file. To reproduce the report, open the `.Rmd` file in RStudio, install the required packages, and knit to PDF.

Required packages:

- `readr`
- `dplyr`
- `ggplot2`

## AI Use Disclosure Statement

ChatGPT was used to assist with code refinement, debugging, and improvement of statistical writing. Final analytical decisions and interpretations were reviewed by the author.

## GitHub Repository

[https://github.com/Sello0622/6270-2.0.git](https://github.com/Sello0622/6270-2.0.git)
