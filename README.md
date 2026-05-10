# NYS BRFSS 2023: Physical Activity and Hypertension

## Project overview

This repository contains a reproducible R Markdown final report and an interactive Shiny dashboard using the 2023 New York State Department of Health Behavioral Risk Factor Surveillance System (NYSDOH BRFSS) dataset.

The project examines the association between aerobic physical activity and self-reported hypertension among adults in New York State. The main research question is:

> Among adults in New York State in the 2023 BRFSS, is meeting the aerobic physical activity recommendation associated with lower prevalence of self-reported hypertension?

The goal of this project is to demonstrate a full public health data analysis workflow, including data loading, cleaning, descriptive statistics, visualization, uncertainty estimates, statistical testing, regression modeling, interpretation, and interactive communication through a Shiny app.

## Repository contents

| File | Description |
|---|---|
| `VTPEH6270 Final Report.Rmd` | R Markdown final report that can be knitted to PDF for submission. |
| `app.R` | Shiny dashboard for interactive exploration of hypertension prevalence by physical activity status and demographic subgroups. |
| `README.md` | Project description, reproducibility notes, and instructions for running the report and app. |

## Data source

This project uses the 2023 NYSDOH BRFSS Survey Data, publicly available through Health Data NY.

The final report downloads the dataset directly from Health Data NY using R, so the analysis does not depend on a local file path. The data are read from the downloaded zip file and then cleaned for analysis.

## Variables used

| Role | Variable | Description |
|---|---|---|
| Main exposure | `_PAINDX3` | Aerobic physical activity index; used to compare adults who meet vs. do not meet aerobic physical activity recommendations. |
| Main outcome | `_RFHYPE6` | Self-reported hypertension status. |
| Covariate | `_AGE65YR` / `_AGEG5YR` | Age category, used for adjustment in the report and filtering in the app. |
| Covariate | `_SEX` | Respondent sex. |
| Additional app variable | `GENHLTH` | Self-rated general health. |
| Additional app variable | `_INCOMG1` | Household income category. |
| Survey weight | `_LLCPWT` | BRFSS final sampling weight, used to produce weighted estimates. |

## Final report

The R Markdown report is structured as a scientific report with the following sections:

1. Introduction  
2. Materials and Methods  
3. Results  
4. Discussion and Interpretation  
5. Conclusion  
6. Reproducibility and AI Use Disclosure  
7. References  

The report includes:

- clear research question and public health context;
- data source and variable descriptions;
- reproducible data loading and cleaning code;
- analytic sample definition;
- weighted hypertension prevalence estimates;
- 95% confidence intervals;
- visualizations with uncertainty shown using error bars;
- chi-square test for the unadjusted association;
- survey-weighted logistic regression adjusted for age group and sex;
- odds ratios, confidence intervals, and p-values;
- interpretation that distinguishes statistical significance from practical significance.

## Shiny dashboard

The Shiny app provides an interactive dashboard for exploring the same public health question in a more user-friendly way.

Dashboard features include:

- filters for age group and sex;
- weighted hypertension prevalence by physical activity status;
- approximate 95% confidence intervals;
- general health distribution by physical activity status;
- summary statistics table;
- filtered data table;
- variable explanations with names matched to the data table;
- clear interpretation panel for non-technical users.

The dashboard is designed for public health communication and exploratory analysis. For formal inference, the final report provides the more appropriate statistical modeling framework.

## How to run the final report

Open `VTPEH6270 Final Report.Rmd` in RStudio and click **Knit** to generate the PDF report.

Before knitting, install the required packages if they are not already installed:

```r
install.packages(c("tidyverse", "knitr", "survey", "scales"))
```

The report uses the following main packages:

```r
library(tidyverse)
library(knitr)
library(survey)
library(scales)
```

Because the report is configured to generate a PDF, a LaTeX installation may be required. If PDF knitting fails because of LaTeX, install TinyTeX:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

## How to run the Shiny app locally

Open `app.R` in RStudio and click **Run App**.

Required packages include:

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "DT",
  "scales"
))
```

Then run:

```r
shiny::runApp()
```

## Statistical approach

The final report uses weighted analysis because BRFSS is a survey dataset. The main model is a survey-weighted logistic regression with self-reported hypertension as the binary outcome and aerobic physical activity status as the main exposure, adjusting for age group and sex.

The Shiny app uses weighted descriptive estimates and approximate 95% confidence intervals for communication and exploration. The app includes a note that a formal publication-quality BRFSS analysis should account for the full complex survey design, including strata, clusters, and final weights when available.

## Main interpretation

This project evaluates whether adults who meet aerobic physical activity recommendations have lower self-reported hypertension prevalence than adults who do not meet the recommendations. The analysis should be interpreted as associational rather than causal because BRFSS is cross-sectional and based on self-reported survey responses.

A lower hypertension prevalence among adults meeting physical activity recommendations would be consistent with public health expectations, but the result should not be interpreted as proof that physical activity alone caused lower hypertension risk. Age, sex, income, health status, access to care, medication use, and other behavioral factors may also influence the relationship.

## Limitations

Key limitations include:

- the cross-sectional design, which prevents establishing temporality;
- self-reported hypertension and behavior measures;
- potential residual confounding;
- exclusion of records with missing or invalid values;
- limited adjustment variables in the main model;
- simplified uncertainty estimation in the Shiny app compared with full complex survey methods.

## Reproducibility

The final report is designed to be reproducible because it downloads the public dataset directly from Health Data NY and contains the full data cleaning and analysis workflow. The Shiny app uses the same core variables and supports transparent exploration of the cleaned dataset.

## AI use disclosure

AI assistance was used to support code organization, wording, dashboard design, and interpretation drafting. All code, statistical choices, results, and final interpretations were reviewed and edited by the author.

## Author

Xibiao Wang

## Course context

VTPEH 6270 R / public health data analysis project
