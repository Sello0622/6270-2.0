---
title: "VTPEH 6270 - Check Point 03"
subtitle: "Data Set Selection & R markdown Report Outline"
author: "Xibiao Wang"
date: "`r Sys.Date()`"
editor_options:
  chunk_output_type: console
output:
  pdf_document:
    toc: true
    toc_depth: 1
    number_sections: true
    latex_engine: xelatex
urlcolor: blue
# bibliography: references.bib
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

```

# Research Question and Background

## Research Question

Is meeting the aerobic physical activity recommendation associated with lower prevalence of self-reported hypertension among adults in the 2023 New York State BRFSS?

## Scientific Plausibility and Context

Hypertension is a major modifiable risk factor for cardiovascular disease and remains one of the leading contributors to morbidity and mortality in the United States. Regular aerobic physical activity has been consistently shown to reduce blood pressure through improvements in vascular endothelial function, autonomic balance, insulin sensitivity, and weight control. The American College of Cardiology and the American Heart Association recommend at least 150 minutes of moderate-intensity aerobic physical activity per week for cardiovascular health promotion and blood pressure reduction.

Epidemiologic evidence demonstrates that individuals who meet aerobic physical activity guidelines have lower prevalence of hypertension compared to physically inactive individuals. A meta-analysis by Cornelissen and Smart (2013) found that aerobic exercise significantly reduces systolic and diastolic blood pressure in both hypertensive and normotensive adults. Similarly, the 2018 Physical Activity Guidelines Advisory Committee Scientific Report concluded that higher levels of physical activity are associated with lower incidence of hypertension.

In the 2023 New York State BRFSS dataset, aerobic physical activity is categorized using the calculated variable _PAINDX3, which classifies respondents as meeting or not meeting the ≥150-minute recommendation (Page 16). Hypertension status is derived from _RFHYPE6, based on whether a health professional has told the respondent they have high blood pressure (Page 19). Examining the association between these variables allows evaluation of whether adherence to aerobic guidelines is associated with lower hypertension prevalence in this population.



# Data Preparation


## 3.1 Variable Selection

The following variables were selected from the 2023 New York State BRFSS dataset:
	 Exposure variable: _PAINDX3 (Aerobic Physical Activity Index)
	 “Meet Aerobic Recommendations” (≥150 minutes/week)
	 “Did Not Meet Aerobic Recommendations”
	 Outcome variable: _RFHYPE6 (Hypertension awareness)
	 “Yes” (told high blood pressure)
	 “No”
	 Covariates:
	  AGE65YR (Age group: “Age 18 to 64” vs “Age 65 or older”)
	  SEX (Male/Female)

These variables are provided in labeled categorical form in the BRFSS dataset and are appropriate for summary and visualization.

## 3.2 Data Cleaning

Observations with missing, refused, or “don’t know” responses were excluded from the analysis to avoid misclassification.

Specifically, the following responses were removed:
	 PAINDX3: responses other than
“Meet Aerobic Recommendations” or
“Did Not Meet Aerobic Recommendations”
	 RFHYPE6: responses other than
“Yes” or “No”
	 AGE65YR: responses other than
“Age 18 to 64” or
“Age 65 or older”

This ensured that only respondents with valid exposure, outcome, and age information were included in the final analytic dataset (n = 14,544).

## 3.3 Derived Variables

To simplify interpretation and facilitate statistical analysis, binary variables were created:

Hypertension (binary outcome)

A new variable hypertension was created:
	  1 = Yes (told high blood pressure)
	  0 = No

Physical Activity (binary exposure)

A new variable meets_PA was created:
	  1 = Meets aerobic recommendation (≥150 min/week)
	  0 = Does not meet recommendation

These binary variables allow direct estimation of prevalence differences, prevalence ratios, and odds ratios.



## 3.4 Categorization Requirement

The assignment requires that at least one considered variable be categorical for summary and visualization purposes.

Both the exposure variable (_PAINDX3) and outcome variable (_RFHYPE6) are categorical variables by definition in the BRFSS dataset.

Therefore, no additional discretization was required.


```{r, results='hide'}
# Import data
library(readr)

temp <- tempfile()
download.file("https://health.data.ny.gov/download/tk4g-wdfe/application%2Fx-zip-compressed",temp)
df <- read.csv(unz(temp, "NYSDOH_BRFSS_SurveyData_2023.csv"))
unlink(temp)

names(df)
grep("PA", names(df), value = TRUE)
grep("HYPE", names(df), value = TRUE)

library(dplyr)

df_clean <- df %>%
  select(X_PAINDX3, X_RFHYPE6, X_AGE65YR, X_SEX) %>%
  filter(
    X_PAINDX3 %in% c("Meet Aerobic Recommendations",
                     "Did Not Meet Aerobic Recommendations"),
    X_RFHYPE6 %in% c("Yes", "No"),
    X_AGE65YR %in% c("Age 18 to 64", "Age 65 or older")
  ) %>%
  mutate(
    hypertension = ifelse(X_RFHYPE6 == "Yes", 1, 0),
    meets_PA     = ifelse(X_PAINDX3 == "Meet Aerobic Recommendations", 1, 0),
    age_group    = factor(X_AGE65YR),
    sex          = factor(X_SEX)
  )
```


# Grouped Summary Statistics
## Stratified Analysis
```{r}
tab <- table(df_clean$meets_PA, df_clean$hypertension)
tab
prop.table(tab, margin = 1) * 100

library(dplyr)

summary_table <- df_clean %>%
  group_by(meets_PA) %>%
  summarise(
    n = n(),
    hypertension_cases = sum(hypertension),
    prevalence = mean(hypertension) * 100
  )

summary_table

chisq.test(tab)

model <- glm(hypertension ~ meets_PA + age_group + sex,
             data = df_clean,
             family = binomial)

summary(model)

exp(coef(model))

exp(confint(model))


```
A total of 14,544 respondents were included in the analytic sample after excluding observations with missing or invalid responses.

Descriptive Statistics by Physical Activity Status

Among individuals who did not meet aerobic physical activity recommendations (n = 5,608), 2,198 reported having hypertension, corresponding to a prevalence of 39.2%.

Among individuals who met aerobic physical activity recommendations (n = 8,936), 2,943 reported having hypertension, corresponding to a prevalence of 32.9%.

The prevalence of hypertension was therefore 6.3 percentage points higher among individuals who did not meet aerobic physical activity guidelines compared to those who met recommendations.

Statistical Test of Association

A Pearson’s Chi-square test was conducted to examine the association between meeting aerobic physical activity recommendations and hypertension status.

The association was statistically significant:

χ²(1) = 58.81, p < 0.001.

This indicates that hypertension prevalence differs significantly between the two physical activity groups.

Multivariable Adjustment

To account for potential confounding by age and sex, a multivariable logistic regression model was fitted.

After adjusting for age group and sex:
	  Meeting aerobic physical activity recommendations was associated with lower odds of hypertension
OR = 0.69 (95% CI: 0.64–0.74)
	  Individuals aged 65 years or older had substantially higher odds of hypertension compared to those aged 18–64
OR = 4.38 (95% CI: 4.06–4.73)
	  Male respondents had higher odds of hypertension compared to female respondents
OR = 1.42 (95% CI: 1.32–1.52)

These results suggest that meeting aerobic physical activity recommendations is independently associated with lower hypertension prevalence, even after accounting for age and sex differences.

### Interpretation
The findings consistently demonstrate an inverse association between aerobic physical activity and hypertension. Individuals who met recommended physical activity levels had both lower unadjusted prevalence and significantly reduced adjusted odds of hypertension. The strength and consistency of the association across descriptive and multivariable analyses support the hypothesis that regular aerobic physical activity is associated with improved cardiovascular health outcomes.

# Comparative Visualizations

```{r, message=FALSE, warning=FALSE, fig.width=10, fig.height=5}

library(ggplot2)
library(dplyr)

plot_data <- df_clean %>%
  group_by(meets_PA) %>%
  summarise(
    prevalence = mean(hypertension) * 100
  )

plot_data

ggplot(plot_data, aes(x = factor(meets_PA),
                      y = prevalence,
                      fill = factor(meets_PA))) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_x_discrete(labels = c("Did Not Meet PA",
                              "Met PA")) +
  labs(
    x = "Aerobic Physical Activity Status",
    y = "Hypertension Prevalence (%)",
    title = "Hypertension Prevalence by Physical Activity Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

plot_data_ci <- df_clean %>%
  group_by(meets_PA) %>%
  summarise(
    n = n(),
    cases = sum(hypertension),
    prevalence = mean(hypertension),
    se = sqrt(prevalence * (1 - prevalence) / n),
    lower = (prevalence - 1.96 * se) * 100,
    upper = (prevalence + 1.96 * se) * 100,
    prevalence = prevalence * 100
  )

ggplot(plot_data_ci, aes(x = factor(meets_PA),
                         y = prevalence)) +
  geom_col(width = 0.6, fill = "steelblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2) +
  scale_x_discrete(labels = c("Did Not Meet PA",
                              "Met PA")) +
  labs(
    x = "Aerobic Physical Activity Status",
    y = "Hypertension Prevalence (%)"
  ) +
  theme_classic(base_size = 14)

```
## Association Visualization

Figure 1 presents hypertension prevalence stratified by aerobic physical activity status.

Individuals who did not meet aerobic physical activity recommendations had a hypertension prevalence of 39.2%, whereas those who met recommendations had a prevalence of 32.9%.

The bar plot with 95% confidence intervals visually demonstrates a clear difference between the two groups, with non-overlapping confidence intervals suggesting a statistically meaningful difference.

This visualization directly supports the research question and aligns with both the chi-square test and multivariable logistic regression findings.


Visual Interpretation

The graphical comparison indicates an inverse association between aerobic physical activity and hypertension prevalence.

Individuals who meet recommended levels of physical activity consistently show lower hypertension prevalence. The magnitude of the difference (~6 percentage points) is epidemiologically meaningful.

Given that the adjusted logistic regression analysis yielded:

OR = 0.69 (95% CI: 0.64–0.74),

the visualization reinforces the conclusion that physical activity is independently associated with lower hypertension odds.
 

```{r}

# publication PDF
ggsave("Hypertension_PA_plot.pdf",
       width = 6,
       height = 4,
       dpi = 300)

```





# AI Use Disclosure Statement

AI Use Disclosure Statement

Yes, I used ChatGPT to assist with this assignment. The tool was used to help refine the analysis plan, debug and improve R code that I wrote, and enhance clarity in statistical interpretation and writing. All analytical decisions and final interpretations were reviewed and verified by me.



# References

Cornelissen, V. A., & Smart, N. A. (2013). Exercise training for blood pressure: A systematic review and meta‐analysis. *Journal of the American Heart Association*, 2(1), e004473.

Physical Activity Guidelines Advisory Committee. (2018). *Physical Activity Guidelines Advisory Committee Scientific Report*. U.S. Department of Health and Human Services.

Whelton, P. K., Carey, R. M., Aronow, W. S., et al. (2018). 2017 ACC/AHA guideline for the prevention, detection, evaluation, and management of high blood pressure in adults. *Hypertension*, 71(6), e13–e115.


