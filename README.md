# Statistical Data Science

A collection of statistical analysis projects implemented in R, completed as part of a **Data Science Minor** at the **American University of Afghanistan (AUAF)**, Spring 2021.

## Projects

| File | Description |
|------|-------------|
| `Project.R` | Descriptive statistics analysis of the pension dataset — frequency tables, charts, measures of location/variation/distribution, and relationship analysis |
| `Exercises.R` | Empirical probability, joint analysis of student data, random vs non-random sampling, and confidence interval estimation | 
## Datasets

| Dataset | Used In | Description |
|---------|---------|-------------|
| `pension.csv` | Practical Project | Retirement savings data — age, education, gender, marital status (via `wooldridge` package) |
| `cleaned.csv` | Discussion Project | Student academic data — GPA, gender, class level, height, weight |
| `gradetest.csv` | Discussion Project | Student grade data — midterm, oral, and quiz scores |

## Tech Stack

- R
- wooldridge · moments · Rmisc · dplyr

## Topics Covered

- Frequency distribution tables (qualitative and quantitative)
- Measures of location — mean, median, mode, quantiles
- Measures of variation — variance, SD, CV, range, IQR, MAD
- Measures of distribution — skewness and kurtosis
- Data visualization — pie charts, histograms, scatter plots, density plots
- Correlation and covariance analysis
- Hypothesis testing — t-test, chi-square test
- Empirical probability using ECDF
- Random vs non-random sampling
- Confidence interval estimation (90%, 95%, 99%)

## Getting Started

```bash
git clone https://github.com/saalssall/Statistical_Data_Science.git
cd Statistical_Data_Science
```

Then in R or RStudio:

```r
install.packages(c("wooldridge", "moments", "Rmisc", "dplyr"))
setwd("path/to/Statistical_Data_Science")
source("Practical_Project.R")
```

Make sure the required datasets are in the working directory before running.

---

© 2026 saalssall
