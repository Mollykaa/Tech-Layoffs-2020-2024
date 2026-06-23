# Tech Layoffs 2020–2024 | Statistical Analysis in R

**Author:** Vesethmollyka VAR  
**Institution:** Institut Mines-Télécom Business School  
**Course:** Application of Statistics

![R](https://img.shields.io/badge/R-276DC3?style=flat-square&logo=r&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=flat-square&logo=rstudio&logoColor=white)
![ggplot2](https://img.shields.io/badge/ggplot2-276DC3?style=flat-square&logoColor=white)

---

## Overview

This project performs a statistical analysis of global tech industry layoffs from 2020 to 2024. Using R and ggplot2, the analysis explores layoff trends across continents, examines the relationship between company size and layoffs, and applies hypothesis testing to draw statistically grounded conclusions.

**Dataset:** `tech_layoffs.csv` — 1,418 company-level layoff records including company name, location, continent, company size before and after layoffs, and year.

---

## Research Questions

1. How did tech layoffs trend across continents from 2020 to 2024?
2. Is there a statistically significant correlation between company size and the number of layoffs?
3. Did company size change significantly before vs. after layoffs?

---

## Methods & Analysis

### Descriptive Statistics
- Dataset structure and data type classification
- Summary statistics on `Laid_Off` and `Company_Size_before_Layoffs`
- Distribution analysis with skewness and kurtosis

### Exploratory Data Analysis
- Total layoffs aggregated by continent
- Layoff trends tracked by continent across years (2020–2024)
- Average layoffs by company size category (Small / Medium / Large)

### Hypothesis Testing
- **Correlation test** — relationship between company size and layoffs (with 95% confidence intervals)
- **Paired t-test** — company size before vs. after layoffs
- **Linear regression** — company size as a predictor of layoffs

---

## Visualizations

| Chart | Description |
|---|---|
| Line graph | Layoff trends across continents over time (2020–2024) |
| Scatter plot | Company size vs. number of layoffs |
| Scatter plot with trend line | Linear regression overlay |
| Histogram with density curve | Company size distribution with skewness and kurtosis |
| Error bar plot | Paired t-test results with confidence intervals |

---

## Libraries Used

```r
library(dplyr)   # Data manipulation
library(psych)   # Descriptive statistics
library(ggplot2) # Visualizations
```

---

## How to Run

```r
# 1. Clone the repository
# 2. Set your working directory to the folder containing tech_layoffs.csv
# 3. Open Tech Layoff Analysis.R in RStudio
# 4. Run the script top to bottom
```

---

## Files

| File | Description |
|---|---|
| `Tech Layoff Analysis.R` | Full R analysis script (fixed and cleaned) |
| `tech_layoffs.csv` | Dataset — 1,418 tech layoff records (2020–2024) |
| `tech_layoffs.xlsx` | Dataset in Excel format |
| `Application of Statistics - Vesethmollyka VAR - Final Report.pdf` | Written report with findings and interpretation |
| `Tech Layoffs 2020-2024.pptx` | Presentation slides |

---

*Institut Mines-Télécom Business School — Application of Statistics*
