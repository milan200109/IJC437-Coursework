# IJC437-Coursework
Coding related to the coursework.

Project Overview:

This project analyses UK business births and early survival patterns between 2019 and 2024 using Office for National Statistics (ONS) Business Demography data. The study examines how business formation and one-year survival varied across UK countries and English regions, with particular attention to the COVID-19 shock period.

The analysis combines:

1. Data cleaning and preprocessing of multi-sheet Excel files
2. Exploratory data analysis (EDA)
3. Descriptive visualisations
and an Interrupted Time Series (ITS) regression to assess whether COVID-19 coincides with structural changes in business births.

Data Source:

1. ONS Business Demography Dataset (2019–2024)
    Source: Office for National Statistics

2. Input format: Excel workbook containing multiple reference tables
    Business births: Tables 1.1a–1.1d
    1-year survival rates: Tables 5.1a–5.1e
The raw Excel dataset is not included in this repository and must be downloaded separately.

Project Workflow

1. Data Import & Cleaning
   - Reads multiple Excel sheets
   - Standardises column names
   - Converts numeric fields with special characters
   - Filters valid UK, country, and regional area codes

2. Data Reshaping
   - Transforms wide tables into long format
   - Constructs cohort-based survival datasets
   - Removes duplicates and validates time coverage

3. Exploratory Data Analysis
   - Uses summary() functions to examine distributions
   - Checks for missing values
   - Confirms full coverage for 2019–2024

4. Visualisation
   - UK-level trends in business births
   - Regional and country-level comparisons
   - COVID-19 impact (2019–2020 percentage changes)
   - One-year survival by cohort and region

5. Interrupted Time Series Analysis
   - Estimates an ITS regression using lm()
   - Defines COVID-19 as an intervention period (2020–2021)
   - Evaluates model fit and residual diagnostics
   - Visualises observed vs fitted trends

Installation Instructions

1. Install R

Ensure R (≥ 4.0) is installed on your system.
Download from: https://cran.r-project.org/

2. Install Required Packages

Run the following command in R to install all dependencies:

install.packages(c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "janitor",
  "ggplot2",
  "scales",
  "readr",
  "broom"
))

3. Set File Path

Update the file path in the script to point to your local copy of the ONS 

Excel dataset:

current file path <- "C:/Users/USER/Downloads/BusinessDemography2019-24Dataset.xlsx"

Library Dependencies

This project relies on the following R packages:
1. readxl – reading Excel files
2. dplyr – data manipulation
3. tidyr – reshaping data
4. stringr – string handling
5. janitor – column name cleaning
6. ggplot2 – data visualisation
7. scales – axis and label formatting
8. readr – exporting cleaned datasets
9. broom – tidying regression outputs

Outputs:
1. clean_births_countries_regions_2019_2024.csv
2. clean_survival1yr_countries_regions_cohorts_2019_2023.csv
3. Eight publication-quality figures
4. ITS regression estimates and diagnostics

Reproducibility

To reproduce the analysis:
1. Download the ONS Business Demography Excel dataset
2. Update the file path in the script
3. Run the script from top to bottom in R

All steps are fully scripted for transparency and reproducibility.

Notes:

1. The analysis focuses on one-year survival rates to ensure consistent availability across cohorts and regions.
2. Longer survival horizons are excluded to avoid sample attrition.
3. Results reflect registered business activity only.
