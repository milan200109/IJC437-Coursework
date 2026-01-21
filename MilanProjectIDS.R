rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(ggplot2)
library(scales)
library(readr)
library(broom)

# Input File path

file_path <- "C:/Users/USER/Downloads/BusinessDemography2019-24Dataset.xlsx"

# Function to clean speacial characters

to_num <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, ",", "")
  x <- str_trim(x)
  x[x == ":" | x == ""] <- NA
  suppressWarnings(as.numeric(x))
}

keep_geo <- function(area_code){
  str_detect(area_code, "^(K02|E92|W92|S92|N92|E12)")
}

geo_type <- function(area_code){
  case_when(
    str_detect(area_code, "^K02") ~ "UK",
    str_detect(area_code, "^(E92|W92|S92|N92)") ~ "Country",
    str_detect(area_code, "^E12") ~ "Region",
    TRUE ~ "Other"
  )
}

# Cleaning input data

birth_sheets <- c("Table 1.1a","Table 1.1b","Table 1.1c","Table 1.1d")

read_birth_sheet <- function(sheet){
  
  df <- read_excel(file_path, sheet = sheet, skip = 3) %>%
    clean_names()
  
  names(df)[1:2] <- c("area_code","area_name")
  
  df %>%
    filter(keep_geo(area_code)) %>%
    mutate(area_type = geo_type(area_code)) %>%
    pivot_longer(
      cols = -c(area_code, area_name, area_type),
      names_to = "year_raw",
      values_to = "births_raw"
    ) %>%
    mutate(
      year   = as.integer(str_extract(year_raw, "\\d{4}")),
      births = to_num(births_raw)
    ) %>%
    filter(!is.na(year)) %>%
    select(area_type, area_code, area_name, year, births)
}

births_geo <- bind_rows(lapply(birth_sheets, read_birth_sheet)) %>%
  distinct(area_type, area_code, area_name, year, .keep_all = TRUE) %>%
  arrange(area_type, area_name, year)

# Get survival data based on Cohort

surv_sheets <- c("Table 5.1a","Table 5.1b","Table 5.1c","Table 5.1d","Table 5.1e")
cohort_map  <- c("Table 5.1a"=2019,"Table 5.1b"=2020,"Table 5.1c"=2021,
                 "Table 5.1d"=2022,"Table 5.1e"=2023)

read_survival_sheet <- function(sheet){
  
  cohort_year <- cohort_map[[sheet]]
  
  df <- read_excel(file_path, sheet = sheet, skip = 3) %>%
    clean_names()
  
  names(df)[1:2] <- c("area_code","area_name")
  
  births_col <- names(df)[str_detect(names(df), "births")][1]
  surv_col   <- names(df)[str_detect(names(df), "1_year.*per_cent|percent")][1]
  
  df %>%
    filter(keep_geo(area_code)) %>%
    mutate(area_type = geo_type(area_code)) %>%
    transmute(
      area_type,
      area_code,
      area_name,
      cohort_year = cohort_year,
      births_cohort = to_num(.data[[births_col]]),
      surv_1yr_pct  = to_num(.data[[surv_col]]),
      surviving_businesses = births_cohort * surv_1yr_pct / 100
    )
}

survival_geo <- bind_rows(lapply(surv_sheets, read_survival_sheet)) %>%
  arrange(area_type, area_name, cohort_year)

# Saving clean data

write_csv(births_geo, "clean_births_countries_regions_2019_2024.csv")
write_csv(survival_geo, "clean_survival1yr_countries_regions_cohorts_2019_2023.csv")

# EXPLORATORY DATA ANALYSIS (EDA)

summary(births_geo$births)
summary(survival_geo$surv_1yr_pct)
anyNA(births_geo$births)
anyNA(survival_geo$surv_1yr_pct)

range(births_geo$year)

# Creating theme for VISUALISATIONS

theme_report <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face="bold", hjust=0.5),
    plot.subtitle = element_text(hjust=0.5),
    panel.grid.minor = element_blank()
  )

# Coding of Analysis and VISUALISATIONS

# Plot of UK Births Over Time

births_uk <- births_geo %>% filter(area_type == "UK")

p1 <- ggplot(births_uk, aes(year, births)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2020, linetype = "dashed") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "UK Business Births (2019–2024)",
    subtitle = "Dashed line indicates COVID-19 shock",
    x = "Year",
    y = "Business births"
  ) +
  theme_report

p1

# Plot of Regional Trends

births_regions <- births_geo %>% filter(area_type == "Region")

p2 <- ggplot(births_regions, aes(year, births)) +
  geom_line() +
  facet_wrap(~ area_name, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Regional Business Birth Trends",
    x = "Year",
    y = "Business births"
  ) +
  theme_report

p2

# Plot 3 of COVID Impact (2019 - 2020)

covid_change <- births_regions %>%
  filter(year %in% c(2019, 2020)) %>%
  pivot_wider(names_from = year, values_from = births) %>%
  mutate(pct_change = (`2020` - `2019`) / `2019` * 100)


p3 <- ggplot(covid_change, aes(reorder(area_name, pct_change), pct_change)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(round(x,1), "%")) +
  labs(
    title = "COVID-19 Impact on Business Births (2019–2020)",
    x = "Region",
    y = "Percentage change"
  ) +
  theme_report

p3

# Plot 4 of Country-Level Trends

births_countries <- births_geo %>% filter(area_type == "Country")

p4 <- ggplot(births_countries, aes(year, births)) +
  geom_line() +
  facet_wrap(~ area_name, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = 2020, linetype = "dashed") +
  labs(
    title = "Business Births by UK Country",
    x = "Year",
    y = "Business births"
  ) +
  theme_report

p4 

# Plot 5 of UK Survival Rates by Cohort

surv_uk <- survival_geo %>% filter(area_type == "UK")

p5<- ggplot(surv_uk, aes(x = cohort_year, y = surv_1yr_pct)) +
  geom_line(linewidth = 1.2, colour = "#2c7fb8") +
  geom_point(size = 4, colour = "#2c7fb8") +
  geom_text(
    aes(label = paste0(round(surv_1yr_pct,1), "%")),
    vjust = -0.1,
    size = 4
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "UK 1-Year Business Survival by Birth Cohort",
    subtitle = "COVID cohorts show a modest but persistent decline in survival",
    x = "Birth cohort year",
    y = "1-year survival rate"
  ) +
  theme_report

p5

#Plot 6 of Regional Survival (Pre vs COVID)

surv_levels <- survival_geo %>%
  filter(area_type == "Region", cohort_year %in% c(2019, 2020)) %>%
  mutate(
    cohort = factor(
      cohort_year,
      levels = c(2019, 2020),
      labels = c("2019 (Pre-COVID)", "2020 (COVID)")
    )
  )

p6 <- ggplot(
  surv_levels,
  aes(x = area_name, y = surv_1yr_pct, fill = cohort)
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6,
    colour = "black"
  ) +
  geom_text(
    aes(label = paste0(round(surv_1yr_pct, 1), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c(
      "2019 (Pre-COVID)" = "#4daf4a",
      "2020 (COVID)"     = "#e41a1c"
    )
  ) +
  labs(
    title = "Regional Business Survival Rates Before and During COVID-19",
    subtitle = "1-year survival of businesses born in 2019 and 2020",
    x = "Region",
    y = "1-year survival rate",
    fill = "Birth cohort"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank()
  )

p6

#INTERRUPTED TIME-SERIES REGRESSION

reg_df <- births_uk %>%
  mutate(covid = ifelse(year %in% c(2020, 2021), 1, 0))

its_model <- lm(births ~ year + covid, data = reg_df)

tidy(its_model)
confint(its_model)
glance(its_model)

reg_df <- reg_df %>%
  mutate(
    fitted = fitted(its_model),
    resid  = resid(its_model)
  )

# Observed vs Fitted plot

p7 <- ggplot(reg_df, aes(x = year)) +
  
  # observed data
  geom_line(
    aes(y = births, colour = "Observed"),
    linewidth = 1.2
  ) +
  geom_point(
    aes(y = births, colour = "Observed"),
    size = 2.8
  ) +
  
  # fitted model
  geom_line(
    aes(y = fitted, colour = "Fitted (ITS model)"),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  geom_vline(
    xintercept = 2020,
    linetype = "dotdash",
    colour = "black",
    linewidth = 0.9
  ) +
  
  annotate(
    "text",
    x = 2020.1,
    y = max(reg_df$births),
    label = "COVID-19 onset",
    hjust = 0,
    vjust = 1,
    size = 4
  ) +
  
  scale_colour_manual(
    values = c(
      "Observed" = "#1b9e77",
      "Fitted (ITS model)" = "#d95f02"
    )
  ) +
  
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    title = "Interrupted Time-Series Analysis of UK Business Births",
    subtitle = "Observed data and fitted model before and after the COVID-19 shock",
    x = "Year",
    y = "Number of business births",
    colour = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

p7

#Residual Diagnostics plot

p8 <- ggplot(reg_df, aes(x = year, y = resid)) +
  
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = -10000,
    ymax = 10000,
    fill = "grey80",
    alpha = 0.4
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  geom_point(
    size = 3,
    colour = "#2c7fb8"
  ) +
  
  geom_line(
    linewidth = 0.6,
    colour = "#2c7fb8",
    alpha = 0.6
  ) +
  
  geom_vline(
    xintercept = 2020,
    linetype = "dotdash",
    linewidth = 0.8
  ) +
  
  annotate(
    "text",
    x = 2020.1,
    y = max(reg_df$resid),
    label = "COVID-19 onset",
    hjust = 0,
    vjust = 1,
    size = 4
  ) +
  
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    title = "Regression Residuals from Interrupted Time-Series Model",
    subtitle = "Residuals fluctuate around zero, indicating no systematic model bias",
    x = "Year",
    y = "Residual (observed − fitted business births)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

p8
