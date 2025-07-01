# ✈️ NYC Flight Delay Analysis — STAT 167 Final Project

*Arthur: Lydia Niu, Alexis Castaneda, Aparna Petluri, Gracelynne Mohan, Jenny Zhang, Zoe Shum*

## 📌 Overview

This project investigates **flight delays and volume trends** across New York City's three major airports—**JFK, LGA, and EWR**—using the `nycflights13` dataset. We explore how **seasonal changes** and **weather factors** (e.g., temperature, wind, visibility, and precipitation) influence delays and whether **flight volume** correlates with delay frequency or severity.

---

## ❓ Research Questions

1. Which months and seasons experience the highest flight volumes at each airport?
2. How do average delays vary by month and season?
3. Are delays more frequent/severe during specific weather conditions?
4. Are there significant differences in average delays/volume across the three airports and across different seasons?
5. What relationship exists between busy days (high flight volume), weather (temperature and wind speed), and delays?
6. Which airport is most affected by flight delays due to weather?

---

## 🗂️ Dataset Description

We used the following datasets from the `nycflights13` package:

* `flights`: 336,776 rows × 19 columns — all flights departing NYC in 2013.
* `weather`: 26,115 rows × 15 columns — hourly meteorological data.
* `airports`: 1,458 rows × 8 columns — airport metadata.

Key variables:

* `dep_delay`, `arr_delay`: Departure/arrival delay in minutes
* `origin`: Airport of origin (JFK, LGA, EWR)
* `precip`, `wind_speed`, `visib`, `temp`: Weather conditions

---

## 🧹 Data Cleaning & Preprocessing

* Filtered for JFK, LGA, and EWR flights only
* Removed canceled flights and rows with missing critical variables
* Excluded Dec 31 due to missing weather data
* Created categorized variables for temperature, wind speed, visibility, and precipitation

---

## 📊 Methods & Analysis

We applied both **exploratory** and **inferential** techniques:

### 🔎 Exploratory Data Analysis (EDA)

* Seasonal and monthly trends in volume and delay
* Heatmaps for average delay by weather conditions
* Distributions of volume, temperature, wind speed, and delays

### 📈 Statistical Testing

* **Kruskal-Wallis** tests for delay differences by airport, weather, and month
* **Chi-Square** tests for delay frequency vs. weather condition categories
* **Dunn’s Post-Hoc** comparisons for significant group differences
* **Kendall's Tau** correlation tests for delay vs. weather and volume
* **Multiple Linear Regression** models to predict delay from weather factors

---

## 📌 Key Findings

* **Summer months** (June–July) had the highest average delays
* **Weather conditions** like high temperature, strong wind, low visibility, and precipitation were significantly associated with increased delays
* **EWR** had the highest average delays; **JFK** was most affected during bad weather
* **Flight volume, average wind speed, and temperature** showed no meaningful correlation with delays

---

## ⚠️ Limitations

* Manual thresholds in weather categories may oversimplify conditions
* Aggregated daily weather data may miss short but impactful events
* Strong right-skew in delay data reduced statistical power in some analyses

---

## 👥 Team Contributions

* **Lydia Niu** (Captain): Question 4, Model exploration, Final compilation
* **Alexis Castaneda & Aparna Petluri**: Question 5, EDA plots, Model diagnostics
* **Gracelynne Mohan**: Question 6, Kruskal-Wallis & Dunn Tests
* **Jenny Zhang**: Questions 1 & 2, ANOVA/EDA visualizations
* **Zoe Shum**: Question 3, Heatmaps, Chi-Square Tests, Slide & Report organization

---

## 🛠️ How to Run

To reproduce the analysis:

1. Open `project.Rmd` or `.R` files in RStudio
2. Install required packages:

```r
install.packages(c("tidyverse", "ggplot2", "ggpubr", "rstatix", "dunn.test", "gridExtra"))
```

3. Knit the R Markdown file to view the full analysis and visuals

---

## 📚 Acknowledgments

* `nycflights13` dataset by the Tidyverse team
* STAT 167 Instructors and TAs
* RStudio and tidyverse packages
* AI tools such as ChatGTP and Grok for troubleshooting
