# Port Terminal Literature Review — Data Visualization Project

## Overview

This project analyzes and visualizes data extracted from a literature review focused on:

- Port terminals
- Optimization methods
- Simulation paradigms
- Validation methodologies
- Sustainability considerations
- Geographic distribution of studies

The analysis was developed in **RStudio** using data stored in an Excel spreadsheet.

---

# Project Structure

```text
project/
│
├── data/
│   └── data.xlsx
│
├── outputs/
│   ├── paradigms.png
│   ├── tools_software.png
│   ├── terminal_type.png
│   ├── regions.png
│   └── ...
│
├── scripts/
│   └── analysis.R
│
└── README.md
```

---

# Technologies Used

## Programming Language

- R

## Main Libraries

```r
library(readxl)
library(ggplot2)
library(forcats)
library(RColorBrewer)
```

---

# Data Source

The dataset was imported from an Excel file:

```r
read_excel("data/data.xlsx", skip = 2)
```

The database contains information extracted from scientific articles related to:

- Terminal operations
- Optimization approaches
- Simulation tools
- Validation techniques
- Sustainability analysis
- Geographic distribution

---

# Visualizations Included

The project includes multiple visualizations such as:

## Bar Charts

Used for:

- Main paradigms
- Optimization methods
- Validation methods
- Main processes
- Sustainability analysis
- Economic classification
- Country frequency

## Pie Charts

Used for:

- Region distribution
- Time horizon distribution
- Validation proportion

---

# Example Graph

```r
ggplot(tabla, aes(y = fct_infreq(`Tool / Software`), fill = `Tool / Software`)) +
  geom_bar() +
  theme_minimal()
```

---

# Generated Outputs

All figures are automatically exported into the `outputs/` folder using:

```r
ggsave("outputs/example.png")
```

---

# How to Run the Project

## 1. Install Required Packages

```r
install.packages("readxl")
install.packages("ggplot2")
install.packages("forcats")
install.packages("RColorBrewer")
```

## 2. Open RStudio

Open the project folder in RStudio.

## 3. Run the Script

Execute:

```r
source("scripts/analysis.R")
```

All graphs will be generated automatically inside the `outputs/` directory.

---

# Main Objectives

This project aims to:

- Identify dominant optimization approaches
- Analyze validation methodologies
- Detect regional research trends
- Explore sustainability considerations
- Compare operational paradigms
- Summarize patterns found in the literature

---

# Authors
Sebastián González Rossi 

# License

This project is intended for academic and educational purposes.
