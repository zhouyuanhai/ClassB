# The changing landscape of major infectious diseases in China, 2004–2023

This repository contains the source code and curated datasets for the manuscript: **"The changing landscape of major infectious diseases in China, 2004–2023: a spatiotemporal and counterfactual analysis of the COVID-19 disruption"**.

## 1. System Requirements

### Hardware
* **Standard Desktop Computer**: The code requires a standard computer with enough RAM to support in-memory operations. No non-standard hardware (e.g., GPU clusters) is required.
* **OS**: Tested on Windows 10/11 (also compatible with macOS and Linux).

### Software Dependencies
The analysis was performed using **R (version 4.3.0)** and **Python (version 3.11)**.

#### R Packages:
* `dlookr`
* `forecast` 
* `ggplot2`
* `dplyr`
* "patchwork"

#### Python Packages:
* `pandas`
* `numpy`
* `tensorflow` / `keras` 
* `statsmodels`


## 2. Installation Guide

### Instructions
1.  Clone this repository:
    ```bash
    git clone [https://github.com/zhouyuanhai/ClassB.git](https://github.com/zhouyuanhai/ClassB.git)
    ```
2.  **R Setup**: Install required packages in R:
    ```r
    install.packages(c("dlookr", "forecast", "ggplot2", "dplyr"))
    ```
3.  **Python Setup**: Install dependencies via pip:
    ```bash
    pip install pandas numpy tensorflow statsmodels
    ```

### Typical Install Time
* Approximately **10-20 minutes** on a "normal" desktop computer (depending on internet speed).

## 3. Demo & Instructions for Use

### How to Run the Analysis
The analysis is divided into data cleaning, modeling, and visualization.

1.  **Data Cleaning**
2.  **Modeling**
3.  **Visualization**

### Expected Output
* The scripts will generate `.csv` files containing the ARD/RRD estimates.
* The visualization script will output files corresponding to Figures 1-6 in the paper.

### Expected Run Time
* The full pipeline takes approximately **20-60 minutes** on a standard desktop computer.

## 4. Reproduction Instructions
To fully reproduce the quantitative results reported in the manuscript, please ensure you use the `all_data.csv` provided in this repository and execute the scripts in the numerical order labeled above.
