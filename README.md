---

# 🧭 Spatial Learning Models in Variable Platform Water Maze Task

This project compares two reinforcement learning (RL) models — a **place cell-based model** and a **distance cell-based model** — in a simulated Morris Water Maze task where platform locations vary randomly across trials. The goal is to investigate how well each model captures spatial learning patterns and aligns with empirical data from **C57BL/6 mice**.

---

## 🎯 Project Objectives

- Compare the performance of place cell and distance cell models in a spatial task with variable platform locations.
- Identify **if and when** the place model outperforms the distance model under certain parameter settings.
- Evaluate how well each model replicates experimental behavior, using:
  - **Latency** to find the platform  
  - **Time in target quadrant**  
  - **Time in opposite quadrant**

---

## 📂 Repository Contents

- **`model.Rmd`**  
  Main R Markdown file that:
  - Implements both RL models
  - Runs simulations under variable platform conditions
  - Performs statistical comparisons
  - Generates all key plots and summary statistics

- **`MWMdata.xlsx`**  
  Experimental behavioral data from C57BL/6 mice in the same task.  
  Key metadata (from file header):
  - `Condition`: 1 = fixed platform; 2 = variable platform  
  - `Strain`: 1 = C57BL/6, 2 = DBA/2  
  - `Variables`: latency, quadrant time, swim speed, distance metrics, etc.

- **`Seperate parts of model/`**  
  Optional R scripts containing code for each section. Useful for debugging and extension.

- **Output figures**  
  Automatically generated via `model.Rmd`. Used in the final report for comparison with empirical results.

---

## 📊 Key Results

- The **distance model** learns faster and shows closer correlation with real data in **latency** and **opposite quadrant time**.
- After parameter tuning, the **place model** demonstrates more goal-specific search and **outperforms** the distance model in later stages.
- Neither model fully reproduces the gradual increase in **target quadrant preference** observed in real mice behavior.

---

## ▶️ How to Run

1. **Clone or download** the repository.
2. Open `model.Rmd` in **RStudio**.
3. Ensure `MWMdata.xlsx` is in the same folder.
4. Click **"Knit to PDF"** to run full simulations and generate outputs.

**Required R packages**:
```r
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggpubr)
library(readxl)
```

---

## 🧱 Directory Structure

```
├── model.Rmd                  # Main simulation + analysis script
├── MWMdata.xlsx              # Real mouse behavioral data
├── Seperate parts of model/  # Modular R scripts (optional use)
├── figures/                  # Output figures (optional folder)
└── README.md                 # This file
```

---
