---

# Spatial Learning Models in Variable Platform Water Maze Task

This project compares two reinforcement learning models — a **place cell-based model** and a **distance cell-based model** — in a Morris Water Maze task with **variable platform positions**. The aim is to investigate how these models reproduce spatial learning behavior and how well they align with **experimental data from C57BL/6 mice**.

##  Project Goals

1. **Compare performance** of place-cell and distance-cell models in a spatial learning task where platform locations vary between trials.
2. **Test if and when** the place model can outperform the distance model.
3. **Evaluate** which model better reproduces patterns observed in real experimental data, using metrics like:
   - **Latency**
   - **Target quadrant time**
   - **Opposite quadrant time**

---

##  Contents

- `model.Rmd`:  
  R Markdown script that implements both models, runs simulations, generates plots, and performs statistical comparisons.

- `WMdataLong.xlsx`:  
  Real experimental behavioral data from C57BL/6 mice used for model validation and correlation analysis.

- Output figures:  
  Included in the R Markdown output and used in the final report for comparing model and experimental outcomes.

---

## 📊 Key Results

- **Distance model** learns faster and aligns more closely with experimental latency and opposite quadrant time.
- **Place model**, with tuned parameters, shows improved **goal-specific search** and eventually outperforms the distance model in later training days.
- Neither model fully captures the **gradual increase** in target quadrant preference seen in mice.

---

## 🧠 How to Use

1. Clone or download this repository.
2. Open `model.Rmd` in RStudio.
3. Click **"Knit to PDF"** to run simulations and generate visualizations.
4. Make sure the file `WMdataLong.xlsx` is in the same directory.

### Required R packages:

```r
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggpubr)
library(readxl)
```

---

## 📁 Structure

```
├── model.Rmd                # Main simulation and analysis script
├── WMdataLong.xlsx          # Real behavioral data
├── figures/                 # Output figures (optional)
└── README.md                # This file
```

---

---
