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

- `MWMdata.xlsx`:  
  Real experimental behavioral data from C57BL/6 mice used for model validation and correlation analysis. Data explanation:
  #Condition: 1 - fixed hidden platform (standard WM), 2 - variable hidden platform (randomly changing between 2 opposite locations)
  #Behavioural variables: 1 - latency (s), 2 - proportion of time in target quadrant (not including wall zone), 3 - proportion of time in wall zone, 4 - swim speed (cm/s)
  #5 - proportion of distance in target quadrant, 6 - proportion of distance in wall zone, 7 - total swim distance (cm), 8 - cumulative distance to platform (cm),
  #9 - proportion of distance in opposite quadrant, 10 - proportion of time in opposite quadrant, 11 - mean absolute turning angle, 12 - swim speed st.d.
  #Strain: 1 - C57BL/6, 2 - DBA/2

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
4. Make sure the file `MWMdata.xlsx` is in the same directory.

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
├── MWMdata.xlsx          # Real behavioral data
├── figures/                 # Output figures (optional)
└── README.md                # This file
```

---

---
