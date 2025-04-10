library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(pbkrtest)


extract_long_df <- function(PMs_array, measure_name) {
  values <- as.vector(PMs_array)
  day <- rep(rep(1:Ndays, each = Ntrials), times = Nruns * 2)
  trial <- rep(rep(1:Ntrials, times = Ndays), times = Nruns * 2)
  run <- rep(1:Nruns, each = Ndays * Ntrials, times = 2)
  model <- rep(c("Place Cell", "Distance Cell"), each = Ndays * Ntrials * Nruns)
  
  data.frame(
    Run = factor(run),
    Day = factor(day),
    Trial = trial,
    Model = factor(model),
    Value = values,
    Measure = measure_name
  )
}


df_latency <- extract_long_df(rbind(latency_pc, latency_dc), "Latency")
df_target  <- extract_long_df(rbind(opposite_quadrant_pc, opposite_quadrant_dc), "Target")
df_opposite <- extract_long_df(rbind(target_quadrant_pc, target_dc), "Opposite")

# ===== LATENCY =====
cat("===== LATENCY ANALYSIS =====\n")
mod_latency <- lmer(Value ~ Day * Model + (1|Run), data = df_latency)
summary(mod_latency)


em_latency <- emmeans(mod_latency, ~ Model | Day)
pairs(em_latency)

# ===== TARGET QUADRANT TIME =====
cat("\n===== TARGET QUADRANT TIME ANALYSIS =====\n")
mod_target <- lmer(Value ~ Day * Model + (1|Run), data = df_target)
summary(mod_target)
em_target <- emmeans(mod_target, ~ Model | Day)
pairs(em_target)

# ===== OPPOSITE QUADRANT TIME =====
cat("\n===== OPPOSITE QUADRANT TIME ANALYSIS =====\n")
mod_opposite <- lmer(Value ~ Day * Model + (1|Run), data = df_opposite)
summary(mod_opposite)
em_opposite <- emmeans(mod_opposite, ~ Model | Day)
pairs(em_opposite)
