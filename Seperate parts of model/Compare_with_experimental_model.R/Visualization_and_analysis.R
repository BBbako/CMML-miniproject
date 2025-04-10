library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(pbkrtest)
library(readxl)


exp_latency <- data_c57 %>%
  filter(Variable == 1) %>%
  group_by(Day) %>%
  summarise(
    Latency = mean(Value, na.rm = TRUE),
    SE = sd(Value, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(Model = "Actual", Source = "Experiment")

latency_day_pc$Model <- "Place Cell"
latency_day_dc$Model <- "Distance Cell"

latency_day_pc$Source <- "Simulation"
latency_day_dc$Source <- "Simulation"


combined_latency <- bind_rows(
  latency_day_pc %>% rename(Latency = latency),
  latency_day_dc %>% rename(Latency = latency),
  exp_latency
)
ggplot(combined_latency, aes(x = Day, y = Latency, color = Model, linetype = Source)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Latency - SE, ymax = Latency + SE), width = 0.2, alpha = 0.4) +
  labs(title = "Latency Comparison: Model vs Experiment",
       x = "Training Day",
       y = "Latency (s)") +
  theme_minimal() +
  scale_color_manual(values = c("Place Cell" = "steelblue", "Distance Cell" = "orange", "Actual" = "black")) +
  scale_linetype_manual(values = c("Simulation" = "solid", "Experiment" = "dashed")) +
  theme(plot.title = element_text(hjust = 0.5))


exp_target <- data_c57 %>%
  filter(Variable == 2) %>%
  group_by(Day) %>%
  summarise(
    TargetTime = mean(Value, na.rm = TRUE),
    SE = sd(Value, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(Model = "Actual", Source = "Experiment")


exp_opposite <- data_c57 %>%
  filter(Variable == 10) %>%
  group_by(Day) %>%
  summarise(
    OppositeTime = mean(Value, na.rm = TRUE),
    SE = sd(Value, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(Model = "Actual", Source = "Experiment")


exp_target <- exp_target %>%
  mutate(TargetTime = TargetTime * 100, SE = SE * 100)

exp_opposite <- exp_opposite %>%
  mutate(OppositeTime = OppositeTime * 100, SE = SE * 100)


target_day_pc$Model <- "Place Cell"
target_day_dc$Model <- "Distance Cell"
target_day_pc$Source <- "Simulation"
target_day_dc$Source <- "Simulation"

opposite_day_pc$Model <- "Place Cell"
opposite_day_dc$Model <- "Distance Cell"
opposite_day_pc$Source <- "Simulation"
opposite_day_dc$Source <- "Simulation"


target_day_pc <- rename(target_day_pc, TargetTime = TargetTime)
target_day_dc <- rename(target_day_dc, TargetTime = TargetTime)

opposite_day_pc <- rename(opposite_day_pc, OppositeTime = OppositeTime)
opposite_day_dc <- rename(opposite_day_dc, OppositeTime = OppositeTime)
# Target Time
combined_target <- bind_rows(
  target_day_pc,
  target_day_dc,
  exp_target
)

# Opposite Time
combined_opposite <- bind_rows(
  opposite_day_pc,
  opposite_day_dc,
  exp_opposite
)
ggplot(combined_target, aes(x = Day, y = TargetTime, color = Model, linetype = Source)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = TargetTime - se, ymax = TargetTime + se), width = 0.2, alpha = 0.4) +
  labs(title = "Target Quadrant Time: Model vs Experiment",
       x = "Training Day",
       y = "Time in Target Quadrant (%)") +
  theme_minimal() +
  scale_color_manual(values = c("Place Cell" = "steelblue", "Distance Cell" = "orange", "Actual" = "black")) +
  scale_linetype_manual(values = c("Simulation" = "solid", "Experiment" = "dashed")) +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(combined_opposite, aes(x = Day, y = OppositeTime, color = Model, linetype = Source)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = OppositeTime - se, ymax = OppositeTime + se), width = 0.2, alpha = 0.4) +
  labs(title = "Opposite Quadrant Time: Model vs Experiment",
       x = "Training Day",
       y = "Time in Opposite Quadrant (%)") +
  theme_minimal() +
  scale_color_manual(values = c("Place Cell" = "steelblue", "Distance Cell" = "orange", "Actual" = "black")) +
  scale_linetype_manual(values = c("Simulation" = "solid", "Experiment" = "dashed")) +
  theme(plot.title = element_text(hjust = 0.5))


cor(latency_day_pc$latency, exp_latency$Latency)
cor(latency_day_dc$latency, exp_latency$Latency)
# Target quadrant time
cor(target_day_pc$TargetTime, exp_target$TargetTime)
cor(target_day_dc$TargetTime, exp_target$TargetTime)

# Opposite quadrant time
cor(opposite_day_pc$OppositeTime, exp_opposite$OppositeTime)
cor(opposite_day_dc$OppositeTime, exp_opposite$OppositeTime)
