library(ggplot2)
library(tidyr)
library(dplyr)


#plot latency comparison
latency_day_pc$Model <- "Place Cell"
latency_day_dc$Model <- "Distance Cell"
latency_compare <- rbind(latency_day_pc, latency_day_dc)


ggplot(latency_compare, aes(x = Day, y = latency, color = Model)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = latency - se, ymax = latency + se), width = 0.2, alpha = 0.3) +
  labs(
    title = "Latency Comparison",
    x = "Training Day",
    y = "Average Latency (s)"
  ) +
  scale_color_manual(values = c("Place Cell" = "steelblue", "Distance Cell" = "darkorange")) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = c("Distance Cell" = "orange", "Place Cell" = "steelblue")) +
  annotate("text", x = 2, y = 24.5, label = "*", size = 6)


#plot target quadrant time comparison
target_compare <- rbind(target_day_pc, target_day_dc)

ggplot(target_compare, aes(x = Day, y = TargetTime, color = Model)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = TargetTime - se, ymax = TargetTime + se), width = 0.2, alpha = 0.3) +
  labs(
    title = "Target Quadrant Time Comparison",
    x = "Training Day",
    y = "Time in Target Quadrant (%)"
  ) +
  scale_color_manual(values = c("Place Cell" = "steelblue", "Distance Cell" = "darkorange")) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = c("Distance Cell" = "orange", "Place Cell" = "steelblue")) +
  annotate("text", x = 1:8, y = rep(15.5, 8), label = rep("*", 8), size = 5)

#plot opposite quadrant time comparison

opposite_compare <- rbind(opposite_day_pc, opposite_day_dc)

ggplot(opposite_compare, aes(x = Day, y = OppositeTime, color = Model)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = OppositeTime - se, ymax = OppositeTime + se), width = 0.2, alpha = 0.3) +
  labs(
    title = "Opposite Quadrant Time Comparison",
    x = "Training Day",
    y = "Time in Opposite Quadrant (%)"
  ) +
  scale_color_manual(values = c("Place Cell" = "steelblue", "Distance Cell" = "darkorange")) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = c("Distance Cell" = "orange", "Place Cell" = "steelblue")) +
  annotate("text", x = 2, y = 14.5, label = "**", size = 5) +
  annotate("text", x = 4, y = 14.5, label = "***", size = 5) +
  annotate("text", x = 5, y = 14.5, label = "**", size = 5) +
  annotate("text", x = 6, y = 14.5, label = "***", size = 5) +
  annotate("text", x = 7, y = 14.5, label = "*", size = 5) +
  annotate("text", x = 8, y = 14.5, label = "**", size = 5)
