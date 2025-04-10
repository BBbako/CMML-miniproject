library(ggplot2)
library(tidyr)
library(dplyr)

#plot latency
latency_pc <- latency
latency_stats <- apply(latency, c(1,2), function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x))))


  day <- rep(1:Ndays, each = Ntrials)
  trial <- rep(1:Ntrials, times = Ndays)
  mean_latency <- as.vector(latency_stats[1,,])
  se_latency <- as.vector(latency_stats[2,,])

  latency_df_pc <- data.frame(
    Day = day,
    Trial = trial,
    MeanLatency = mean_latency,
    SE = se_latency
  )


  
  latency_day_pc <- latency_df_pc %>%
    group_by(Day) %>%
    summarise(
      latency = mean(MeanLatency),
      se = sd(MeanLatency) / sqrt(n())
    )


  ggplot(latency_day_pc, aes(x = Day, y = latency)) +
    geom_line(color = "steelblue") +
    geom_point(size = 2, color = "steelblue") +
    geom_errorbar(aes(ymin = latency - se, ymax = latency + se), width = 0.2, alpha = 0.4) +
    labs(
      title = "Latency Across Training Days (Place Cell Model)",
      x = "Training Day",
      y = "Average Latency (s)"
    ) +
    theme_minimal()

# plot target quadrant time
  
target_quadrant_pc <-target_quadrant
target_stats <- apply(target_quadrant, c(1,2), function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x))))


mean_target <- as.vector(target_stats[1,,])
se_target <- as.vector(target_stats[2,,])


day <- rep(1:Ndays, each = Ntrials)
trial <- rep(1:Ntrials, times = Ndays)

target_df_pc <- data.frame(
  Day = day,
  Trial = trial,
  MeanTarget = mean_target,
  SE = se_target
)


target_day_pc <- target_df_pc %>%
  group_by(Day) %>%
  summarise(
    TargetTime = mean(MeanTarget),
    se = sd(MeanTarget) / sqrt(n())
  )%>%
  mutate(Model = "Place Cell")

ggplot(target_day_pc, aes(x = Day, y = TargetTime)) +
  geom_line(color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  geom_errorbar(aes(ymin = TargetTime - se, ymax = TargetTime + se), width = 0.2, alpha = 0.4) +
  labs(
    title = "Target Quadrant Time Across Training Days",
    x = "Training Day",
    y = "Time in Target Quadrant (%)"
  ) +
  theme_minimal()

#plot opposite quandrant time

opposite_quadrant_pc <- opposite_quadrant
opposite_stats_pc <- apply(opposite_quadrant, c(1, 2), function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x))))
mean_opposite_pc <- as.vector(opposite_stats_pc[1,,])
se_opposite_pc <- as.vector(opposite_stats_pc[2,,])


day <- rep(1:Ndays, each = Ntrials)
trial <- rep(1:Ntrials, times = Ndays)

opposite_df_pc <- data.frame(
  Day = day,
  Trial = trial,
  MeanOpposite = mean_opposite_pc,
  SE = se_opposite_pc
)


opposite_day_pc <- opposite_df_pc %>%
  group_by(Day) %>%
  summarise(
    OppositeTime = mean(MeanOpposite),
    se = sd(MeanOpposite) / sqrt(n())
  ) %>%
  mutate(Model = "Place Cell")
