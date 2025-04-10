library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

experimental_data <- read_excel("MWMdata.xlsx")
colnames(experimental_data)[1] <- "Variable"
# latency (1), target quadrant time (2), opposite quadrant time (10)
behavior_vars <- list(
  latency = 1,
  target = 2,
  opposite = 10
)


data_c57 <- experimental_data %>%
  filter(Condition == 2, Strain == 1)


plot_behavior <- function(df, varcode, varname, ylabel) {
  df_var <- df %>%
    filter(Variable == varcode) %>%
    group_by(Day) %>%
    summarise(
      mean_value = mean(Value, na.rm = TRUE),
      se = sd(Value, na.rm = TRUE) / sqrt(n())
    )

  ggplot(df_var, aes(x = Day, y = mean_value)) +
    geom_line(color = "steelblue") +
    geom_point(size = 2, color = "steelblue") +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se),
                  width = 0.2, alpha = 0.4) +
    labs(
      title = paste(varname, "Across Training Days (C57BL/6, Variable Platform)"),
      x = "Training Day",
      y = ylabel
    ) +
    theme_minimal()
}


plot_behavior(data_c57, behavior_vars$latency, "Latency", "Latency (s)")
plot_behavior(data_c57, behavior_vars$target, "Target Quadrant Time", "Proportion in Target Quadrant")
plot_behavior(data_c57, behavior_vars$opposite, "Opposite Quadrant Time", "Proportion in Opposite Quadrant")
