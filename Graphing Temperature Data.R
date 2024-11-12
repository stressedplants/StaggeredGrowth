### Libraries 
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(ggExtra)
library(readxl)

### Data
tiny_tag <- read_excel("Raw Data/tiny_tag_temp_data.xlsx", sheet = 1)

# Seperate the data
grow1 <- which(tiny_tag$Time > make_date(2024, 4, 29) & tiny_tag$Time < make_date(2024, 4, 29)+21) 
grow2 <- which(tiny_tag$Time > make_date(2024, 5, 06) & tiny_tag$Time < make_date(2024, 5, 06)+21)
grow3 <- which(tiny_tag$Time > make_date(2024, 5, 13) & tiny_tag$Time < make_date(2024, 5, 13)+21)
grow4 <- which(tiny_tag$Time > make_date(2024, 5, 20) & tiny_tag$Time < make_date(2024, 5, 20)+21)
grow5 <- which(tiny_tag$Time > make_date(2024, 5, 27) & tiny_tag$Time < make_date(2024, 5, 27)+21)
grow6 <- which(tiny_tag$Time > make_date(2024, 6, 03) & tiny_tag$Time < make_date(2024, 6, 03)+21)

# Apply new values to the data
grow1_tag <- tiny_tag$Temperature[min(grow1):max(grow1)]
grow2_tag <- tiny_tag$Temperature[min(grow2):max(grow2)]
grow3_tag <- tiny_tag$Temperature[min(grow3):max(grow3)]
grow4_tag <- tiny_tag$Temperature[min(grow4):max(grow4)]
grow5_tag <- tiny_tag$Temperature[min(grow5):max(grow5)]
grow6_tag <- tiny_tag$Temperature[min(grow6):max(grow6)]

### Create stats on the data
# Mean
grow1_mean <- mean(grow1_tag)
grow2_mean <- mean(grow2_tag)
grow3_mean <- mean(grow3_tag)
grow4_mean <- mean(grow4_tag)
grow5_mean <- mean(grow5_tag)
grow6_mean <- mean(grow6_tag)

# Median
grow1_median <- median(grow1_tag)
grow2_median <- median(grow2_tag)
grow3_median <- median(grow3_tag)
grow4_median <- median(grow4_tag)
grow5_median <- median(grow5_tag)
grow6_median <- median(grow6_tag)

# Standard Deviation
grow1_sd <- sd(grow1_tag)
grow2_sd <- sd(grow2_tag)
grow3_sd <- sd(grow3_tag)
grow4_sd <- sd(grow4_tag)
grow5_sd <- sd(grow5_tag)
grow6_sd <- sd(grow6_tag)

# Min 
grow1_min <- min(grow1_tag)
grow2_min <- min(grow2_tag)
grow3_min <- min(grow3_tag)
grow4_min <- min(grow4_tag)
grow5_min <- min(grow5_tag)
grow6_min <- min(grow6_tag)

# Max
grow1_max <- max(grow1_tag)
grow2_max <- max(grow2_tag)
grow3_max <- max(grow3_tag)
grow4_max <- max(grow4_tag)
grow5_max <- max(grow5_tag)
grow6_max <- max(grow6_tag)

# Create a dataframe
# grow_data <- data.frame(
  grow = c("Grow 1", "Grow 2", "Grow 3", "Grow 4", "Grow 5", "Grow 6"),
  mean = c(grow1_mean, grow2_mean, grow3_mean, grow4_mean, grow5_mean, grow6_mean),
  median = c(grow1_median, grow2_median, grow3_median, grow4_median, grow5_median, grow6_median),
  sd = c(grow1_sd, grow2_sd, grow3_sd, grow4_sd, grow5_sd, grow6_sd),
  min = c(grow1_min, grow2_min, grow3_min, grow4_min, grow5_min, grow6_min),
  max = c(grow1_max, grow2_max, grow3_max, grow4_max, grow5_max, grow6_max)
)


### Data
tiny_tag <- read_excel("Raw Data/tiny_tag_temp_data.xlsx", sheet = 1)

# Create a long-format data frame for the different growth phases
grow1_tag <- tiny_tag %>% filter(Time > make_date(2024, 4, 29) & Time < make_date(2024, 4, 29) + 21) %>%
  mutate(growth_phase = "Grow 1")

grow2_tag <- tiny_tag %>% filter(Time > make_date(2024, 5, 06) & Time < make_date(2024, 5, 06) + 21) %>%
  mutate(growth_phase = "Grow 2")

grow3_tag <- tiny_tag %>% filter(Time > make_date(2024, 5, 13) & Time < make_date(2024, 5, 13) + 21) %>%
  mutate(growth_phase = "Grow 3")

grow4_tag <- tiny_tag %>% filter(Time > make_date(2024, 5, 20) & Time < make_date(2024, 5, 20) + 21) %>%
  mutate(growth_phase = "Grow 4")

grow5_tag <- tiny_tag %>% filter(Time > make_date(2024, 5, 27) & Time < make_date(2024, 5, 27) + 21) %>%
  mutate(growth_phase = "Grow 5")

grow6_tag <- tiny_tag %>% filter(Time > make_date(2024, 6, 03) & Time < make_date(2024, 6, 03) + 21) %>%
  mutate(growth_phase = "Grow 6")

# Combine the data
all_grow_data <- bind_rows(grow1_tag, grow2_tag, grow3_tag, grow4_tag, grow5_tag, grow6_tag)

# Create a boxplot with ggplot2
temp_box_plot <- ggplot(all_grow_data, aes(x = growth_phase, y = Temperature, fill = growth_phase)) +
  geom_boxplot(coef = 3) +
  theme_classic() +
  labs(title = "Temperature Boxplot by Growth Phase", x = "Growth Phase", y = "Temperature") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red", fill = "red") +
  scale_fill_brewer(palette = "Set2") 

## Save temp_box_plot to file
ggsave("Processed Data/temp_box_plot.png", plot = temp_box_plot, width = 10, height = 6, units = "in", dpi = 300)

### Bar chart for mean temperature at each growth stage, with min and max temperature overlayed
gplot(grow_data, aes(x = grow, y = mean, fill = grow)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Mean Temperature at Each Growth Stage", x = "Growth Stage", y = "Temperature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")
######################################################

# Load the data from Excel
tiny_tag <- read_excel("Raw Data/tiny_tag_temp_data.xlsx", sheet = 1)

# Ensure that the Time column is in POSIXct format for date-time manipulation
tiny_tag$Time <- as.POSIXct(tiny_tag$Time, format = "%Y-%m-%d %H:%M:%S")

# Growth periods based on dates
growth_periods <- list(
  grow1 = tiny_tag %>% filter(Time >= as.Date("2024-04-29") & Time < as.Date("2024-05-19")),
  grow2 = tiny_tag %>% filter(Time >= as.Date("2024-05-06") & Time < as.Date("2024-05-26")),
  grow3 = tiny_tag %>% filter(Time >= as.Date("2024-05-13") & Time < as.Date("2024-06-02")),
  grow4 = tiny_tag %>% filter(Time >= as.Date("2024-05-20") & Time < as.Date("2024-06-09")),
  grow5 = tiny_tag %>% filter(Time >= as.Date("2024-05-27") & Time < as.Date("2024-06-16")),
  grow6 = tiny_tag %>% filter(Time >= as.Date("2024-06-03") & Time < as.Date("2024-06-23"))
)

# Helper function to classify time into Day and Evening
classify_day_evening <- function(hour) {
  ifelse(hour >= 6 & hour < 18, "Day", "Evening")
}

# Add classification to each growth period
growth_periods <- lapply(growth_periods, function(df) {
  df$Day_Evening <- classify_day_evening(hour(df$Time))
  return(df)
})

# Function to calculate stats for each growth period
calculate_stats <- function(df) {
  avg_temp <- mean(df$Temperature, na.rm = TRUE)
  peak_temp <- max(df$Temperature, na.rm = TRUE)
  trough_temp <- min(df$Temperature, na.rm = TRUE)
  diff_peak_trough <- peak_temp - trough_temp
  time_peak_temp <- df$Time[which.max(df$Temperature)]
  
  avg_day_temp <- mean(df$Temperature[df$Day_Evening == "Day"], na.rm = TRUE)
  avg_evening_temp <- mean(df$Temperature[df$Day_Evening == "Evening"], na.rm = TRUE)
  
  return(data.frame(
    avg_temp = avg_temp,
    peak_temp = peak_temp,
    trough_temp = trough_temp,
    diff_peak_trough = diff_peak_trough,
    time_peak_temp = time_peak_temp,
    avg_day_temp = avg_day_temp,
    avg_evening_temp = avg_evening_temp
  ))
}

# Apply the function to each growth period
results <- lapply(growth_periods, calculate_stats)

# Combine results into a single data frame
results_df <- bind_rows(results, .id = "Growth_Phase")
results_df

# Line Plot of peak and trough values with sd error bars
peak_and_trough <- ggplot(results_df, aes(x = Growth_Phase)) +
  geom_point(aes(y = peak_temp, color = "Peak Temp"), size = 3) +
  geom_point(aes(y = trough_temp, color = "Lowest Temp"), size = 3) +
  geom_errorbar(aes(ymin = trough_temp - sd(trough_temp), ymax = trough_temp + sd(trough_temp), color = "Lowest Temp"), width = 0.2) +
  geom_errorbar(aes(ymin = peak_temp - sd(peak_temp), ymax = peak_temp + sd(peak_temp), color = "Peak Temp"), width = 0.2) +
  labs(title = "Highest and Lowest Temperatures per Growth Phase",
       x = "Growth Phase", y = "Temperature") +
  scale_color_manual(values = c("Peak Temp" = "red", "Lowest Temp" = "blue")) +
  theme_classic()

peak_and_trough

# Bar chart of average temperature during day and evening with sd bar
avg_day_evening <- ggplot(results_df, aes(x = Growth_Phase)) +
  geom_bar(aes(y = avg_day_temp, fill = "Day Temp"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = avg_evening_temp, fill = "Evening Temp"), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = avg_day_temp - sd(avg_day_temp), ymax = avg_day_temp + sd(avg_day_temp), fill = "Day Temp"), width = 0.2, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = avg_evening_temp - sd(avg_evening_temp), ymax = avg_evening_temp + sd(avg_evening_temp), fill = "Evening Temp"), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Average Temperature during Day and Evening per Growth Phase",
       x = "Growth Phase", y = "Temperature") +
  scale_fill_manual(values = c("Day Temp" = "orange", "Evening Temp" = "purple")) +
  theme_classic()

avg_day_evening

# Save the plots to files
ggsave("Processed Data/peak_and_trough.png", plot = peak_and_trough, width = 10, height = 6, units = "in", dpi = 300)
ggsave("Processed Data/avg_day_evening.png", plot = avg_day_evening, width = 10, height = 6, units = "in", dpi = 300)


### Notes for future 
### - Graph temp data for first and last week of each growth phase
### - Generate table data on the graphed data 