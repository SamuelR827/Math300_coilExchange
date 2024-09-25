# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
ANXIETY <- read.csv("anxiety-cleaned.csv")

# Calculate global average anxiety rate by year
global_trend <- ANXIETY %>%
  group_by(Year) %>%
  summarise(global_anxiety = mean(Anxiety, na.rm = TRUE))

# Plot the global anxiety trend over time
ggplot(global_trend, aes(x = Year, y = global_anxiety)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Global Anxiety Prevalence Trend (1990-2019)",
       x = "Year", y = "Average Anxiety Prevalence (%)") +
  theme_minimal()

