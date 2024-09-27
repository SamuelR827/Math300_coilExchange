# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
ANXIETY <- read.csv("anxiety-cleaned.csv")

# 1. Calculate global average anxiety rate by year
global_trend <- ANXIETY %>%
  group_by(Year) %>%
  summarise(global_anxiety = mean(Anxiety, na.rm = TRUE))

# 2. Calculate global statistics (mean, median, std dev) for the entire dataset
global_stats <- ANXIETY %>%
  summarise(
    global_mean = mean(Anxiety, na.rm = TRUE),
    global_median = median(Anxiety, na.rm = TRUE),
    global_sd = sd(Anxiety, na.rm = TRUE)
  )

# Display global statistics
print("Global Anxiety Statistics (1990-2019):")
print(global_stats)

# 3. Plot the global anxiety trend over time
ggplot(global_trend, aes(x = Year, y = global_anxiety)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Global Anxiety Prevalence Trend (1990-2019)",
       x = "Year", y = "Average Anxiety Prevalence (%)") +
  theme_minimal()

# 4. Calculate statistics for individual countries
countries <- c('United States', 'India', 'China')

for (country in countries) {
  # Filter data for the selected country
  country_data <- ANXIETY %>%
    filter(Entity == country)
  
  # Calculate the mean, median, and standard deviation for each country
  country_stats <- country_data %>%
    summarise(
      country_mean = mean(Anxiety, na.rm = TRUE),
      country_median = median(Anxiety, na.rm = TRUE),
      country_sd = sd(Anxiety, na.rm = TRUE)
    )
  
  # Print the statistics for each country
  print(paste("Anxiety Statistics for", country, "(1990-2019):"))
  print(country_stats)
  
  # 5. Plot the trend for the selected country
  p <- ggplot(country_data, aes(x = Year, y = Anxiety)) +
    geom_line(color = "green", size = 1) +
    labs(title = paste("Anxiety Trend in", country, "(1990-2019)"),
         x = "Year", y = "Anxiety Prevalence (%)") +
    theme_minimal()
  
  # Explicitly print the plot within the loop
  print(p)
}

# 6. Box-Whisker plot for global anxiety trends
ggplot(ANXIETY, aes(x = as.factor(Year), y = Anxiety)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Global Anxiety Distribution (1990-2019)",
       x = "Year", y = "Anxiety Prevalence (%)") +
  theme_minimal()

# 7. Box-Whisker plot for individual countries
for (country in countries) {
  country_data <- ANXIETY %>%
    filter(Entity == country)
  
  # Create the box-whisker plot for the selected country
  p <- ggplot(country_data, aes(x = as.factor(Year), y = Anxiety)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = paste("Anxiety Distribution in", country, "(1990-2019)"),
         x = "Year", y = "Anxiety Prevalence (%)") +
    theme_minimal()
  
  # Explicitly print the plot within the loop
  print(p)
}

