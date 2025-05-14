library(ggridges)
library(dplyr)
library(ggplot2)
library(readr)

# Question: Are there significant differences in average 
# delays/volume across the three airports and seasons?

# Import processed data, .rds
flights_clean <- read_rds("flights_clean.rds")

# First sub dataset: Summarize average delays and flight volume by airport and season
summary_stats <- flights_clean %>%
  group_by(origin, season) %>%
  summarise(
    avg_delay = mean(avg_delay, na.rm = TRUE),  # Mean delay per group
    flight_volume = n(),  # Number of flights (volume)
    median_delay = median(avg_delay, na.rm = TRUE)  # Median delay for robustness
  ) %>%
  ungroup()

# Inspect the summary
# print(summary_stats)


# 1. Ridge plot of average delays by airport, faceted by season
# Ridge plot stacks density curves for each airport and season, offering a compact view of distributions.
ggplot(flights_clean, aes(x = avg_delay, y = season, fill = origin)) +
  geom_density_ridges(alpha = 0.7) +
  facet_wrap(~ origin, ncol = 1) +
  coord_cartesian(xlim = c(-50, 200)) +
  scale_fill_manual(values = c("EWR" = "red", "JFK" = "green", "LGA" = "blue")) +
  labs(title = "Plot1: Ridge Plot of Average Delays by Season and Airport",
       x = "Average Delay (minutes)",
       y = "Season",
       fill = "Airport") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 2. Bar plot of mean delays using summary_stats
ggplot(summary_stats, aes(x = origin, y = avg_delay, fill = season)) +
  geom_col(position = "dodge") +
  labs(title = "Plot2: Mean Average Delay by Airport and Season",
       x = "Airport",
       y = "Mean Average Delay (minutes)",
       fill = "Season") +
  theme_minimal()

# 3. Bar plot of flight volume
ggplot(summary_stats, aes(x = origin, y = flight_volume, fill = season)) +
  geom_col(position = "dodge") +
  labs(title = "Plot3: Flight Volume by Airport and Season",
       x = "Airport",
       y = "Number of Flights",
       fill = "Season") +
  theme_minimal()

# second sub dataset: Summarize by airport, season, and day to get daily averages for scatter plot
daily_stats <- flights_clean %>%
  group_by(origin, season, month, day) %>%
  summarise(
    avg_delay = mean(avg_delay, na.rm = TRUE),
    flight_volume = n()
  ) %>%
  ungroup()

# 4. Scatter plot of volume vs delay
ggplot(daily_stats, aes(x = flight_volume, y = avg_delay, color = origin)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~season) +
  scale_color_manual(values = c("EWR" = "red", "JFK" = "blue", "LGA" = "black")) +
  labs(title = "Plot4: Flight Volume vs Average Delay by Season",
       x = "Daily Flight Volume", y = "Daily Average Delay (minutes)") +
  theme_minimal()


# 4. Scatter plot for flight volume/delayed time by season and airport.
# newest version: color-season, shape-airport
ggplot(daily_stats, aes(x = flight_volume, y = avg_delay, color = season, shape = origin)) +
  geom_point(size = 3, alpha = 0.8, stroke = 1.5) + 
  scale_shape_manual(values = c("EWR" = 16, "JFK" = 17, "LGA" = 15)) +  # Shapes for airports
  scale_color_manual(values = c("Fall" = "#E41A1C", "Spring" = "royalblue", "Summer" = "darkgreen", "Winter" = "yellow")) +  # Colors for seasons
  labs(title = "Plot5: Flight Volume vs Average Delay by Season and Airport",
       x = "Daily Flight Volume",
       y = "Daily Average Delay (minutes)",
       color = "Season",
       shape = "Airport") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.box = "vertical"
  )
