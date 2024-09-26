
# Load necessary library
library(ggplot2)

# Original data
adult_tone_survival_data <- data.frame(
  Temperature = c(12.5, 15.0, 17.5, 20.0, 22.5, 25.0, 27.5, 30.0, 32.5, 35.0),
  Female_longevity = c(0, 21, 46.7, 26.8, 28.2, 18.0, 14.2, 13.3, 11.7, 13.4)
)

adult_tjap_survival_data <- data.frame(
  Temperature = c(16,21,26,31),
  Female_longevity = c(102,66,30.4,28)
)

# Fit a polynomial model of degree 3
adult_tjap_survival_model <- lm(Female_longevity ~ poly(Temperature, 2, raw=TRUE), data = adult_tjap_survival_data)
adult_tone_survival_model <- lm(Female_longevity ~ poly(Temperature, 2, raw=TRUE), data = adult_tone_survival_data)

# ###data from kaplan - meier curves
#
# time_points <- c(0, 20, 40, 60, 80, 100, 120, 140, 160)
# survival_probabilities <- c(1, 0.9, 0.75, 0.6, 0.45, 0.3, 0.15, 0.05, 0)
#
# # Create a data frame with the time points and survival probabilities for each temperature
# time_points <- seq(0, 160, by = 20)
#
# # Hypothetical survival probabilities for each temperature
# survival_probabilities_16 <- c(1, 1, 1, 0.8, 0.8, 0.5, 0.25, 0.25, 0)
# survival_probabilities_21 <- c(1, 1, 0.9, 0.8, 0.1, 0, 0, 0, 0)
# survival_probabilities_26 <- c(1, 0.9, 0.12, 0, 0, 0, 0, 0, 0)
# survival_probabilities_31 <- c(1, 0.9, 0, 0, 0, 0, 0, 0, 0)
#
# # Combine into a data frame
# adult_tjap_survival_data2 <- data.frame(
#   Time = rep(time_points, 4),
#   Temperature = rep(c(16, 21, 26, 31), each = length(time_points)),
#   Survival = c(survival_probabilities_16, survival_probabilities_21, survival_probabilities_26, survival_probabilities_31)
# )
#
#
#
# # Function to calculate mean longevity using trapezoidal rule
# calculate_mean_longevity <- function(time, survival) {
#   auc <- sum(diff(time) * (head(survival, -1) + tail(survival, -1)) / 2)
#   return(auc)
# }
#
# # Calculate mean longevity for each temperature
# mean_longevity_16 <- calculate_mean_longevity(time_points, survival_probabilities_16)
# mean_longevity_21 <- calculate_mean_longevity(time_points, survival_probabilities_21)
# mean_longevity_26 <- calculate_mean_longevity(time_points, survival_probabilities_26)
# mean_longevity_31 <- calculate_mean_longevity(time_points, survival_probabilities_31)
#
# # Print mean longevity for each temperature
# mean_longevity <- data.frame(
#   Temperature = c(16, 21, 26, 31),
#   Mean_Longevity = c(mean_longevity_16, mean_longevity_21, mean_longevity_26, mean_longevity_31)
# )
#
# print(mean_longevity)




# # Define the range of temperatures for prediction
temperature_range <- seq(5, 30, by = 0.5)
#
# Calculate survival probabilities for the range of temperatures
survival_probabilities <- sapply(temperature_range, function(T) calculate_adult_tjap_survival(T, adult_tjap_survival_model))

# Create a data frame with the results
survival_results <- data.frame(Temperature = temperature_range, Daily_Survival_Probability = survival_probabilities)
#
# # Plot the results
ggplot(survival_results, aes(x = Temperature, y = Daily_Survival_Probability)) +
  geom_line(size = 1, color = "black") +
  labs(
    x = "Temperature (°C)",
    y = "Daily Survival Probability"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5)
  )
#
#
ggsave("graphs/tjap_daily_survival_prob.png", width = 8, height = 6, dpi = 300)


calculate_adult_tjap_survival(10, adult_tjap_survival_model)
