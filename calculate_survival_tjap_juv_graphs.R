# Load necessary libraries
library(stats)
library(ggplot2)

# Define the data frame with temperature and emerged adults percentage
temperature_data <- data.frame(
  Temperature = c(18, 22, 25, 28, 31, 34),
  Emerged_Adults_Percentage = c(89.1, 98.1, 97.3, 98.2, 95.2, 88.9)
  )

# Convert percentages to proportions
Emerged_Adults_Proportion <- temperature_data$Emerged_Adults_Percentage / 100

# Add the proportions vector to the dataframe
temperature_data$Emerged_Adults_Proportion <- Emerged_Adults_Proportion

print(temperature_data)


params_juv_dev <- list(
  juv_T0 = 12.43,
  juv_Tm = 35,
  juv_coef_a = 0.0065,
  juv_coef_b = 0.0808
)

params_juv_surv <- list(
  juv_a = -0.013861228,
  juv_b = 0.077445952,
  juv_c = -0.001496
)

# Fit the polynomial regression model for emergence percentage
emerged_adults_poly <- lm(Emerged_Adults_Proportion ~ poly(Temperature, 2, raw=TRUE), data=temperature_data)
coefficients_tjap_juv_survival <- coef(emerged_adults_poly)

print(coefficients_tjap_juv_survival)


# Define temperature range
temperature_range <- seq(5, 30, by = 0.1)

# Calculate daily survival probabilities for the temperature range
daily_survival_values <- sapply(temperature_range, function(T) {
  calculate_survival_tjap_juv(T, a, b, c, juv_T0, juv_Tm, juv_coef_a, juv_coef_b)
})

# Create a data frame for plotting
plot_data <- data.frame(Temperature = temperature_range, Daily_Survival_Probability = daily_survival_values)

# Plot the results
ggplot(plot_data, aes(x = Temperature, y = Daily_Survival_Probability)) +
  geom_line(color = "blue") +
  labs(title = "Predicted Daily Survival Probability vs Temperature",
       x = "Temperature (°C)",
       y = "Daily Survival Probability") +
  theme_minimal()

ggsave("graphs/juv_tjap_daily_survival.png", width = 8, height = 6, dpi = 300)

##graph predicting emergence rate 

# Load necessary library
library(ggplot2)

# Define the function to predict the emergence rate
predict_emergence_rate <- function(T, a, b, c) {
  emergence_rate <- a + b * T + c * T^2
  return(emergence_rate)
}

# Coefficients from the polynomial model
a <- -0.013861228
b <- 0.077445952
c <- -0.001496376

# Define temperature range
temperature_range <- seq(10, 35, by = 0.1)

# Calculate predicted emergence rates for the temperature range
predicted_emergence_rates <- sapply(temperature_range, function(T) {
  predict_emergence_rate(T, a, b, c)
})

# Create a data frame for plotting
plot_data <- data.frame(Temperature = temperature_range, Predicted_Emergence_Rate = predicted_emergence_rates)

# Plot the results
ggplot() +
  geom_line(data = plot_data, aes(x = Temperature, y = Predicted_Emergence_Rate), color = "red", linetype = "dashed", size = 1, alpha = 0.7) +
  geom_point(data = temperature_data, aes(x = Temperature, y = Emerged_Adults_Proportion), size = 3) +
  labs(x = "Temperature (°C)",
       y = "Predicted Emergence Rate") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

ggsave("graphs/juv_tjap_predicted_emergence.png", width = 8, height = 6, dpi = 300)