calculate_daily_fecundity_fit <- function(current_temp) {
  # Known data points correlating temperature with ovi-period and total fecundity
  temp_data <- data.frame(
    Temperature = c(18.8, 20, 23, 27, 30, 33),
    OviPeriod = c(58, 70.13, 59.25, 38.71, 26.25, 24.17),
    TotalFecundity = c(8.3, 199.8, 308.67, 278.92, 152.84, 121.00)
  )
  
  # Temperature thresholds defining the viable range for fecundity calculations
  lower_temp_threshold <- 17.8
  upper_temp_threshold <- 34
  
  # Return 0 if the temperature is outside the viable range
  if (current_temp < lower_temp_threshold || current_temp > upper_temp_threshold) {
    return(0)
  }
  
  # Exclude rows with NA to ensure the model is fitted on complete cases
  complete_data <- na.omit(temp_data)
  
  # Fit polynomial models for fecundity and ovi-period
  fecundity_fit <- lm(TotalFecundity ~ poly(Temperature, 2, raw = TRUE), data = complete_data)
  period_fit <- lm(OviPeriod ~ poly(Temperature, 2, raw = TRUE), data = complete_data)
  
  # Predict fecundity and ovi-period at the current temperature
  predicted_fecundity <- predict(fecundity_fit, newdata = data.frame(Temperature = current_temp))
  predicted_period <- predict(period_fit, newdata = data.frame(Temperature = current_temp))
  
  # Calculate daily fecundity as total fecundity divided by the ovi-period
  daily_fecundity <- predicted_fecundity / predicted_period
  
  return(daily_fecundity)
}

# Example use of the function
current_temp <- 15
dynamic_fecundity_per_day_fit <- calculate_daily_fecundity_fit(current_temp)
print(dynamic_fecundity_per_day_fit)

library(ggplot2)

# Define the range of temperatures for plotting (15°C to 35°C)
temperature_range <- seq(15, 35, by = 0.1)

# Apply the model to each temperature point
daily_fecundity_fitted <- sapply(temperature_range, calculate_daily_fecundity_fit)

# Create a dataframe for plotting
fitted_plot_data <- data.frame(
  Temperature = temperature_range,
  DailyFecundity = daily_fecundity_fitted
)

# Plot using ggplot2
fitted_fecundity_plot <- ggplot(fitted_plot_data, aes(x = Temperature, y = DailyFecundity)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Model of Daily Fecundity vs. Temperature",
       subtitle = "Based on polynomial regression model with temperature bounds",
       x = "Temperature (°C)", y = "Daily Fecundity (eggs per day)") +
  theme_minimal()

print(fitted_fecundity_plot)

