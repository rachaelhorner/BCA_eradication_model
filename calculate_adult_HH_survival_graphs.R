
# data from govindan
# adult_longevity_data <- data.frame(
#   Temperature = c(17, 20, 23, 27, 30, 33),
#   Female_Longevity = c(227.88, 181.5, 130.08, 85.96, 84.68, 90.67),
#   Male_Longevity = c(211.4, 180.75, 119.12, 61.20, 83.62, 89.83),
#   Pre_oviposition_Period = c(NA, 32.67, 14.67, 11.54, 10.84, 11),
#   Oviposition_Period = c(NA, 70.13, 59.25, 38.71, 26.25, 24.17),
#   Fecundity = c(NA, 199.8, 308.67, 278.92, 152.84, 121)
# )
##data from Mermer et al. 
adult_longevity_data <- data.frame(
  Temperature = c(15, 18, 22, 25, 27, 30, 32, 17, 20, 22, 25, 27, 30, 33),
  Female_Longevity = c(55.5, 75.1, 43.8, 28.3, 22.8, 13.8, 15.7, 52.7, 28.4, 42.2, 47.97, 32.8, 31.1, 24.9)
)
##plot of raw data

ggplot(adult_longevity_data, aes(x = Temperature, y = Female_Longevity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(title = "Female Longevity vs. Temperature", x = "Temperature (°C)", y = "Longevity (days)") +
  theme_minimal()

# Fit a linear model
 
#adult_longevity_model <- lm(Female_Longevity ~ poly(Temperature, 2), data = adult_longevity_data)
adult_longevity_model <- lm(Female_Longevity ~ Temperature, data = adult_longevity_data)
summary(adult_longevity_model)

# Extract model coefficients
coefficients <- coef(adult_longevity_model)
intercept <- coefficients[1]
slope <- coefficients[2]

# Print the coefficients
cat("Intercept:", intercept, "\n")
cat("Slope:", slope, "\n")

####linear model fit graph###

# Predict using the linear model
temp_seq <- seq(10, 35, by = 0.1)
linear_pred <- predict(adult_longevity_model, newdata = data.frame(Temperature = temp_seq))

# Create a data frame for the predictions
linear_data <- data.frame(Temperature = temp_seq, Longevity = linear_pred)

# Plot the data and the linear model fit
plot <- ggplot() +
  geom_point(data = adult_longevity_data, aes(x = Temperature, y = Female_Longevity), color = "black") +
  geom_line(data = linear_data, aes(x = Temperature, y = Longevity), color = "red", linetype = "dashed", size = 1, alpha = 0.7) +
  labs(
    x = "Temperature (°C)",
    y = "Adult Longevity (days)",
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  xlim(10, 35)  # Set the x-axis limits

# Save the plot
ggsave("graphs/linear_model_fit_adult_survival.png", plot = plot, width = 8, height = 6, dpi = 300)

print(plot)


####GRAPH OF DAILY SURV PROBS####

# Generate temperature sequence for predictions
temp_seq <- seq(10, 35, by = 0.1)

# Calculate daily survival probabilities using the custom function
daily_survival_custom <- sapply(temp_seq, calculate_adult_HH_survival, model = adult_longevity_model)

# Create a data frame for the predictions
survival_data_custom <- data.frame(Temperature = temp_seq, Daily_Survival_Probability = daily_survival_custom)

# Plot the predicted daily survival probabilities
plot_custom <- ggplot(survival_data_custom, aes(x = Temperature, y = Daily_Survival_Probability)) +
  geom_line(color = "black",size = 1) +
  labs(
    x = "Temperature (°C)",
    y = "Daily Survival Probability",
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  xlim(10, 35)  # Set the x-axis limits

# Save the plot
ggsave("graphs/predicted_daily_survival_adult_HH.png", plot = plot_custom, width = 8, height = 6, dpi = 300)

print(plot_custom)