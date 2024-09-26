# Define the data frame with experimental data
fecundity_temp_data <- data.frame(
  Temperature = c(17, 20, 23, 27, 30, 33),
  Sample_Size = c(4, 10, 12, 13, 19, 3),
  Female_Adult_Longevity = c(227.88, 181.5, 130.08, 85.96, 84.68, 90.67),
  Male_Adult_Longevity = c(211.4, 180.75, 119.12, 61.2, 83.62, 89.83),
  Pre_oviposition_Period = c(NA, 32.67, 14.67, 11.54, 10.84, 11.00),
  Oviposition_Period = c(NA, 70.13, 59.25, 38.71, 26.25, 24.17),
  Fecundity = c(NA, 199.8, 308.67, 278.92, 152.84, 121.00),
  DailyFecundity = c(NA, 1.100826, 2.372924, 3.244765, 1.804913, 1.334510)
)

# Remove rows with NA values in DailyFecundity
fecundity_temp_data <- na.omit(fecundity_temp_data)

# Fit a quadratic polynomial model
quad_model <- lm(DailyFecundity ~ poly(Temperature, 2, raw=TRUE), data = fecundity_temp_data)
summary(quad_model)

# Extract coefficients from the quadratic model
quad_coefs <- coef(quad_model)
quad_coefs

# Generate a sequence of temperatures from 15°C to 35°C
temperature_range <- seq(15, 35, by = 0.1)

# Calculate predictions for the quadratic model
quad_pred <- predict(quad_model, newdata = data.frame(Temperature = temperature_range))

# Ensure predictions are non-negative
quad_pred <- pmax(quad_pred, 0)

# Create a data frame for plotting
plot_data_quad <- data.frame(Temperature = temperature_range, DailyFecundity = quad_pred)

# Plot the raw data and the quadratic model predictions using ggplot2
ggplot() +
  geom_point(data = fecundity_temp_data, aes(x = Temperature, y = DailyFecundity), size = 3) +
  geom_line(data = plot_data_quad, aes(x = Temperature, y = DailyFecundity), color = "red", linetype = "dashed", size = 1, alpha = 0.7) +
  labs(x = "Temperature (°C)", y = "Daily Fecundity (eggs/female/day)")+
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),    
    axis.title.y = element_text(size = 14),  
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )

# Save the plot
ggsave("graphs/HH_fecundity_quadratic_model.png", width = 8, height = 6, dpi = 300)
