

###TEST
# Parameters for the models (assuming you've determined Tmin and Tmax)
T0 <- 12.43 # Example lower bound for females
Tm <- 35 # Example upper bound for females


# Coefficients from the linear equations
coef_a <- 0.0065
coef_b <- 0.0808

test_temperatures <- seq(10, 36, by = 0.5)  # Create a sequence of temperatures from 20 to 30 by 0.5 degree increments

# Apply the juvenile development rate function to each temperature
test_results <- sapply(test_temperatures, function(temp) calculate_tjap_juv_dev_rate(temp, T0, Tm, coef_a, coef_b))

# Calculate the reciprocal of the development rate, handling division by zero
reciprocals <- ifelse(test_results == 0, NA, 1 / test_results)

# Combine temperatures, juvenile development rates, and their reciprocals into a dataframe
results_df <- data.frame(
  Temperature = test_temperatures, 
  Juvenile_Development_Rate = test_results,
  Reciprocal_of_Development_Rate = reciprocals
)

# Print the results
print(results_df)

# Plot
ggplot(results_df, aes(x = Temperature)) +
  geom_line(aes(y = Juvenile_Development_Rate)) + 
  labs(x = "Temperature (°C)", 
       y = "Development Rate (1/days)") + 
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14)
  ) 


# Display the plot
ggsave("graphs/tjap_DevelopmentRatesPlot.png", width = 10, height = 6, dpi = 300)




