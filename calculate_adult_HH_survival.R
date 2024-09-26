calculate_adult_HH_survival <- function(temperature) {
  # Extract coefficients from the linear model
  intercept <- 94.63993 
  slope <- -2.361192
  
  # Calculate predicted longevity using the linear model formula
  predicted_longevity <- intercept + slope * temperature
  
  # Debug: Print intermediate values
  # print(paste("Temperature:", temperature))
  # print(paste("Predicted Longevity:", predicted_longevity))
  
  # Calculate daily survival probability
  daily_survival_probability <- 1 - (1 / predicted_longevity)
  
  # Debug: Print the calculated survival probability
  # print(paste("Daily Survival Probability:", daily_survival_probability))
  
  return(daily_survival_probability)
}


