calculate_adult_tjap_survival <- function(temperature, model) {
  
  new_data <- data.frame(Temperature = temperature)
  
  predicted_longevity <- predict(model, newdata = new_data)
  

  
  # Cap the predicted longevity at 112 days (16 weeks)
  if (predicted_longevity > 112) {
    predicted_longevity <- 112
  }
  
  # Debug: Print intermediate values
  # print(paste("Temperature:", temperature))
  # print(paste("Predicted Longevity:", predicted_longevity))
  
 
  daily_survival <- 1 - (1 / predicted_longevity)

  
  return(daily_survival)
}



