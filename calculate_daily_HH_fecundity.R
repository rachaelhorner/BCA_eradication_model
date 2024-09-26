# Define the function to calculate daily fecundity based on temperature
calculate_daily_HH_fecundity <- function(temperature,
                                      beta_0 = -25.73397680,   # Coefficient for the constant term
                                      beta_1 = 2.16081809,     # Coefficient for the linear term (Temperature)
                                      beta_2 = -0.04081738,    # Coefficient for the quadratic term (Temperature^2)
                                      T_min = 17.5,            # Minimum viable temperature
                                      T_max = 36) {            # Maximum viable temperature
  # Check if temperature is outside the viable range
  if (temperature <= T_min || temperature >= T_max) {
    return(0)  # Return fecundity as zero outside the viable temperature range
  }
  
  # Calculate predicted fecundity using the quadratic model
  daily_fecundity <- beta_0 + beta_1 * temperature + beta_2 * temperature^2
  
  # Ensure fecundity does not go below zero
  daily_fecundity <- max(daily_fecundity, 0)
  
  return(daily_fecundity)
}


