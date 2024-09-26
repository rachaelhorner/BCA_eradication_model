# Define the function to calculate daily fecundity based on temperature
calculate_daily_fecundity_tjap <- function(temperature,
                                           beta_0 = 42.7196,   # Coefficient for the constant term
                                           beta_1 = -5.9323,   # Coefficient for the linear term (Temperature)
                                           beta_2 = 0.2662,    # Coefficient for the quadratic term (Temperature^2)
                                           beta_3 = -0.0035,   # Coefficient for the cubic term (Temperature^3)
                                           T_min = 12,         # Minimum viable temperature
                                           T_max = 36) {       # Maximum viable temperature
  # Check if temperature is outside the viable range
  if (temperature <= T_min || temperature >= T_max) {
    return(0)  # Return fecundity as zero outside the viable temperature range
  }
  
  # Calculate predicted fecundity using the cubic model
  daily_fecundity <- beta_0 + beta_1 * temperature + beta_2 * temperature^2 + beta_3 * temperature^3
  
  # Ensure fecundity does not go below zero
  daily_fecundity <- max(daily_fecundity, 0)
  
  return(daily_fecundity)
}

