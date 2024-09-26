fit_briere2_model <- function(current_temp, T0, Tm, a, m) {
  # Check if the current temperature is within the viable developmental range (between T0 and Tm)
  # Development rate is set to 0 outside this range, indicating no development
  if (current_temp <= T0 || current_temp >= Tm) {
    return(0)
  } else {
    # Calculate the development rate according to the Briere2 model
    # Model incorporates a non-linear response to temperature, especially near the upper threshold Tm
    rate <- a * current_temp * (current_temp - T0) * (Tm - current_temp)^(1/m)
    # Log the calculated rate for debugging purposes
    #print(paste("Temp:", current_temp, "Rate:", rate))
    return(rate)
  }
}


