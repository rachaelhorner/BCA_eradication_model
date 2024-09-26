
###briere1 function

fit_briere1_model <- function(current_temp, T0, Tm, C) {
  # Check if the current temperature is within the viable developmental range (between T0 and Tm)
  # Development rate is set to 0 outside this range, indicating no development
  if (current_temp <= T0 || current_temp >= Tm) {
    return(0)
  } else {
    # Calculate the development rate according to the Briere1 model
    rate <- C * current_temp * (current_temp - T0) * (Tm - current_temp)^(1/2)
    return(rate)
  }
}