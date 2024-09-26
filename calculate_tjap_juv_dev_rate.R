# Define the function to calculate development rate with bounds
calculate_tjap_juv_dev_rate <- function(current_temp, T0, Tm, coef_a, coef_b) {
  if (current_temp <= T0 || current_temp >= Tm) {
    return(0)  # Development does not occur outside of Tmin and Tmax
  } else {
    # Linear model for development rate within the viable temperature range
    return(coef_a * current_temp - coef_b)
  }
}
