calculate_cumulative_juv_HH_survival <- function(T, a, b, c, T0, Tm, a_dev, m_dev) {
  
  # Set survival to zero if temperature exceeds 34 degrees
  if (T > 34) {
    return(0)
  }
  
  # Get development rate using Briere2 model
  dev_rate <- fit_briere2_model(T, T0, Tm, a_dev, m_dev)
  
  # Calculate the number of days for development
  if (dev_rate == 0) {
    days <- 1e+6  # Use almost Inf to represent non-development
  } else {
    days <- 1 / dev_rate
  }
  
  # Calculate daily survival probability
  daily_surv_prob <- calculate_daily_juv_HH_survival(T, a, b, c, T0, Tm, a_dev, m_dev)
  
  # Calculate cumulative survival probability over the lifetime of the stage
  cumulative_surv_prob <- daily_surv_prob^days
  
  return(cumulative_surv_prob)
}



