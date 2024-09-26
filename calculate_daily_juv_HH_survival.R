# Adjusted function to calculate daily survival probability including development rates calculation
calculate_daily_juv_HH_survival <- function(T, a, b, c, T0, Tm, a_dev, m_dev) {
  
  # Set survival to zero if temperature exceeds 34 degrees or is below 12.5
  if (T > 34 || T < 12.5) {
    return(0)
  }
  
  # Get development rate using Briere2 model
  dev_rate <- fit_briere2_model(T, T0, Tm, a_dev, m_dev)
  
  # Calculate the number of days for development
  if (dev_rate == 0) {
    days <- 1e+6  # Use almost Inf to represent non-development
  } else {
    days <- 1 / dev_rate
    # print(paste("Days for development:", days))
  }
  
  # Calculate survival probability using the specified formula
  log_odds <- a + b * T + c * T^2
  # print(paste("Log odds:", log_odds))
  
  
  raw_prob <- exp(log_odds)
  # print(paste("Raw probability:", raw_prob))
  
  # Convert the raw probability to an actual probability
  surv_prob <- raw_prob / (1 + raw_prob)
  # print(paste("Converted survival probability:", surv_prob))
  
  # Adjust for development days
  surv_prob <- surv_prob^(1/days)
  # print(paste("Adjusted survival probability:", surv_prob))
  
  return(surv_prob)
}



