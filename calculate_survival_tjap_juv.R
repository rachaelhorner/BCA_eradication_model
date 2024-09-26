calculate_survival_tjap_juv <- function(T, a, b, c, juv_T0, juv_Tm, juv_coef_a, juv_coef_b) {
  # Calculate development rate
  dev_rate <- calculate_tjap_juv_dev_rate(T, juv_T0, juv_Tm, juv_coef_a, juv_coef_b)
  #print(paste("Temperature:", T, "Development Rate:", dev_rate))
  
  # Calculate days for development if development rate is positive
  d <- if (dev_rate > 0) 1 / dev_rate else 1e+6  
  #print(paste("Days for Development:", d))
  
  # Calculate tJs using the polynomial model coefficients
  tJs <- a + b * T + c * T^2
  #print(paste("tJs:", tJs))
  
  Js_T <- (tJs)^(1/d)
  #print(paste("Js_T:", Js_T))
  
  return(Js_T)
}

# Example usage of the function
# Coefficients from the polynomial model
a <- -0.013861228
b <- 0.077445952
c <- -0.001496376

# Juvenile development parameters
juv_T0 <- 12.43
juv_Tm <- 35
juv_coef_a <- 0.0065
juv_coef_b <- 0.0808

# Example temperature
temperature <- 30

# # Calculate the daily survival probability at the given temperature
# daily_survival <- calculate_survival_tjap_juv(temperature, a, b, c, juv_T0, juv_Tm, juv_coef_a, juv_coef_b)
# print(daily_survival)

