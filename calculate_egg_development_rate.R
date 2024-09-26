# Add this validation function to check for NA values
validate_params <- function(temp, T0, Tm, a, m) {
  if (any(is.na(c(temp, T0, Tm, a, m)))) {
    stop("One or more parameters are NA")
  }
}


calculate_egg_development_rate <- function(current_temp, params_briere) {
  #print(paste("Params - T0:", params_briere$egg_T0, 
           #   "Tm:", params_briere$egg_Tm, 
           #   "a:", params_briere$egg_a, 
           #   "m:", params_briere$egg_m))
  
  rate <- fit_briere2_model(current_temp, 
                            params_briere$egg_T0, 
                            params_briere$egg_Tm, 
                            params_briere$egg_a, 
                            params_briere$egg_m)
  #print(paste("Egg Dev Rate:", rate))
  return(rate)
}


