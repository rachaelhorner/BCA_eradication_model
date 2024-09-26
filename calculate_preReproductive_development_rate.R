calculate_preReproductive_development_rate <- function(current_temp, params_briere) {
  #print(paste("Params - T0:", params_briere$preR_T0, 
    #          "Tm:", params_briere$preR_Tm, 
    #          "C:", params_briere$preR_C))
  
  rate <- fit_briere1_model(current_temp,
                            params_briere$preR_T0,
                            params_briere$preR_Tm,
                            params_briere$preR_C)
  #print(paste("PreReproductive Dev Rate:", rate))
  return(rate)
}