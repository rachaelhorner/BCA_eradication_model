calculate_nymph_development_rate <- function(current_temp, params_briere) {
  #print(paste("Params - T0:", params_briere$nymph_T0, 
   #           "Tm:", params_briere$nymph_Tm, 
   #           "a:", params_briere$nymph_a, 
   #           "m:", params_briere$nymph_m))
  
  rate <- fit_briere2_model(current_temp, 
                            params_briere$nymph_T0, 
                            params_briere$nymph_Tm, 
                            params_briere$nymph_a, 
                            params_briere$nymph_m)
  #print(paste("Nymph Dev Rate:", rate))
  return(rate)
}