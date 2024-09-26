source("fit_briere2_model.R") # development rates for HH eggs and nymphs
source("fit_briere1_model.R")

calculate_all_HH_development_rates <- function(current_temp, params_briere) {
  list(
    Egg = fit_briere2_model(current_temp,
                        params_briere$egg_T0,
                        params_briere$egg_Tm,
                        params_briere$egg_a,
                        params_briere$egg_m),
    Nymph = fit_briere2_model(current_temp,
                          params_briere$nymph_T0,
                          params_briere$nymph_Tm,
                          params_briere$nymph_a,
                          params_briere$nymph_m),
    PreReproductive = fit_briere1_model(current_temp,
                                    params_briere$preR_T0,
                                    params_briere$preR_Tm,
                                    params_briere$preR_C)
  )
}

calculate_egg_development_rate <- function(current_temp, params_briere) {
#  print(paste("Params - T0:", params_briere$egg_T0, 
 #             "Tm:", params_briere$egg_Tm, 
 #             "a:", params_briere$egg_a, 
  #            "m:", params_briere$egg_m))

  rate <- fit_briere2_model(current_temp, 
                            params_briere$egg_T0, 
                            params_briere$egg_Tm, 
                            params_briere$egg_a, 
                            params_briere$egg_m)
 # print(paste("Egg Dev Rate:", rate))
  return(rate)
}

calculate_nymph_development_rate <- function(current_temp, params_briere) {
  #print(paste("Params - T0:", params_briere$nymph_T0, 
   #           "Tm:", params_briere$nymph_Tm, 
  #            "a:", params_briere$nymph_a, 
   #           "m:", params_briere$nymph_m))
  
  rate <- fit_briere2_model(current_temp, 
                            params_briere$nymph_T0, 
                            params_briere$nymph_Tm, 
                            params_briere$nymph_a, 
                            params_briere$nymph_m)
 # print(paste("Nymph Dev Rate:", rate))
  return(rate)
}

calculate_preReproductive_development_rate <- function(current_temp, params_briere) {
#  print(paste("Params - T0:", params_briere$preR_T0, 
 #             "Tm:", params_briere$preR_Tm, 
 #             "C:", params_briere$preR_C))
  
  rate <- fit_briere1_model(current_temp,
                            params_briere$preR_T0,
                            params_briere$preR_Tm,
                            params_briere$preR_C)
 # print(paste("PreReproductive Dev Rate:", rate))
  return(rate)
}