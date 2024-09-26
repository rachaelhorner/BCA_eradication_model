timestep_function_allee <- function(n.now, all_params, current_temp, photoperiod, 
                                    a_value = 0.46, k_value = 2, current_date, A_allee = 100, m_value = 0.1,
                                    novaluron_dates = NULL, novaluron_effect_days = 14) {
  current_date <- as.Date(current_date)
  
  params_briere <- all_params$briere
  params_juv_dev <- all_params$juv_dev
  params_egg_surv <- all_params$egg_surv
  params_nymph_surv <- all_params$nymph_surv
  params_juv_surv <- all_params$juv_surv
  common_params <- all_params$common_params
  
  n.next <- list(
    E1 = NA, E2 = NA, E3 = NA, E4plus = NA, 
    N = NA, P = NA, A = NA, 
    J = NA, Z = NA, D = NA,
    current_temp = current_temp, photoperiod = photoperiod
  )
  
  ###### Calculate and store development rates #####
  egg_dev_rate <- check_dev_rate_in_bounds(
    calculate_egg_development_rate(current_temp, params_briere), "Egg")
  
  nymph_dev_rate <- check_dev_rate_in_bounds(
    calculate_nymph_development_rate(current_temp, params_briere), "Nymph")
  
  preR_dev_rate <- check_dev_rate_in_bounds(
    calculate_preReproductive_development_rate(current_temp, params_briere), "PreReproductive")
  
  tjap_juv_dev_rate <- check_dev_rate_in_bounds(
    calculate_tjap_juv_dev_rate(current_temp, params_juv_dev$juv_T0,
                                params_juv_dev$juv_Tm, params_juv_dev$juv_coef_a,
                                params_juv_dev$juv_coef_b), "Juvenile")
  
  ##### Calculate host fecundity and store it #####
  dynamic_fecundity_per_day <- calculate_daily_HH_fecundity(current_temp)
  if (is.na(dynamic_fecundity_per_day)) dynamic_fecundity_per_day <- 0
  
  ### Calculate daily survival and store them ####
  dynamic_survival_eggs <- calculate_daily_juv_HH_survival(
    current_temp, params_egg_surv$a_surv, params_egg_surv$b_surv, params_egg_surv$c_surv,
    params_briere$egg_T0, params_briere$egg_Tm, params_briere$egg_a, params_briere$egg_m)
  
  # Calculate default daily survival for nymphs without novaluron
  dynamic_survival_nymphs <- calculate_daily_juv_HH_survival(
    current_temp, params_nymph_surv$a_surv, params_nymph_surv$b_surv, params_nymph_surv$c_surv,
    params_briere$nymph_T0, params_briere$nymph_Tm, params_briere$nymph_a, params_briere$nymph_m)
  
  # Check if novaluron is active (within the effect window after an application date)
  if (!is.null(novaluron_dates) && any(current_date >= novaluron_dates & current_date < as.Date(novaluron_dates) + novaluron_effect_days)) {
    # Novaluron is active, apply its effect
    daily_novaluron_survival <- 0.873  # 87.3% survival per day 
    dynamic_survival_nymphs <- dynamic_survival_nymphs * daily_novaluron_survival
    
  }
  
  
  adult_survival <- calculate_adult_HH_survival(current_temp)
  
  tjap_juv_survival <- calculate_survival_tjap_juv(
    current_temp, params_juv_surv$juv_a, params_juv_surv$juv_b, params_juv_surv$juv_c,
    params_juv_dev$juv_T0, params_juv_dev$juv_Tm, params_juv_dev$juv_coef_a, params_juv_dev$juv_coef_b)
  
  tjap_adult_survival <- calculate_adult_tjap_survival(current_temp, all_params$adult_tjap_survival_model)
  
  ### Temperature-driven attack rate ###
  if(current_temp >= 12.43 && current_temp <= 35) {
    common_params$a <- a_value
  } else {
    common_params$a <- 0
  }
  
  ###adjust the no. of parasitoids based on the sex ratio
  female_parasitoids <- n.now$Z * common_params$w
  
  # Calculate the total number of eggs available for parasitism
  total_eggs123 <- n.now$E1 + n.now$E2 + n.now$E3
  # cat("Total Eggs: ", total_eggs, "\n")
  
  
  # Calculate the proportion of each egg stage
  prop_E1 <- ifelse(total_eggs123 > 0, n.now$E1 / total_eggs123, 0)
  prop_E2 <- ifelse(total_eggs123 > 0, n.now$E2 / total_eggs123, 0)
  prop_E3 <- ifelse(total_eggs123 > 0, n.now$E3 / total_eggs123, 0)
  
  # Calculate the probability of parasitism for the total egg pool
  total_prob_parasitism <- ifelse(total_eggs123 > 0 & female_parasitoids > 0, 
                                  (1 - (1 + a_value * female_parasitoids^(1 - m_value) / (k_value * total_eggs123))^(-k_value)),0)
  
  
  
  
  # Apply parasitism across the total egg pool
  total_parasitized <- ifelse(total_prob_parasitism > 0 & total_eggs123 > 0, 
                              rbinom(1, total_eggs123, total_prob_parasitism),0)
  # cat("Total Parasitized: ", total_parasitized, "\n")
  
  
  proportion_parasitized <- ifelse(total_eggs123 > 0, total_parasitized / total_eggs123, NA)
  proportion_not_parasitized <- ifelse(total_eggs123 > 0, 1 - proportion_parasitized, NA)
  
  # cat("Proportion Parasitized: ", proportion_parasitized, "\n")
  # cat("Proportion Not Parasitized: ", proportion_not_parasitized, "\n")
  
  
  
  # Distribute the total parasitized eggs back to E1, E2, and E3 based on their proportions
  E1_parasitized <- ifelse(total_parasitized > 0 & prop_E1 > 0, round(total_parasitized * prop_E1), 0)
  E2_parasitized <- ifelse(total_parasitized > 0 & prop_E2 > 0, round(total_parasitized * prop_E2), 0)
  E3_parasitized <- ifelse(total_parasitized > 0 & prop_E3 > 0, round(total_parasitized * prop_E3), 0)
  
  # # State update
  # E1_parasitized <- ifelse(n.now$E1 > 0 & female_parasitoids > 0, rbinom(1, n.now$E1, (1-(1+common_params$a*female_parasitoids/common_params$k/n.now$E1)^(-common_params$k))),0)
  # E2_parasitized <- ifelse(n.now$E2 > 0 & female_parasitoids > 0, rbinom(1, n.now$E2, (1-(1+common_params$a*female_parasitoids/common_params$k/n.now$E2)^(-common_params$k))),0)
  # E3_parasitized <- ifelse(n.now$E3 > 0 & female_parasitoids > 0, rbinom(1, n.now$E3, (1-(1+common_params$a*female_parasitoids/common_params$k/n.now$E3)^(-common_params$k))),0)
  
  E1_not_parasitized <- n.now$E1 - E1_parasitized
  E2_not_parasitized <- n.now$E2 - E2_parasitized
  E3_not_parasitized <- n.now$E3 - E3_parasitized
  
  
  E1_survived <- rbinom(1, E1_not_parasitized, dynamic_survival_eggs)
  E2_survived <- rbinom(1, E2_not_parasitized, dynamic_survival_eggs)
  E3_survived <- rbinom(1, E3_not_parasitized, dynamic_survival_eggs)
  E4plus_survived <- rbinom(1, n.now$E4plus, dynamic_survival_eggs)
  
  N_survived <- rbinom(1, n.now$N, dynamic_survival_nymphs)
  P_survived <- rbinom(1, n.now$P, adult_survival)
  A_survived <- rbinom(1, n.now$A, adult_survival)
  J_survived <- rbinom(1, n.now$J, tjap_juv_survival)
  Z_survived <- rbinom(1, n.now$Z, tjap_adult_survival)
  D_survived <- rbinom(1, n.now$D, common_params$Q)
  
  Em <- rbinom(1, E4plus_survived, 1 / ((1 / egg_dev_rate) - 3))
  Nm <- rbinom(1, N_survived, nymph_dev_rate)
  Pm <- rbinom(1, P_survived, preR_dev_rate)
  Jm <- rbinom(1, J_survived, tjap_juv_dev_rate)
  
  new_eggs <- rpois(1, common_params$X * dynamic_fecundity_per_day * A_survived * (A_survived / (A_allee + A_survived)))
  
  n.next$E1 <- as.integer(new_eggs)
  n.next$E2 <- as.integer(E1_survived)
  n.next$E3 <- as.integer(E2_survived)
  n.next$E4plus <- E4plus_survived + E3_survived - Em
  n.next$N <- N_survived + Em - Nm 
  # n.next$P <- P_survived + Nm - Pm
  # n.next$A <- A_survived + Pm
  n.next$J <- J_survived + (E1_parasitized + E2_parasitized + E3_parasitized) - Jm
  n.next$Z <- Z_survived + Jm 
  
  
  # Entering diapause condition
  if (photoperiod < 12.7) { 
    # Maturing nymphs and pre-reproductive adults automatically into diapause
    n.next$D <-  D_survived + P_survived + Nm
    n.next$A <-  0
    n.next$P <-  0
  }
  else if (photoperiod < 13.5) {
    # In autumn pre-reproductive adults and adults don't go into diapause 
    # until the photoperiod is less than 12.7, so we need to calculate the population as normal
    n.next$P <- P_survived + Nm - Pm 
    n.next$A <- A_survived + Pm
    
    # In spring adults stay in diapause until the photoperiod exceeds 13.5
    n.next$D <-  D_survived
  }
  else
  {
    # This is the normal case in spring/summer when the photoperiod is above 13.5
    # All adults in diapause become pre-reproductive adults
    n.next$P <-  P_survived + Nm - Pm  + D_survived
    n.next$A <-  A_survived + Pm
    n.next$D <-  0
  }
  
  # Kill eggs and nymphs after March 20 i.e. photoperiod <12.08
  if (photoperiod < 12.08) {  
    n.next$E1 <- 0
    n.next$E2 <- 0
    n.next$E3 <- 0
    n.next$E4plus <- 0
    n.next$N <- 0
    n.next$J <- 0
  }
  
  
  # Return the results as a list
  return(c(n.next, list(date = current_date, 
                        E1_not_parasitized = E1_not_parasitized, 
                        E2_not_parasitized = E2_not_parasitized, 
                        E3_not_parasitized = E3_not_parasitized,
                        total_eggs123 = total_eggs123,
                        proportion_parasitized = proportion_parasitized,
                        proportion_not_parasitized = proportion_not_parasitized)))
}
