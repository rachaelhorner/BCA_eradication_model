run_model_once_allee <- function(temp_dataset, photoperiod_dataset, all_params, a_value = 0.15, k_value, m_value, 
                                 release_frequency, release_volume, release_dates, novaluron_dates = NULL, 
                                 novaluron_effect_days = 14, A_allee = 100, initial_E1 = 0, initial_E2 = 0, 
                                 initial_E3 = 0, initial_E4plus = 0, initial_N = 0, initial_P = 0, initial_A = 1000, 
                                 initial_J = 0, initial_Z = 0, initial_D = 0) {
  
  # Initialize the list to store the results of each time step
  results <- vector("list", length(temp_dataset)) 
  
  # Define the initial conditions for n.now
  n.now <- list(
    E1 = initial_E1, E2 = initial_E2, E3 = initial_E3, E4plus = initial_E4plus, 
    N = initial_N, P = initial_P, A = initial_A, 
    J = initial_J, Z = initial_Z, D = initial_D,
    current_temp = temp_dataset[1],
    photoperiod = photoperiod_dataset[1]
  )
  
  
  # Loop through each time step and store the results
  for (i in seq_len(length(temp_dataset))) {
    # Update the current temperature for the current time step
    current_temp <- temp_dataset[i]  
    current_photoperiod <- photoperiod_dataset[i]
    current_date <- as.Date(simulation_start_date + (i - 1))  # Adjust current_date to align with simulation start date
    
    
    n.now$current_temp <- current_temp
    n.now$photoperiod <- current_photoperiod
    
    if (current_date %in% release_dates) {
      n.now$Z <- n.now$Z + release_volume
    }
    
    # Check if novaluron is applied (within the effect window of novaluron_effect_days)
    if (any(current_date >= novaluron_dates & current_date < (novaluron_dates + novaluron_effect_days))) {
      novaluron_active <- TRUE
    } else {
      novaluron_active <- FALSE
    }
    #   print(paste("Parasitoids released on", current_date, ": Total Parasitoid Density =", n.now$Z))
    # } else {
    #   print(paste("No parasitoids released on", current_date))
    # }
    # 
    
    # Call the timestep function and update `n.now`
    n.now <- timestep_function_allee(n.now, all_params, current_temp, 
                                     current_photoperiod, a_value, 
                                     k_value, current_date, A_allee, m_value,
                                     novaluron_dates = novaluron_dates, novaluron_effect_days = novaluron_effect_days)
    
    results[[i]] <- n.now  # Store the result for this timestep
  }
  
  # Convert results list to a data frame
  # Convert results list to a data frame
  results_df <- do.call(rbind, lapply(seq_along(results), function(i) {
    timestep_result <- as.data.frame(t(results[[i]]))
    # Add the timestep index and current date to the dataframe
    timestep_result$timestep <- i
    timestep_result$date <- simulation_start_date + (i - 1)
    return(timestep_result)
  }))
  
  
  return(results_df)
}