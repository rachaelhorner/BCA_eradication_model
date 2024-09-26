# Example Model Run Script

source("model_setup_final.R")

# Run the model with dummy parameters and datasets
results <- run_model_once_allee(temp_dataset, photoperiod_dataset, all_params, a_value = 0.15, k_value, m_value, 
                                 release_frequency, release_volume, release_dates, novaluron_dates = NULL, 
                                 novaluron_effect_days = 14, A_allee = 100, initial_E1 = 0, initial_E2 = 0, 
                                 initial_E3 = 0, initial_E4plus = 0, initial_N = 0, initial_P = 0, initial_A = 1000, 
                                 initial_J = 0, initial_Z = 0, initial_D = 0)

# Print first few rows of results
head(results)

view(results)
