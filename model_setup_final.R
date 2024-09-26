
# Function to check if a package is installed, and install it if not
install_if_missing <- function(packages) {
  # Get the list of packages currently installed
  installed_packages <- installed.packages()[, "Package"]
  
  # Loop through each package and check if it is installed
  for (pkg in packages) {
    if (!(pkg %in% installed_packages)) {
      install.packages(pkg, dependencies = TRUE)
    }
    # Load the package
    library(pkg, character.only = TRUE)
  }
}

# List of required packages
required_packages <- c("tidyr", "dplyr", "ggplot2", "readr", 
                       "lubridate", "tidyverse","geosphere", "scales", "viridis", 
                       "gridExtra", "tools", "stringr","suncalc", "progress")

# Install and load the required packages
install_if_missing(required_packages)

library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyverse)
library(geosphere)
library(scales)
library(viridis)
library(gridExtra)
library(tools)
library(stringr)
library(suncalc)
library(progress)

source("timestep_function_allee.R")
source("fit_briere2_model.R")
source("fit_briere1_model.R")
source("calculate_egg_development_rate.R")
source("calculate_nymph_development_rate.R")
source("calculate_preReproductive_development_rate.R")
source("calculate_tjap_juv_dev_rate.R") #split - calculates development rate for juvenile Tjap, function called "tjap_juv_lm"
source("calculate_daily_HH_fecundity.R") # split
source("check_dev_rate_in_bounds.R") # checks development rates are between 0 and 1
source("calculate_daily_juv_HH_survival.R") #split
source("calculate_adult_HH_survival.R") #split
source("calculate_survival_tjap_juv.R")
source("calculate_adult_tjap_survival.R")
source("calculate_adult_tjap_survival_graphs.R")
source("auckland_weather_data.R") #loads weather dataset
source("run_model_once_allee.R")
source("calculate_daily_fecundity_tjap.R")


# Define all parameters in a nested list structure
all_params <- list(
  briere = list(
    egg_T0 = 10.020,
    egg_Tm = 33.564,
    egg_a = 3.52e-4,
    egg_m = 8.473,
    nymph_T0 = 13.109,
    nymph_Tm = 33.5,
    nymph_a = 4.7e-4,
    nymph_m = 4.180,
    preR_C = 6.572e-05,  # C
    preR_T0 = 13.36,     # T0
    preR_Tm = 36.35      # Tm
  ),
  juv_dev = list(
    juv_T0 = 12.43,
    juv_Tm = 35,
    juv_coef_a = 0.0065,
    juv_coef_b = 0.0808
  ),
  egg_surv = list(
    life_stage = "Egg",
    a_surv = 12.509,
    b_surv = -0.833,
    c_surv = 0.017,
    T0 = 10.020,
    Tm = 33.564,
    a_dev = 3.52e-4,
    m_dev = 8.473
  ),
  nymph_surv = list(
    life_stage = "Nymph",
    a_surv = 10.904,
    b_surv = -0.594,
    c_surv = 0.012,
    T0 = 13.109,
    Tm = 33.5,
    a_dev = 4.7e-4,
    m_dev = 4.180
  ),
  # adult_longevity = list(
  #   adult_a = 133.462,
  #   adult_b = -122.452,
  #   adult_c = 50.613
  # ),
  
  
  juv_surv = list(
    juv_a = -0.013861228,
    juv_b = 0.077445952,
    juv_c = -0.001496376
  ),
  adult_tjap_surv = list(
    beta_0 = -371.78,
    beta_1 = 53.26,
    beta_2 = -2.238,
    beta_3 = 0.0295
  ),
  common_params = list(
    a = 0.15,
    k = 2,
    X = 0.5,
    Q = 0.9993, # daily mortality overwintering adults HH
    w = 0.85, # sex ratio of parasitoid adults
    A_allee = 100
  ),
  adult_tjap_survival_model = adult_tjap_survival_model
)


################################################################
#Setting up for Auckland#
###############################################################

# Ensure data consistency by explicitly converting to Date format
standardize_dates <- function(df, date_column) {
  df[[date_column]] <- as.Date(df[[date_column]])
  return(df)
}

# Standardize dates in the dataset
daily_mean_temps_two_years <- standardize_dates(daily_mean_temps_two_years, "plot_date")

daily_mean_temps_two_years

# Calculate the simulation start date: the first date where photoperiod > 13.5 hours + 14 days
threshold_date <- daily_mean_temps_two_years$plot_date[
  which(daily_mean_temps_two_years$photoperiod > 13.5)[1]
]

threshold_date

simulation_start_date <- threshold_date + 14

simulation_start_date

# # Subset the datasets to start from the simulation_start_date
start_index <- which(daily_mean_temps_two_years$plot_date == simulation_start_date)
temp_dataset <- daily_mean_temps_two_years$mean_temp[start_index:length(daily_mean_temps_two_years$mean_temp)]
photoperiod_dataset <- daily_mean_temps_two_years$photoperiod[start_index:length(daily_mean_temps_two_years$photoperiod)]



generate_release_dates <- function(frequency, start_date, end_date) {
  seq(from = as.Date(start_date), to = as.Date(end_date), by = paste(frequency, "days"))
}

#First year release period
release_start_date <- simulation_start_date + 28

release_end_date <- daily_mean_temps_two_years$plot_date[
  which(daily_mean_temps_two_years$photoperiod < 12.7)[1]
] + 14

# Generate release dates for the first period
release_dates_period_1 <- list(
  '7' = generate_release_dates(7, release_start_date, release_end_date),
  '14' = generate_release_dates(14, release_start_date, release_end_date),
  '28' = generate_release_dates(28, release_start_date, release_end_date)
)


release_start_date2 <- threshold_date + 365

release_end_date_2 <- release_end_date + 365

# Generate release dates for the second period
release_dates_period_2 <- list(
  '7' = generate_release_dates(7, release_start_date2, release_end_date_2),
  '14' = generate_release_dates(14, release_start_date2, release_end_date_2),
  '28' = generate_release_dates(28, release_start_date2, release_end_date_2)
)

# Combine release dates from both periods into one list
release_dates_combined <- list(
  '7' = c(release_dates_period_1[['7']], release_dates_period_2[['7']]),
  '14' = c(release_dates_period_1[['14']], release_dates_period_2[['14']]),
  '28' = c(release_dates_period_1[['28']], release_dates_period_2[['28']])
)


# Create a list of release dates for each frequency
release_dates <- list(
  '7' = generate_release_dates(7, release_start_date, release_end_date),
  '14' = generate_release_dates(14, release_start_date, release_end_date),
  '28' = generate_release_dates(28, release_start_date, release_end_date)
)

print(release_dates)

release_dates <- lapply(release_dates, as.Date)
release_dates_combined <- lapply(release_dates_combined, as.Date)

release_dates
str(release_dates)
str(temp_dataset)
str(photoperiod_dataset)
###################################################################
#NOVALURON APPLICATION DATES
#################################################################
novaluron_effect_days <- 14


# First year novaluron application period
novaluron_start_date <- simulation_start_date + 35  # Adjust based on when you want the first application
novaluron_end_date <- daily_mean_temps_two_years$plot_date[
  which(daily_mean_temps_two_years$photoperiod < 12.7)[1]
] + 28  # Set end date based on the photoperiod condition or another criterion

# Generate novaluron application dates for the first year (14-day interval)
novaluron_dates_period_1 <- generate_release_dates(14, novaluron_start_date, novaluron_end_date)

# Second year novaluron application period
novaluron_start_date2 <- novaluron_start_date + 365  # Novaluron applications for the second year start exactly 1 year later
novaluron_end_date2 <- novaluron_end_date + 365  # Corresponding end date for the second year

# Generate novaluron application dates for the second year (14-day interval)
novaluron_dates_period_2 <- generate_release_dates(14, novaluron_start_date2, novaluron_end_date2)

# Combine novaluron dates from both years
novaluron_dates_combined <- c(novaluron_dates_period_1, novaluron_dates_period_2)

# Convert novaluron dates to Date format
novaluron_dates_combined <- as.Date(novaluron_dates_combined)

# Print the combined novaluron application dates
print(novaluron_dates_combined)

novaluron_dates_combined <- lapply(novaluron_dates_combined, as.Date)
