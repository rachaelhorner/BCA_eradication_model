# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)  # for percentage formatting in ggplot

source("calculate_daily_juv_HH_survival.R")
source("fit_briere2_model.R")
source("fit_briere1_model.R")

# Parameters for the Briere2 model and survival function
params_briere <- list(
  egg_T0 = 10.020,
  egg_Tm = 33.564,
  egg_a = 3.52e-4,
  egg_m = 8.473,
  nymph_T0 = 13.109,
  nymph_Tm = 33.5,
  nymph_a = 4.7e-4,
  nymph_m = 4.180
)

# Parameters for the survival function
params_survival <- list(
  eggs = list(a = 12.509, b = -0.833, c = 0.017),
  nymphs = list(a = 10.904, b = -0.594, c = 0.012)
)

# Generate a sequence of temperatures from 10 to 34 degrees
temperatures <- seq(10, 34, by = 1)

# Calculate daily survival probabilities for eggs
daily_survival_eggs <- sapply(temperatures, function(T) {
  calculate_daily_juv_HH_survival(T, params_survival$eggs$a, params_survival$eggs$b, params_survival$eggs$c, params_briere$egg_T0, params_briere$egg_Tm, params_briere$egg_a, params_briere$egg_m)
})

# Calculate daily survival probabilities for nymphs
daily_survival_nymphs <- sapply(temperatures, function(T) {
  calculate_daily_juv_HH_survival(T, params_survival$nymphs$a, params_survival$nymphs$b, params_survival$nymphs$c, params_briere$nymph_T0, params_briere$nymph_Tm, params_briere$nymph_a, params_briere$nymph_m)
})

# Create the data frame for daily survival probabilities
daily_survival_df <- data.frame(
  Temperature = rep(temperatures, 2),
  Survival_Probability = c(daily_survival_eggs, daily_survival_nymphs),
  Stage = rep(c("Eggs", "Nymphs"), each = length(temperatures))
)

# Define stage colors
stage_colors <- c("Eggs" = "#33FFFF", "Nymphs" = "#009900", "Pre_Reproductive_Adults" = "blue", "Adults" = "brown",
                  "Diapausing_Adults" = "orange", "Parasitoid_juveniles" = "#CCCCCC", "Parasitoid_adults" = "black")

# Plot daily survival probability
ggplot(daily_survival_df, aes(x = Temperature, y = Survival_Probability, color = Stage)) +
  geom_line(size = 1) +
  labs(
    x = "Temperature (°C)",
    y = "Daily Survival Probability"
  ) +
  scale_color_manual(values = stage_colors) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# Save the plot
ggsave("graphs/daily_survival_probability_HH_juvs.png", width = 8, height = 6, dpi = 300)