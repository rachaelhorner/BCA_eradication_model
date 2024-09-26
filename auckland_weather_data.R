latitude <- -36.8841  # Auckland, New Zealand

# Read and preprocess the dataset
temp_dataset <- read_csv("./mtalbert.csv", col_types = cols(
  date = col_character(),
  mean_air_temp = col_character()
))

# Trim whitespace and handle potential non-visible characters
temp_dataset <- temp_dataset %>%
  mutate(mean_air_temp = str_trim(mean_air_temp))

# Convert date and mean_air_temp columns
temp_dataset <- temp_dataset %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"),
         mean_air_temp = as.numeric(mean_air_temp))

# Create a new date column without the year for plotting
temp_dataset <- temp_dataset %>%
  mutate(date_no_year = make_date(2000, month(date), day(date))) %>%
  arrange(date_no_year)

# Adjust the date for plotting purposes to handle the November 11th start date
temp_dataset <- temp_dataset %>%
  mutate(plot_date = if_else(date_no_year >= make_date(2000, 10, 20), 
                             date_no_year, 
                             date_no_year + years(1)))

# Calculate the mean temperature for each day across all years
daily_mean_temps <- temp_dataset %>%
  group_by(plot_date) %>%
  summarise(mean_temp = mean(mean_air_temp, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(plot_date)

# Function to calculate photoperiod
calculate_photoperiod <- function(date, latitude) {
  jd <- as.numeric(format(date, "%j"))  # Convert date to Julian day
  photoperiod <- daylength(latitude, jd)
  return(photoperiod)
}

# Add photoperiod to the dataset
temp_dataset <- temp_dataset %>%
  mutate(photoperiod = calculate_photoperiod(plot_date, latitude))

# Calculate the mean temperature and photoperiod for each day across all years
daily_mean_temps <- temp_dataset %>%
  group_by(plot_date) %>%
  summarise(mean_temp = mean(mean_air_temp, na.rm = TRUE),
            photoperiod = mean(photoperiod, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(plot_date)

# Ensure no NA values are present in mean_temp and photoperiod
daily_mean_temps <- daily_mean_temps %>%
  filter(!is.na(mean_temp) & !is.na(photoperiod))

#######################################################################################
# Extend the dataset to cover two years
#####################################################################################
daily_mean_temps_two_years <- daily_mean_temps %>%
  bind_rows(daily_mean_temps %>%
              mutate(plot_date = plot_date + years(1)))

# Ensure dates are properly formatted and no NA values exist
daily_mean_temps_two_years <- daily_mean_temps_two_years %>%
  mutate(plot_date = as.Date(plot_date))


##################GRAPH OF PHOTOPERIOD AND TEMP######################

# Find the dates when photoperiod drops below 12.7 hours (Enter diapause) and exceeds 13.5 hours (Exit diapause)
diapause_enter_dates <- daily_mean_temps_two_years %>%
  filter(photoperiod < 12.7) %>%
  slice(c(1, which.min(abs(plot_date - as.Date("2001-12-31"))))) %>%
  pull(plot_date)

diapause_exit_dates <- daily_mean_temps_two_years %>%
  filter(photoperiod > 13.5) %>%
  slice(which.min(abs(plot_date - as.Date("2001-07-01")))) %>%  # Only select the second "Exit diapause"
  pull(plot_date)

# Adjust the position of the labels and make dotted lines more visible
ggplot(daily_mean_temps_two_years, aes(x = plot_date)) +
  geom_line(aes(y = mean_temp, color = "Mean Temperature (°C)"), size = 1) +  # Temperature line
  geom_line(aes(y = photoperiod, color = "Photoperiod (hours)"), linetype = "dashed", size = 1) +  # Photoperiod line
  
  # Add vertical lines for entering diapause
  geom_vline(xintercept = as.numeric(diapause_enter_dates), linetype = "dotted", color = "black", size = 1.2) +
  annotate("text", x = diapause_enter_dates[1], y = 11, label = "Enter diapause", angle = 90, vjust = -0.5, color = "black", size = 5) +
  annotate("text", x = diapause_enter_dates[2], y = 11, label = "Enter diapause", angle = 90, vjust = -0.5, color = "black", size = 5) +
  
  # Add vertical line for the second "Exit diapause"
  geom_vline(xintercept = as.numeric(diapause_exit_dates), linetype = "dotted", color = "black", size = 1.2) +
  annotate("text", x = diapause_exit_dates, y = 11, label = "Exit diapause", angle = 90, vjust = -0.5, color = "black", size = 5) +
  
  scale_y_continuous(
    name = "Mean Temperature (°C)", 
    sec.axis = sec_axis(~., name = "Photoperiod (hours)")  # Secondary y-axis for photoperiod
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  labs(x = "Date") +
  scale_color_manual(values = c("Mean Temperature (°C)" = "blue", "Photoperiod (hours)" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(color = "blue", size = 16),
    axis.title.y.right = element_text(color = "red", size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"  # Remove the legend
  )

# Save the plot
ggsave("graphs/temperature_photoperiod_with_diapause_exits.png", width = 10, height = 6, dpi = 300)













