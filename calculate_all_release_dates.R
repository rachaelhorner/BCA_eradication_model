# Calculate release dates for multiple frequencies
# # Example usage
# release_frequencies <- c(7, 14, 28)
# all_release_dates <- calculate_all_release_dates("2000-01-01", "2001-12-31", release_frequencies)
# print(all_release_dates)
calculate_all_release_dates <- function(start_date, end_date, release_frequencies) {
  release_dates_list <- list()
  for (frequency in release_frequencies) {
    dates <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day")
    if (frequency > 0) {
      release_dates <- dates[dates >= as.Date("2000-12-09") & dates <= as.Date("2001-03-14")]
      release_dates <- release_dates[seq(1, length(release_dates), by = frequency)]
    } else {
      release_dates <- as.Date(character(0))  # No releases for the control
    }
    release_dates_list[[as.character(frequency)]] <- release_dates
  }
  return(release_dates_list)
}