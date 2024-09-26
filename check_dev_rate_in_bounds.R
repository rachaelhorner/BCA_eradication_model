# Helper function to check the development rate
check_dev_rate_in_bounds <- function(dev_rate, life_stage) {
  if (dev_rate < 0 || dev_rate > 1) {
    stop(paste(life_stage, "development rate out of bounds:", dev_rate))
  }
  return(dev_rate)
}

