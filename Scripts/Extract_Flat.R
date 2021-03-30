# This function takes a vector TimeSeries of length N=8760 (number of hours in a year)
# with the characteristic of being essentially flat in the first two months of the year,
# and returns the value of that "flat" level

Extract.Flat.Level <- function(TimeSeries){

  # Total number of hours screened (1440h=2months)
  L <- 1440
  # Number of hours making each sub-block in which max is taken
  subwindow <- 12 
  # Adjust L so that it is a multiple of subwindow
  L <- L - L%%subwindow
  
  Short <- TimeSeries[1:L]
  # Reshape so that each row contains a subwindow-hour period
  Short <- matrix(Short, ncol = subwindow, byrow = T)
  # Compute maximum in each row
  M <- apply(Short, 1, max)
  
  return(mean(M))
  
}








