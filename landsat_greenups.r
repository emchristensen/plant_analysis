# This script is meant to find local maxima in Landsat NDVI data to determine timing of spring and 
# fall greenups.
# EMC 5/11/15


# ================================================================================================
# find timing of all green-ups for each year

landsat = read.csv('data/LandsatNDVI_monthly.csv')

green = c()
for (n in seq(2,length(landsat$x)-1)) {
  if (landsat$x[n] > landsat$x[n-1] && landsat$x[n] > landsat$x[n+1]) {
    green = rbind(green,landsat[n,])
  }
}
