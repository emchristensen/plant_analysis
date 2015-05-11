# This script is meant to find local maxima in Landsat NDVI data to determine timing of spring and 
# fall greenups.
# EMC 5/11/15

landsat = read.csv('data/LandsatNDVI_monthly.csv')

# ===============================================================================================
# exploratory plots
boxplot(landsat$x~landsat$month)

m2000 = landsat[landsat$year == 2000,]
plot(m2000$month,m2000$x,type='l',xlab='month',ylab='NDVI',xlim=c(1,12),ylim=c(1000,5000))
for (n in unique(landsat$year)) {
  yr = landsat[landsat$year == n,]
  lines(yr$month,yr$x)
}

maxofyear = c()
for (n in unique(landsat$year)) {
  yr = landsat[landsat$year == n,]
  yr = yr[yr$month<6,]
  maxofyear = append(maxofyear,yr[yr$x==max(yr$x),2])
}
hist(maxofyear,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))


# ================================================================================================
# find local max of all green-ups for each year

green = c()
for (n in seq(2,length(landsat$x)-1)) {
  if (landsat$x[n] > landsat$x[n-1] && landsat$x[n] > landsat$x[n+1]) {
    green = rbind(green,landsat[n,])
  }
}
