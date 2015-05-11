# This script summarizes landsat data (approximately one image every 16 days) to monthly
# EMC 5/11/15

# =================================================================================================
landsat = read.csv('data/LandsatNDVI_L5.csv')
lsat = landsat[landsat$PctCloud<10,]
lsat$date = as.Date(paste(lsat$Year,lsat$JulianDay),format='%Y %j')
lsat$month = as.integer(format(lsat$date,'%m'))
monthlylsat = aggregate(lsat$Mean,
                        by=list(month = lsat$month, year = lsat$Year),
                        FUN=mean)
monthlylsat$date = as.Date(paste(monthlylsat$year,monthlylsat$month,1),format='%Y %m %d')


write.csv(monthlylsat,file='data/LandsatNDVI_monthly.csv')
