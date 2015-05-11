source('portal_weather/weather_daily_summary.r')
source('portal_weather/csv_to_dataframe.r')
source('plants/munge_summer_annuals.r')
source('plants/munge_winter_annuals.r')
source('data/munge_ndvi.r')


# monthly mean
monthlyndvi = aggregate(ndvi$mean,
                        by=list(month = ndvi$month, year = ndvi$year),
                        FUN=mean)
monthlyndvi$date = as.Date(paste(monthlyndvi$year,monthlyndvi$month,1),format='%Y %m %d')

rawls = csv_to_dataframe('C:/Users/EC/Documents/Portal_EC/NDVI/LandsatNDVI1990_2006.csv')
landsat = rawls[rawls$PctCloud<10,]
landsat$date = as.Date(paste(landsat$Year,landsat$JulianDay),format='%Y %j')


# ================================================================================================
# summer
summannabund$date = as.Date(paste(summannabund$year,9,1),format='%Y %m %d')
summannabund$month = rep(9,length(summannabund$x))
ndvisummann = merge(summannabund,monthlyndvi,by=c('year','month'),all.y=T)


par(mar=c(5,4,4,5)+.1)
plot(ndvisummann$date.y,ndvisummann$x.y,type="l",col="red",xlab='',ylab='NDVI')
par(new=TRUE)
plot(ndvisummann$date.x,ndvisummann$x.x,,type="b",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("plant census abund",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("NDVI","plant census"))

# ==============================================================================================
# winter
winannabund$date = as.Date(paste(winannabund$year,4,1),format='%Y %m %d')
winannabund$month = rep(4,length(winannabund$x))
ndviwinann = merge(winannabund,monthlyndvi,by=c('year','month'),all.y=T)

par(mar=c(5,4,4,5)+.1)
plot(ndviwinann$date.y,ndviwinann$x.y,type="l",col="red",xlab='',ylab='NDVI')
par(new=TRUE)
plot(ndviwinann$date.x,ndviwinann$x.x,,type="b",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("plant census abund",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("NDVI","plant census"))

# =================================================================================================
# both
annuals = rbind(summannabund,winannabund)
ndviann = merge(annuals,monthlyndvi,by=c('year','month'),all.y=T)

par(mar=c(5,4,4,5)+.1)
plot(ndviann$date.y,ndviann$x.y,type="l",col="red",xlab='',ylab='NDVI')
par(new=TRUE)
plot(ndviann$date.y,ndviann$x.x,,type="b",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("plant census abund",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("NDVI","plant census"))

# =============================================================================================
# regression
model1 = lm(ndviann$x.x~ndviann$x.y)
summary(model1)
plot(ndviann$x.y,ndviann$x.x,ylab='census data',xlab='ndvi data')
abline(model1)
