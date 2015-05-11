source('plants/munge_winter_annuals.r')
source('plants/munge_summer_annuals.r')
source('rodent_analysis/get_rodent_data.r')
source('data/period_dates.r')

dat = get_rodent_data('controls',seq(1977,2013))
#limit to plots used in annual plant data
dat = dat[dat$plot %in% c(4,8,11,12,14,17),]
per_count = aggregate(dat$species,
                    by=list(period = dat$period),
                    FUN=length)
date_count = merge(per_count,period_dates,by='period')
# ==========================================================================
# individual plots and just DM
dm = dat[dat$species=='DS',]
dm$energy = 5.69*dm$wgt^(3/4)
dm_count = aggregate(dm$species,by=list(period=dm$period,plot=dm$plot),FUN=length)
dm_dates = merge(dm_count,period_dates,by='period')
dm_wmo = dm_dates[dm_dates$mo %in% c(4,5,6,7),]
dm_wint = aggregate(dm_wmo$x,by=list(year=dm_wmo$yr,plot=dm_wmo$plot),FUN=sum)
winannplots = aggregate(wcontrols$x,by=list(year=wcontrols$year,plot=wcontrols$plot),FUN=mean)
summannplots = aggregate(scontrols$x,by=list(year=scontrols$year,plot=scontrols$plot),FUN=mean)

par(mar=c(5,4,4,5)+.1)
plot(dm_wint$year,dm_wint$x,xlim=c(1980,2012))
par(new=T)
plot(winannplots$year[winannplots$year<2010],winannplots$x[winannplots$year<2010],col='red',ylab='',xlim=c(1980,2012))

dm = dm[!is.na(dm$energy),]
dm_energy = aggregate(dm$energy,by=list(period=dm$period,plot=dm$plot),FUN=sum)
energy_dates = merge(dm_energy,period_dates,by='period')
energy_wmo = energy_dates[energy_dates$mo %in% c(3,4,5,6),]
energy_wint = aggregate(energy_wmo$x,by=list(year=energy_wmo$yr,plot=energy_wmo$plot),FUN=mean)
energy_plants = merge(energy_wint,winannplots,by=c('year','plot'))
energy_plants_80s = energy_plants[energy_plants$year < 1996,]
plot(energy_plants_80s$x.y,energy_plants_80s$x.x)
energy_plants_00s = energy_plants[energy_plants$year >1995,]
plot(energy_plants_00s$x.y,energy_plants_00s$x.x)

dm_plants = merge(dm_wint,winannplots,by=c('year','plot'))
plot(dm_plants$x.y,dm_plants$x.x,xlab='plants',ylab='rodents')
# ==========================================================================
# 'winter' rodents
wint_count = date_count[date_count$mo %in% c(5,6),]
wint_rats = aggregate(wint_count$x,
                      by=list(year = wint_count$yr),
                      FUN=sum)

wint_rats_plants = merge(wint_rats,winannabund,by='year')
plot(wint_rats_plants$x.y,wint_rats_plants$x.x)

summ_count = date_count[date_count$mo %in% c(9,10),]
summ_rats = aggregate(summ_count$x,
                      by=list(year = summ_count$yr),
                      FUN=mean)
plot(summ_rats)
lines(summ_rats)

summ_rats_plants = merge(summ_rats,summannabund,by='year')
plot(summ_rats_plants$x.y,summ_rats_plants$x.x)
