source('portal_weather/yearly_winter_precip.r')
source('portal_weather/yearly_summer_precip.r')
source('plants/munge_winter_annuals.r')
source('plants/munge_summer_annuals.r')


# ==================================================================================
# compare plants to season prior to census (4 mo)
summer_plant_ppt = merge(summer_ppt,summannabund,by='year')
winter_plant_ppt = merge(winter_ppt,winannabund,by='year')

plot(winter_plant_ppt$ppt,winter_plant_ppt$x,
     xlab='winter ppt (mm)',ylab='plant abundance (stems/quadrat)',
     main='Winter Annuals')
plot(summer_plant_ppt$ppt,summer_plant_ppt$x,
     xlab='summer ppt (mm)',ylab='plant abundance (stems/quadrat)',
     main='Summer Annuals')

plot(winannabund)
plot(summannabund)

plot(winter_ppt)
plot(summer_ppt)
lines(summer_ppt)

winlm = lm(winter_plant_ppt$x~winter_plant_ppt$ppt)
summary(winlm)

winlm2 = lm(winter_plant_ppt$x~log(winter_plant_ppt$ppt))
summary(winlm2)

sumlm = lm(summer_plant_ppt$x[7:25]~summer_plant_ppt$ppt[7:25])
summary(sumlm)

# outliers
# winter: 1984, 1989, 1987
plot(winter_plant_ppt$ppt[c(1,3,4,6,8:25)],winter_plant_ppt$x[c(1,3,4,6,8:25)])


# ======================================================================================
# just one month prior to census
monthlyppt = csv_to_dataframe('data/Monthly_ppt_1980_present_patched.csv')
july = monthlyppt[monthlyppt$month==7,]

summer_plant_july = merge(summannabund,july,by='year')
plot(summer_plant_july$sumprecip,summer_plant_july$x)

march = monthlyppt[monthlyppt$month==3,]
winter_plant_march = merge(winannabund,march,by='year')
plot(winter_plant_march$sumprecip,winter_plant_march$x)
