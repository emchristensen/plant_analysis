source('portal_weather/csv_to_dataframe.r')

plantfile = 'data/WinterAnn1986_2014.csv'

plants = csv_to_dataframe(plantfile)

by_year = aggregate(plants$abundance,
                    by=list(year = plants$year, species=plants$species),
                    FUN = sum)

num_sp = aggregate(by_year$species,
                   by=list(year=by_year$year),
                   FUN=length)
plot(num_sp)
lines(num_sp)

plants14 = plants[plants$year==2014,]
sp_2014 = aggregate(plants14$abundance,
                    by=list(species=plants14$species),                    
                    FUN=sum)
p14 = sp_2014[order(-sp_2014$x),]
top_sp = p14[1:10,]


plants13 = plants[plants$year==2013,]
sp_2013 = aggregate(plants13$abundance,
                    by=list(species=plants13$species),                    
                    FUN=sum)
p13 = sp_2013[order(-sp_2013$x),]
top_sp13 = p13[1:10,]

plants12 = plants[plants$year==2012,]
sp_2012 = aggregate(plants12$abundance,
                    by=list(species=plants12$species),                    
                    FUN=sum)
p12 = sp_2012[order(-sp_2012$x),]
top_sp12 = p12[1:10,]

plants09 = plants[plants$year==2009,]
sp_2009 = aggregate(plants09$abundance,
                    by=list(species=plants09$species),                    
                    FUN=sum)
p09 = sp_2009[order(-sp_2009$x),]
top_sp09 = p09[1:10,]

plants08 = plants[plants$year==2008,]
sp_2008 = aggregate(plants08$abundance,
                    by=list(species=plants08$species),                    
                    FUN=sum)
p08 = sp_2008[order(-sp_2008$x),]
top_sp08 = p08[1:10,]

plants07 = plants[plants$year==2007,]
sp_2007 = aggregate(plants07$abundance,
                    by=list(species=plants07$species),                    
                    FUN=sum)
p07 = sp_2007[order(-sp_2007$x),]
top_sp07 = p07[1:10,]
