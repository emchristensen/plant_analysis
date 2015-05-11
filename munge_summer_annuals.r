source('portal_weather/csv_to_dataframe.r')



# notes:
# -no data: 2013, 2012, 2010, 2003
# -some data, but seem incomplete: 2009
# -only select stakes (13,15,31,37,51,57,73,75): 1982, 1981, 1980, 1979, 1978
# -no plot 24 pre-1980
# -1985 has no plot 1 or 2
# -1978 quadrats were 1m x .5m, all other years .5m x .5m
# -1989 census was conducted 9/2-9/15
# -1990 census was conducted 8/12-8/21
# -1991 census was conducted 8/16-8/27
# -1992 census was conducted 8/29-9/23
# -1993 census was conducted 9/5-9/15
# -1994 census was conducted 8/21-8/26
# - pre-1988 had some weird seed addition/annual removal going on in plots 1,2,6,9,13,18,20,22
# -"full" census seems to have been done in 2011, 2008, 2007, 2006, 2005, 2004, 2002, 
#       2001, 2000, 1999, 1998, 1997, 1996, 1995, 1994, 1993, 1992, 1991, 1990, 1989, 
#       1988, 1987, 1986, 1984, 1983
# -only using un-messed with control plots: 4,8,11,12,14,17



# =======================================================================================
# functions

summer_empty_quads = function(plantframe) {
  # this function finds missing quadrat/plot combinations in plant data and sets abundance to zero
  quadabund = aggregate(plantframe$Abundance,
                        by=list(year = plantframe$Year,
                                plot = plantframe$Plot,
                                stake = plantframe$Stake),
                        FUN = sum)
  # deal with the early years, when only 8 of 16 quadrats were censused in each plot
  earlyyears = c(1979,1980,1981,1982)
  plots = seq(1,24)
  earlystakes = c(13,15,31,37,51,57,73,75)
  eyr = vector()
  epl = vector()
  est = vector()
  n = 1
  for (y in earlyyears) {
    for (p in plots) {
      for (s in earlystakes) {
        eyr[n] = y
        epl[n] = p
        est[n] = s
        n = n+1
      }
    }
  }
  earlyquads = data.frame(year = eyr,plot=epl,stake=est)
  earlyquads2 = merge(quadabund,earlyquads,by=c('year','plot','stake'),all.y=T)
  earlyquads2$x[is.na(earlyquads2$x)] = 0
  
  
  years = c(1983, 1984, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 
            1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005, 2006, 2007, 2008, 2011)
  plots = seq(1,24)
  stakes = c(11,13,15,17,31,33,35,37,51,53,55,57,71,73,75,77)
  
  yr = vector()
  pl = vector()
  st = vector()
  n = 1
  for (y in years) {
    for (p in plots) {
      for (s in stakes) {
        yr[n] = y
        pl[n] = p
        st[n] = s
        n = n+1
      }
    }
  }
  quads = data.frame(year = yr,plot = pl,stake=st)
  quads2 = merge(quadabund,quads,by=c('year','plot','stake'),all.y=T)
  quads2$x[is.na(quads2$x)] = 0
  
  
  quads3 = rbind(earlyquads2,quads2)
  return(quads3)
}



plantframe = csv_to_dataframe('data/SummerAnn1978_2013.csv')

quadabund = summer_empty_quads(plantframe)

scontrols = quadabund[quadabund$plot %in% c(4,8,11,12,14,17),]
summannabund = aggregate(scontrols$x,
                        by=list(year = scontrols$year),
                        FUN = mean)
