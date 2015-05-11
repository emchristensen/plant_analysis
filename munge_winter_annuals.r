source('portal_weather/csv_to_dataframe.r')



# notes:
# -no data: 2011, 2010, 2006, 2000, 1980
# -some data, but seem incomplete: 2009, 1999, 1996
# -only select stakes (11,17,33,35,53,55,71,77): 1982, 1981, 1978
# -only select stakes (13,15,73,75): 1979
# -no plot 24 pre-1980
# -1978 quadrats were 1m x .5m, all other years .5m x .5m
# -1990 census was conducted 4/24-4/28
# -1991 census was conducted 4/10-4/20
# -1992 census was conducted 4/21-5/20
# -1993 census was conducted 4/24-5/11
# -1994 census was conducted 3/28-4/3
# -2013 census was conducted 3/12-3/17
# -2014 census was conducted 3/28-4/3
# - pre-1988 had some weird seed addition/annual removal going on in plots 1,2,6,9,13,18,20,22
# -"full" census seems to have been done in 2014, 2013, 2012, 2008, 2007, 2005, 2004, 2003,
#       2002, 2001, 1998, 1997, 1995, 1994, 1993, 1992, 1991, 1990, 1989, 1988, 1987, 1986, 
#       1985, 1984, 1983
# -only using un-messed with control plots: 4,8,11,12,14,17

setwd('C:/Users/EC/git_dir/')

# =======================================================================================
# functions

winter_empty_quads = function(plantframe) {
  # this function finds missing quadrat/plot combinations in plant data and sets abundance to zero
  quadabund = aggregate(plantframe$abundance,
                        by=list(year = plantframe$year,
                                plot = plantframe$plot,
                                stake = plantframe$stake),
                        FUN = sum)
  # deal with the early years, when only 8 of 16 quadrats were censused in each plot
  earlyyears = c(1981,1982)
  plots = seq(1,24)
  earlystakes = c(11,17,33,35,53,55,71,77)
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
  
  # deal with the rest of the years, when all 16 quadrats were censused
  years = c(1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 
            1997, 1998, 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2012, 2013, 2014)
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



plantframe = read.csv('data/WinterAnn1978_2014.csv')

quadabund = winter_empty_quads(plantframe)

wcontrols = quadabund[quadabund$plot %in% c(4,8,11,12,14,17),]
winannabund = aggregate(wcontrols$x,
                        by=list(year = wcontrols$year),
                        FUN = mean)

# ============================================================================
# exploratory plots
winannplots = aggregate(wcontrols$x,
                        by=list(year=wcontrols$year,plot=wcontrols$plot),
                        FUN=mean)

plot(winannplots$year,winannplots$x)
lines(winannabund,xlab='',ylab='',col='red',lwd=2)

quadplots = aggregate(quadabund$x,
                      by=list(year=quadabund$year,plot=quadabund$plot),
                      FUN=mean)
plot(quadplots$year,quadplots$x)
