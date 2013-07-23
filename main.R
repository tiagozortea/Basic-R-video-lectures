#This script creates the plots developed in the lecture ( http://www.youtube.com/watch?v=zkEUQxoIyjk ).
#Author: Tiago Zortea
#10/23/2012

setwd('C:/Users/tiago/Desktop/R video lectures/Lecture 22 - Project final code overview')
source("functions.R")
w=getWeather(start="1901-01-01",stationID="087205") 
#Station: 087205 - Plant City        Date of first observation: September 1, 1892
#Station type: COOP - what is this?        
#City, State: Plant City, FL      County: Hillsborough County
#Latitude: 28.02361      Longitude: -82.14222
write.csv(w,file='087205.csv',row.names=F)
##################################################################
w=read.csv(file='087205.csv',stringsAsFactors=F)
w=sanilizeWeatherData(w)

yaccp=yearlyAccmulatedRainfall(w)
plot(x=yaccp$year,y=yaccp$accPrecip,lwd=2,type='l')
abline(lm(accPrecip ~ year, data=yaccp),lty=2)

##################################################################

ymeantemp=yearlyMeanTemp(w)
plot(x=ymeantemp$year,y=ymeantemp$meantemp,lwd=2,type='l')
abline(lm(meantemp ~ year, data=ymeantemp),lty=2)

##################################################################

ymaxtemp=yearlyMaxTemp(w)
plot(x=ymaxtemp$year,y=ymaxtemp$max,lwd=2,type='l')
abline(lm(max ~ year, data=ymaxtemp),lty=2)

##################################################################

ymintemp=yearlyMinTemp(w)
plot(x=ymintemp$year,y=ymintemp$min,lwd=2,type='l')
abline(lm(min ~ year, data=ymintemp),lty=2)

##################################################################

yhotdays=yearlyHotDaysNumber(w)
plot(x=yhotdays$year,y=yhotdays$hot,lwd=2,type='l')
abline(lm(hot ~ year, data=yhotdays),lty=2)

##################################################################

ff=firstFrost(w)
plot(x=ff$year,y=ff$firstFrost,lwd=2,type='l')
abline(lm(firstFrost ~ year, data=ff),lty=2)

##################################################################

lf=lastFrost(w)
plot(x=lf$year,y=lf$lastFrost,lwd=2,type='l')
abline(lm(lastFrost ~ year, data=lf),lty=2)

##################################################################

maccp=monthlyAccmulatedRainfall(w)
plot(x=maccp$month,y=maccp$accPrecip,lwd=2,type='l')

##################################################################

fsr=firstSummerRain(w)
plot(x=fsr$year,y=fsr$firstSummerRain,lwd=2,type='l')
abline(lm(firstSummerRain ~ year, data=fsr),lty=2)
