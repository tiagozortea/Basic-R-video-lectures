# This script contains the functions developed in the lectures ( https://www.youtube.com/playlist?list=PLFf3DKi9pkFQceRv27Wm_EtNx6QOiCpOY )
# Author: Tiago Zortea
# 10/23/2012

getWeather=function(stationID="083326",start="2011-01-01",hashkey="4662f37d6874be9529a5fd5ae652322ae258c38bea7eb9a0ac8bb24e84559"){
  urlList <-URLencode(paste("http://nc-climate.ncsu.edu/dynamic_scripts/cronos/getCRONOSdata.php?station=",stationID,"&start=",start,"&obtype=D&parameter=tempmin,tempmax,precip&hash=",hashkey,sep=""))
	url=url(urlList)
	open(url)
	table=read.csv(url, header=T, sep="|",skip=33)
	close(url)
	return(table)
}

fillNAsTemperature = function(temp){ #temp= temperature vector
	naPos=which(is.na(temp))
	for(i in naPos){
		if(i==1){
			previous=NA
		}else{ 
			previous=temp[i-1]
		}
		if(i==length(temp)){
			following=NA
		}else{ 
			following=temp[i+1]
		}		
		if(!is.na(following)&&!is.na(previous)){
			temp[i]=(previous+following)/2
		}
		if(is.na(following)){
			temp[i]=previous
		}
		if(is.na(previous)){
			temp[i]=following
		}
	}
	print(paste(length(naPos),'values temperature estimated.'))
	return(temp)
}

#w=sanilizeWeatherData(w)
sanilizeWeatherData = function(wData){
	wData$DataAvailability=NULL
	wData$ob=strptime(wData$ob,format=("%Y-%m-%d"))
	first=which(!is.na(wData$tempmin) & !is.na(wData$tempmax) & !is.na(wData$precip))[1]
	wData=wData[first:dim(wData)[1],]
	naPos=which(is.na(wData$precip))
	wData[naPos,'precip']=0
	print(paste(length(naPos),'day(s) in which precipitation assumed to be 0.'))
	wData[,'tempmin']=fillNAsTemperature(wData[,'tempmin'])
	wData[,'tempmax']=fillNAsTemperature(wData[,'tempmax'])
	print(summary(wData))
	return(wData)
} 

#Yearly accumulated rainfall.
#yaccp=yearlyAccmulatedRainfall(w)
yearlyAccmulatedRainfall=function(w){
	samplesTreshold=340 # years with less than 340 measurements will not be considered or the sum wouldn't make sense
	w$year=as.numeric(format(w$ob,"%Y"))
	samples=aggregate(w$precip, by=list(w$year), FUN=length)
	validYears=which(samples[,2]>340) # only the positions of valid years
	result=aggregate(w$precip, by=list(w$year), FUN=sum)[validYears,]
	names(result)=c('year','accPrecip')
	return(result)
}

#Yearly mean of temperature across the years.
#ymeantemp=yearlyMeanTemp(w)
yearlyMeanTemp=function(w){ 
	w$tempmean=(w$tempmax+w$tempmin)/2
	samplesTreshold=340 # years with less than 340 measurements will not be considered or the sum wouldn't make sense
	w$year=as.numeric(format(w$ob,"%Y"))
	samples=aggregate(w$tempmean, by=list(w$year), FUN=length)
	validYears=which(samples[,2]>340) # only the positions of valid years
	result=aggregate(w$tempmean, by=list(w$year), FUN=mean)[validYears,]
	names(result)=c('year','meantemp')
	return(result)
}


#Yearly mean of maximum temperature across the years.
#ymaxtemp=yearlyMaxTemp(w)
yearlyMaxTemp=function(w){ 
	samplesTreshold=340 # years with less than 340 measurements will not be considered or the sum wouldn't make sense
	w$year=as.numeric(format(w$ob,"%Y"))
	samples=aggregate(w$tempmax, by=list(w$year), FUN=length)
	validYears=which(samples[,2]>340) # only the positions of valid years
	result=aggregate(w$tempmax, by=list(w$year), FUN=mean)[validYears,]
	names(result)=c('year','max')
	return(result)
}

#Yearly mean of minimum temperature across the years.
#ymintemp=yearlyMinTemp(w)
yearlyMinTemp=function(w){ 
	samplesTreshold=340 # years with less than 340 measurements will not be considered or the sum wouldn't make sense
	w$year=as.numeric(format(w$ob,"%Y"))
	samples=aggregate(w$tempmin, by=list(w$year), FUN=length)
	validYears=which(samples[,2]>340) # only the positions of valid years
	result=aggregate(w$tempmin, by=list(w$year), FUN=mean)[validYears,]
	names(result)=c('year','min')
	return(result)
}

#Number of days over 35C (95F) across all years.
#yhotdays=yearlyHotDaysNumber(w)
yearlyHotDaysNumber=function(w){ 
	w$hot=ifelse(w$tempmax>35,1,0)
	samplesTreshold=340 # years with less than 340 measurements will not be considered or the sum wouldn't make sense
	w$year=as.numeric(format(w$ob,"%Y"))
	samples=aggregate(w[,1], by=list(w$year), FUN=length)
	validYears=which(samples[,2]>340) # only the positions of valid years
	result=aggregate(w$hot, by=list(w$year), FUN=sum)[validYears,]
	names(result)=c('year','hot')
	return(result)
}

#Date of the first day with temperature <00C (320F) after begin of fall.
#ff=firstFrost(w)
firstFrost=function(w){ 
	fallMonths=c(8,9,10,11) #0 to 11
	w$month=w$ob$mon
	w$frost=ifelse(w$tempmin<0,TRUE,FALSE)
	w$firstFrost=ifelse(w$frost & (w$month%in%fallMonths) ,w$ob$yday,366) # it should search in january and february also, but would greatly complicate the algoritm
	samplesTreshold=340 # years with less than 340 measurements will not be considered or the sum wouldn't make sense
	w$year=as.numeric(format(w$ob,"%Y"))
	samples=aggregate(w[,1], by=list(w$year), FUN=length)
	validYears=which(samples[,2]>340) # only the positions of valid years
	w=na.omit(w)
	result=aggregate(w$firstFrost, by=list(w$year), FUN=min)[validYears,]
	names(result)=c('year','firstFrost')
	return(result)
}

#Date of the last day with temperature <00C (320F) until begin of summer.
#lf=lastFrost(w)
lastFrost=function(w){ 
	springMonths=c(1,2,3,4,5) #0 to 11
	w$month=w$ob$mon
	w$frost=ifelse(w$tempmin<0,TRUE,FALSE)
	w$lastFrost=ifelse(w$frost & (w$month%in%springMonths) ,w$ob$yday,0) 
	samplesTreshold=340 # years with less than 340 measurements will not be considered or the sum wouldn't make sense
	w$year=as.numeric(format(w$ob,"%Y"))
	samples=aggregate(w[,1], by=list(w$year), FUN=length)
	validYears=which(samples[,2]>340) # only the positions of valid years
	w=na.omit(w)
	result=aggregate(w$lastFrost, by=list(w$year), FUN=max)[validYears,]
	names(result)=c('year','lastFrost')
	return(result)
}

#Date of the last day with temperature <00C (320F) until begin of summer.
#lf=lastFrost(w)
lastFrost=function(w){ 
	springMonths=c(0,1,2,3,4) #0 to 11
	w$month=w$ob$mon
	w$frost=ifelse(w$tempmin<0,TRUE,FALSE)
	w$lastFrost=ifelse(w$frost & (w$month%in%springMonths) ,w$ob$yday,0) 
	samplesTreshold=340 # years with less than 340 measurements will not be considered or the sum wouldn't make sense
	w$year=as.numeric(format(w$ob,"%Y"))
	samples=aggregate(w[,1], by=list(w$year), FUN=length)
	validYears=which(samples[,2]>340) # only the positions of valid years
	w=na.omit(w)
	result=aggregate(w$lastFrost, by=list(w$year), FUN=max)[validYears,]
	names(result)=c('year','lastFrost')
	return(result)
}

#Monthly accumulated rainfall summed across the years.
#maccp=monthlyAccmulatedRainfall(w)
monthlyAccmulatedRainfall=function(w){ # not really climate change
	w$month=as.numeric(format(w$ob,"%m"))
	w$year=as.numeric(format(w$ob,"%Y"))
	result=aggregate(w$precip, by=list(w$month), FUN=sum)
	result$x=result$x/(length(unique(w$year)))
	names(result)=c('month','accPrecip')
	return(result)
}

#First occurrence of more than 25mm (1 Inch) of rainfall in less than 3 days after start of spring (begin of the rain season).
#fsr=firstSummerRain(w)
firstSummerRain=function(w){
	summerMonths=c(4,5,6,7)
	threeDayRainfall=w$precip
	w$month=w$ob$mon
	for(i in c(1:length(threeDayRainfall))){
		if(i==1) {
			threeDayRainfall[i]=w$precip[i]
		} else if (i==length(threeDayRainfall)){
			threeDayRainfall[i]=w$precip[i]
		}
		else {
			threeDayRainfall[i]=sum(w$precip[i+1],w$precip[i],w$precip[i-1])
		}
	}
	w$threeDayRainfall=threeDayRainfall
	w$intenseRain=ifelse(w$threeDayRainfall>1,TRUE,FALSE)
	w$firstSummerRain=ifelse(w$intenseRain & (w$month%in%summerMonths) ,w$ob$yday,365) 
	samplesTreshold=340 # years with less than 340 measurements will not be considered or the sum wouldn't make sense
	w$year=as.numeric(format(w$ob,"%Y"))
	samples=aggregate(w[,1], by=list(w$year), FUN=length)
	validYears=which(samples[,2]>340) # only the positions of valid years
	result=aggregate(w$firstSummerRain, by=list(w$year), FUN=min)[validYears,]
	names(result)=c('year','firstSummerRain')
	return(result)
}




