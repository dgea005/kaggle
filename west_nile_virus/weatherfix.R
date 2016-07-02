# need to arange data to be usable


data_dir = "C:/DataSets/west_nile"

weather = read.csv(unzip(file.path(data_dir, "weather.csv.zip")))


# following - script - drop codesum
# will investigate how to reintroduce - will potentially need dummy variables
# drop some rows with no data in them: "Depth", "Water1", "Snowfall"
weather = subset(weather, select = -c(Depth, Water1, SnowFall))

# CodeSum
ee<-levels(weather$CodeSum)
tr<-c()
for (i in 1:length(ee)){
  tr<-c(tr,unlist(strsplit(ee[i],' ')))}
tr<-unique(tr[-1])
tr[9]<-'FGP'
for (j in 1:length(tr)){
  for (r in 1:nrow(weather)){
    if(grepl(weather[r,'CodeSum'], tr[j])){
      eval(parse(text=paste('weather$',tr[j],'[',r,']=1',sep='')))}
    else{eval(parse(text=paste('weather$',tr[j],'[',r,']=0',sep='')))}}}




# Dates
weather$Date = levels(weather$Date)[as.numeric(weather$Date)]
weather$Date = as.Date(weather$Date)

weather$Year = year(weather$Date)
weather$Month = month(weather$Date)
weather$Day = day(weather$Date)


# WetBulb
# there are 4 missing values - will just plug in average for now
weather$WetBulb = as.character(weather$WetBulb)
weather$WetBulb = as.numeric(weather$WetBulb)
wetbulb_avg = mean(weather$WetBulb, na.rm = TRUE)
weather$WetBulb[is.na(weather$WetBulb)] = wetbulb_avg

#PrecipTotal - need to deal with T; TRACE - like a tiny amount, and M - MISSING
weather$PrecipTotal = as.character(weather$PrecipTotal)
weather$PrecipTotal[weather$PrecipTotal == "  T"] = "0.0001"
weather$PrecipTotal = as.numeric(weather$PrecipTotal)
# average or some other value may be better here e.g. average for week?
weather$PrecipTotal[is.na(weather$PrecipTotal)] = 0


#StnPressure seem to be 
weather$StnPressure = as.character(weather$StnPressure)
weather$StnPressure = as.numeric(weather$StnPressure)
stnavg = mean(x = weather$StnPressure, na.rm = TRUE)
weather$PrecipTotal[is.na(weather$PrecipTotal)] = stnavg


# SeaLevel seems to have data missing in both
weather$SeaLevel = as.character(weather$SeaLevel)
weather$SeaLevel = as.numeric(weather$SeaLevel)
sealevelavg = mean(x = weather$SeaLevel, na.rm = TRUE)
weather$SeaLevel[is.na(weather$SeaLevel)] = sealevelavg

# Heat
# will just set M to 0 for now
weather$Heat = as.character(weather$Heat)
weather$Heat = as.numeric(weather$Heat)
weather$Heat[is.na(weather$Heat)] = 0


# want to combine the two weather stations
station1 = weather[weather["Station"] == 1, ]
station2 = weather[weather["Station"] == 2, ]
# station variable is no longer meaningful - so drop it
station1 = subset(station1, select = -c(Station))
station2 = subset(station2, select = -c(Station))


# TAVG
# convert Tavg from factor to numeric
station1$Tavg = as.character(station1$Tavg)
station1$Tavg = as.numeric(station1$Tavg)

# issue for station2 - we have 11 M's - will just take the average of min and max
station2$Tavg = as.character(station2$Tavg)
station2$Tavg = as.numeric(station2$Tavg)
# this tranformation converts the m's to NA's - so just need to convert the NA's to new average
station2[is.na(station2$Tavg), "Tavg"]  =
  (station2[is.na(station2$Tavg), "Tmax"] + station2[is.na(station2$Tavg), "Tmin"]) / 2

# DEPART
# want to convert Depart to numeric
# can see there are no M's to worry about
station1$Depart = as.character(station1$Depart)
station1$Depart = as.numeric(station1$Depart)

#station2 depart
# will just set to 0 for now because all 1472 values are M
# will just have to assume no departure from normal - might be a bad idea
# could be better to just use station1 values if they are similar
station2$Depart = as.character(station2$Depart)
station2$Depart = as.numeric(station2$Depart)
station2$Depart = 0


station1$Cool = as.character(station1$Cool)
station1$Cool = as.numeric(station1$Cool)

station2$Cool = as.character(station2$Cool)
station2$Cool = as.numeric(station2$Cool)
station2$Cool[is.na(station2$Cool)] = 0

station1$Sunrise = as.character(station1$Sunrise)
station1$Sunrise = as.numeric(station1$Sunrise)

station2$Sunrise = station1$Sunrise

station1$Sunset = as.character(station1$Sunset)
station1$Sunset = as.numeric(station1$Sunset)

station2$Sunset = station1$Sunset

# try average speed of station2 
# none missing in station1
station1$AvgSpeed = as.character(station1$AvgSpeed)
station1$AvgSpeed = as.numeric(station1$AvgSpeed)

station2$AvgSpeed = as.character(station2$AvgSpeed)
station2$AvgSpeed = as.numeric(station2$AvgSpeed)
stat2_avgspeed_mean = mean(station2$AvgSpeed, na.rm = TRUE)
station2$AvgSpeed[is.na(station2$AvgSpeed)] = stat2_avgspeed_mean


# str(station1)
# str(station2)
# # factors have all been converted to numeric
# 
# # could try just using station1 for now - since it has less missing data
# plot(station1$Tavg)
# plot(station1$Date, station1$Tavg)


# want consequtive days of rain
head(station1$PrecipTotal)
station1$ConsRain = 0
# actually really need to do this for each year
s1years = unique(year(station1$Date))
for (i in 1:length(s1years)){
  y = s1years[i]
  dts = station1$Date[station1$Year==y]
  counter = 0
  for (j in 1:length(dts)){
    # have: the dates within each year inside this loop
    # want: to know consequtive days with rain - 
    # Need to start at first date - see if it rains and then add to some sort of counter
    # Each day assign 2 numbers; current counter - every new days it rains increase this by 1
    # if it doesn't rain reset to 0, every iteration through loop want to assign this after
    # it has been increase
    if (station1$PrecipTotal[station1$Date == dts[j]] != 0) {
      counter = counter + 1
    } else {
      counter = 0
    }
    station1$ConsRain[station1$Date == dts[j]] = counter
  }
}


# sunrise / sunset data

# will have to string split?


# this gives sunrise in minutes
station1$Sunrise = as.character(station1$Sunrise)
station1$Sunrise_min = 0
for (i in 1:length(station1$Sunrise)){
  station1$Sunrise_min[i] = as.numeric(substr(station1$Sunrise[i], 1, 1)) * 60 + as.numeric(substr(station1$Sunrise[i], 2, 3))
}

station1$Sunset = as.character(station1$Sunset)
as.numeric(substr(station1$Sunset[1], 1, 2)) * 60 + as.numeric(substr(station1$Sunset[1], 3, 4))

station1$Sunset_min = 0
for (i in 1:length(station1$Sunset)){
  station1$Sunset_min[i] = as.numeric(substr(station1$Sunset[i], 1, 2)) * 60 + as.numeric(substr(station1$Sunset[i], 3, 4))
}

station1$Sun_min = 0
station1$Sun_min = station1$Sunset_min - station1$Sunrise_min

