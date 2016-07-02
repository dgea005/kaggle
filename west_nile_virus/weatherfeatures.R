# want to add these columns in by station
# probably best to add in a column of 0s first for each new feature

# weather Tavg_28 days=================================================
# With the previous 28 days for the earlier dates? i.e., first 28 of month
weather2 = data.frame(weather$Date, weather$Station)
names(weather2) = c("Date", "Station")
head(weather2)


weather2$Tavg_28 = 0
weather2$Tavg_28sd = 0
for (i in 1:length(unique(weather$Date))){
  obs_date = unique(weather$Date)[i]
  for (k in 1:2){
    avg = mean(weather$Tavg[weather$Date <= obs_date & 
                              (weather$Date >= obs_date - days(28)) & 
                               weather$Station == k])
    std = sd(weather$Tavg[weather$Date <= obs_date & 
                              (weather$Date >= obs_date - days(28)) & 
                              weather$Station == k])
    weather2$Tavg_28[weather$Station == k & weather$Date == obs_date] = avg
    weather2$Tavg_28sd[weather$Station == k & weather$Date == obs_date] = std
  }
}

# weather Tavg_12 days=================================================
weather2$Tavg_12 = 0
weather2$Tavg_12sd = 0
for (i in 1:length(unique(weather$Date))){
  obs_date = unique(weather$Date)[i]
  for (k in 1:2){
    avg = mean(weather$Tavg[weather$Date <= obs_date & 
                                 (weather$Date >= obs_date - days(12)) & 
                                 weather$Station == k])
    std = mean(weather$Tavg[weather$Date <= obs_date & 
                              (weather$Date >= obs_date - days(12)) & 
                              weather$Station == k])
    weather2$Tavg_12[weather$Station == k & weather$Date == obs_date] = avg
    weather2$Tavg_12sd[weather$Station == k & weather$Date == obs_date] = std
  }
}

# weather Tavg_05 days=================================================
weather2$Tavg_05 = 0
weather2$Tavg_05sd = 0
for (i in 1:length(unique(weather$Date))){
  obs_date = unique(weather$Date)[i]
  for (k in 1:2){
    avg = mean(weather$Tavg[weather$Date <= obs_date & 
                                 (weather$Date >= obs_date - days(5)) & 
                                 weather$Station == k])
    std = sd(weather$Tavg[weather$Date <= obs_date & 
                              (weather$Date >= obs_date - days(5)) & 
                              weather$Station == k])
    
    weather2$Tavg_05[weather$Station == k & weather$Date == obs_date] = avg
    weather2$Tavg_05sd[weather$Station == k & weather$Date == obs_date] = std
  }
}

# weather Tavg_02 days=================================================
weather2$Tavg_02 = 0
weather2$Tavg_02sd = 0
for (i in 1:length(unique(weather$Date))){
  obs_date = unique(weather$Date)[i]
  for (k in 1:2){
    avg = mean(weather$Tavg[weather$Date <= obs_date & 
                                 (weather$Date >= obs_date - days(2)) & 
                                 weather$Station == k])
    std = sd(weather$Tavg[weather$Date <= obs_date & 
                              (weather$Date >= obs_date - days(2)) & 
                              weather$Station == k])
    weather2$Tavg_02[weather$Station == k & weather$Date == obs_date] = avg
    weather2$Tavg_02sd[weather$Station == k & weather$Date == obs_date] = std
  }
}

# standard deviation of Tavg

# PrecipAv 28 days
weather2$PrcpT_28 = 0
weather2$PrcpT_28sd = 0
for (i in 1:length(unique(weather$Date))){
  obs_date = unique(weather$Date)[i]
  for (k in 1:2){
    avg = mean(weather$PrecipTotal[weather$Date <= obs_date & 
                                 (weather$Date >= obs_date - days(28)) & 
                                 weather$Station == k])
    std = sd(weather$PrecipTotal[weather$Date <= obs_date & 
                                     (weather$Date >= obs_date - days(28)) & 
                                     weather$Station == k])
    weather2$PrcpT_28[weather$Station == k & weather$Date == obs_date] = avg
    weather2$PrcpT_28sd[weather$Station == k & weather$Date == obs_date] = std
  }
}

# Prcp 10 days
weather2$PrcpT_10 = 0
weather2$PrcpT_10sd = 0
for (i in 1:length(unique(weather$Date))){
  obs_date = unique(weather$Date)[i]
  for (k in 1:2){
    avg = mean(weather$PrecipTotal[weather$Date <= obs_date & 
                                        (weather$Date >= obs_date - days(10)) & 
                                        weather$Station == k])
    std = sd(weather$PrecipTotal[weather$Date <= obs_date & 
                                        (weather$Date >= obs_date - days(10)) & 
                                        weather$Station == k])
    
    weather2$PrcpT_10[weather$Station == k & weather$Date == obs_date] = avg
    weather2$PrcpT_10sd[weather$Station == k & weather$Date == obs_date] = std
  }
}


# plot(weather$PrcpT_10[year(weather$Date) == 2007 & weather$Station == 1], type = "l")
# lines(weather$PrcpT_28[year(weather$Date) == 2007 & weather$Station == 2], col = "red")


# week min and max temperatures

# 7 days min and max temperature
weather2$Tmin_min7 = 0
weather2$Tmax_max7 = 0
for (i in 1:length(unique(weather$Date))){
  obs_date = unique(weather$Date)[i]
  for (k in 1:2){
    minimum = min(weather$Tmin[weather$Date <= obs_date & 
                                 (weather$Date >= obs_date - days(7)) & 
                                 weather$Station == k])
    maximum = max(weather$Tmax[weather$Date <= obs_date & 
                                 (weather$Date >= obs_date - days(7)) & 
                                 weather$Station == k])
    weather2$Tmin_min7[weather$Station == k & weather$Date == obs_date] = minimum 
    weather2$Tmax_max7[weather$Station == k & weather$Date == obs_date] = maximum
  }
}



# rain in past week
weather2$Rain_7 = 0
weather2$Rain_7Total = 0
for (i in 1:length(unique(weather$Date))){
  obs_date = unique(weather$Date)[i]
  for (k in 1:2){
    rain_total = sum(weather$PrecipTotal[weather$Date <= obs_date & 
                                 (weather$Date >= obs_date - days(7)) & 
                                 weather$Station == k])
    rain_bin = as.numeric(rain_total > 0)
    
    weather2$Rain_7[weather$Station == k & weather$Date == obs_date] = rain_bin
    weather2$Rain_7Total[weather$Station == k & weather$Date == obs_date] = rain_total
  }
}

#DewPoint average
weather2$Dew14 = 0
for (i in 1:length(unique(weather$Date))){
  obs_date = unique(weather$Date)[i]
  for (k in 1:2){
    avg = mean(weather$DewPoint[weather$Date <= obs_date & 
                                     (weather$Date >= obs_date - days(14)) & 
                                     weather$Station == k])
    weather2$Dew14[weather$Station == k & weather$Date == obs_date] = avg
  }
}






