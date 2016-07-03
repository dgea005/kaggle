##
## script to preprocess data for WNV
## imputes missing values, converts types and add features
##


library(plyr)
library(dplyr)
library(lubridate)


# read in data
data_dir = "data"
train = read.csv(file.path(data_dir, "train.csv"))
weather = read.csv(file.path(data_dir, "weather.csv"))
spray = read.csv(file.path(data_dir, "spray.csv"))
test = read.csv(file.path(data_dir, "test.csv"))

##
## preprocessing of weather.csv data
## parses description, fill missing data ,etc.
##

source("weather_preprocess.R")


##
## date conversion
##

add_date_information <- function(data){
  data$Date = levels(data$Date)[as.numeric(data$Date)]
  data$Date = as.Date(data$Date)
  data$Year = year(data$Date)
  data$Month = month(data$Date)
  data$Day = day(data$Date)
  data$Yday = yday(data$Date)
  data$Week = week(data$Date)
  return(data)  
}

train = add_date_information(train)
test = add_date_information(test)


##
## species preprocessing
##


## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
# create vector of train then test species columns
vSpecies<-c(as.character(train$Species),as.character(test$Species))
# convert unspecified to culex erraticus
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies[-which(vSpecies == "CULEX PIPIENS" |
                vSpecies == "CULEX PIPIENS/RESTUANS" |
                vSpecies == "CULEX RESTUANS")] = "CULEX OTHER"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))

train$Species2 = factor(vSpecies[1:nrow(train)], levels=unique(vSpecies))
test$Species2 = factor(vSpecies[(nrow(train)+1):length(vSpecies)], levels=unique(vSpecies))


## create dummy/ one hot encoded variables
## + an additional column of 1's for unspecified culex

add_species_dummies <- function(data){
  data$CE = 0
  data$CE[data$Species == "CULEX ERRATICUS"] = 1
  data$CP = 0
  data$CP[data$Species == "CULEX PIPIENS"] = 1
  data$CLXPR = 0
  data$CLXPR[data$Species == "CULEX PIPIENS/RESTUANS"] = 1
  data$CR = 0
  data$CR[data$Species == "CULEX RESTUANS"] = 1
  data$CS = 0
  data$CS[data$Species == "CULEX SALINARIUS"] = 1
  data$CTAR = 0
  data$CTAR[data$Species == "CULEX TARSALIS"] = 1
  data$CTER = 0
  data$CTER[data$Species == "CULEX TERRITANS"] = 1
  data$CUNSP = 1
  return(data)
}

train = add_species_dummies(train)
test = add_species_dummies(test)


##
## create weather features
## 


## window function to create weather from previous x days of weather data
create_variable_from_weather <- function(data, wthr_dt, weather_var, new_col_name, days_, func){
  unique_dates = unique(data$Date)
  for (i in 1:length(unique_dates)){
    obs_date = unique_dates[i]
    data_subset = wthr_dt[wthr_dt$Date <= obs_date & (wthr_dt$Date >= obs_date - days(days_)), weather_var]
    data[data$Date == obs_date, new_col_name] = do.call(func, list(na.omit(data_subset)))
  }
  return(data)
}

## average temperatures: one month rolling window

train = create_variable_from_weather(train, station1, 'Tavg', 'Tavg1_mAv', 28, mean)
train = create_variable_from_weather(train, station1, 'Tavg', 'Tavg1_mS', 28, sd)  #std deviation
train = create_variable_from_weather(train, station2, 'Tavg', 'Tavg2_mAv', 24, mean)
test = create_variable_from_weather(test, station1, 'Tavg', 'Tavg1_mAv', 28, mean)
test = create_variable_from_weather(test, station1, 'Tavg', 'Tavg1_mS', 28, sd)  #std deviation
test = create_variable_from_weather(test, station2, 'Tavg', 'Tavg2_mAv', 24, mean)



## precipitation: one week rolling window
train = create_variable_from_weather(train, station1, 'PrecipTotal', 'Tavg1_prcpAv', 7, mean)
train = create_variable_from_weather(train, station1, 'PrecipTotal', 'Tavg1_prcpS', 7, sd)
test = create_variable_from_weather(test, station1, 'PrecipTotal', 'Tavg1_prcpAv', 7, mean)
test = create_variable_from_weather(test, station1, 'PrecipTotal', 'Tavg1_prcpS', 7, sd)


## min and max temperatures: one week rolling window
train = create_variable_from_weather(train, station1, 'Tmin', 'Tmin1_wk', 7, min)
train = create_variable_from_weather(train, station1, 'Tmax', 'Tmax1_wk', 7, max)
test = create_variable_from_weather(test, station1, 'Tmin', 'Tmin1_wk', 7, min)
test = create_variable_from_weather(test, station1, 'Tmax', 'Tmax1_wk', 7, max)


## rain during week: rolling window




# changed 7 to 10
for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(11)), ]
  train[train$Date == obs_date, "rain_wk"] = as.numeric(sum(data_subset$PrecipTotal))
}


for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(30)), ]
  train[train$Date == obs_date, "rain_wk15"] = as.numeric(sum(data_subset$PrecipTotal))
}

for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(12)), ]
  test[test$Date == obs_date, "rain_wk"] = as.numeric(sum(data_subset$PrecipTotal))
}



# need to explore DewPoint and WetBulb
# found 15 days was the best for this
for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(15)), ]
  train[train$Date == obs_date, "dew_wk"] = mean(data_subset$DewPoint)
}

for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(15)), ]
  test[test$Date == obs_date, "dew_wk"] = mean(data_subset$DewPoint)
}


# dew sd

for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(15)), ]
  train[train$Date == obs_date, "dew_sd"] = sd(data_subset$DewPoint)
}

for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(15)), ]
  test[test$Date == obs_date, "dew_sd"] = sd(data_subset$DewPoint)
}


# wetbulb
for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(4)), ]
  train[train$Date == obs_date, "wb_wk"] = mean(data_subset$WetBulb)
}


for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(4)), ]
  test[test$Date == obs_date, "wb_wk"] = mean(data_subset$WetBulb)
}

for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(10)), ]
  train[train$Date == obs_date, "wb_sd"] = sd(data_subset$WetBulb)
}


for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(10)), ]
  test[test$Date == obs_date, "wb_sd"] = sd(data_subset$WetBulb)
}





for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(10)), ]
  train[train$Date == obs_date, "speed_wk"] = mean(data_subset$AvgSpeed)
}

for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(10)), ]
  test[test$Date == obs_date, "speed_wk"] = mean(data_subset$AvgSpeed)
}

for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(7)), ]
  train[train$Date == obs_date, "speed_sd"] = sd(data_subset$AvgSpeed)
}

for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(7)), ]
  test[test$Date == obs_date, "speed_sd"] = sd(data_subset$AvgSpeed)
}
# 10 best so far for speed_sd


# minutes of sunlight
# want to try moving average i.e., average from 21-28 days ago


for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date - days(25) & (station1$Date >= obs_date - days(28)), ]
  train[train$Date == obs_date, "sunlight"] = mean(data_subset$Sun_min)
}


for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  data_subset = station1[station1$Date <= obs_date - days(0) & (station1$Date >= obs_date - days(7)), ]
  test[test$Date == obs_date, "sunlight"] = mean(data_subset$Sun_min)
}


# direction?

for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(7)), ]
  train[train$Date == obs_date, "dir"] = sd(data_subset$ResultDir)
}




