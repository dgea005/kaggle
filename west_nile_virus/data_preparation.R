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


## rain during past x days: rolling window
train = create_variable_from_weather(train, station1, 'PrecipTotal', 'rain_wk', 11, sum)
train = create_variable_from_weather(train, station1, 'PrecipTotal', 'rain_wk15', 30, sum)

test = create_variable_from_weather(test, station1, 'PrecipTotal', 'rain_wk', 11, sum)
test = create_variable_from_weather(test, station1, 'PrecipTotal', 'rain_wk15', 30, sum)

## DewPoint and Wetbulb: rolling window past x days
train = create_variable_from_weather(train, station1, 'DewPoint', 'dew_wk', 15, mean) 
test = create_variable_from_weather(test, station1, 'DewPoint', 'dew_wk', 15, mean)

train = create_variable_from_weather(train, station1, 'DewPoint', 'dew_sd', 15, sd) 
test = create_variable_from_weather(test, station1, 'DewPoint', 'dew_sd', 15, sd)

train = create_variable_from_weather(train, station1, 'WetBulb', 'wb_wk', 4, mean) 
test = create_variable_from_weather(test, station1, 'WetBulb', 'wb_wk', 4, mean)

train = create_variable_from_weather(train, station1, 'WetBulb', 'wb_sd', 10, sd) 
test = create_variable_from_weather(test, station1, 'WetBulb', 'wb_sd', 10, sd)

## windspeed
train = create_variable_from_weather(train, station1, 'AvgSpeed', 'speed_wk', 10, mean) 
test = create_variable_from_weather(test, station1, 'AvgSpeed', 'speed_wk', 10, mean)

train = create_variable_from_weather(train, station1, 'AvgSpeed', 'speed_sd', 7, sd) 
test = create_variable_from_weather(test, station1, 'AvgSpeed', 'speed_sd', 7, sd)

train = create_variable_from_weather(train, station1, 'ResultDir', 'dir', 28, mean) 
test = create_variable_from_weather(test, station1, 'ResultDir', 'dir', 28, mean) 

# minutes of sunlight
## originally was looking at a window of 28-20 days ago, etc
train = create_variable_from_weather(train, station1, 'Sun_min', 'sunlight', 28, mean) 
test = create_variable_from_weather(test, station1, 'Sun_min', 'sunlight', 28, mean)

## some daily values
## Tmin, Tmax, Tavg - on day of measurement

s1_temps = dplyr::select(station1, Date, Tmin, Tmax, Tavg, PrecipTotal)%>%
              group_by(Date, Tmin, Tmax, Tavg, PrecipTotal)
train = unique(inner_join(train, s1_temps, by='Date'))
test = unique(inner_join(test, s1_temps, by='Date'))


##
## duplicate trap recording stuff
## max of 50 records per row - so traps with more are kind of duplicated
## want to find these
## also remove duplicate rows here...
##


# here are over 50 mosquitos if:
# Multiple entries of SAME: Trap, Date and Species

duplicate_traps <- function(data){
  dup_occurances = dplyr::select(data, Trap, Date, Species2) %>%
                      group_by(Trap, Date, Species2) %>%
                      mutate(MosDup2 = n())
  data = unique(inner_join(data, dup_occurances))
  data$MosDup = as.integer((data$MosDup2 == 1)==FALSE)
  return(data)
}

train = duplicate_traps(train)
test = duplicate_traps(test)

source('predict_num_mosquitos.R')
