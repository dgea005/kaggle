# need to arange data to be usable
library(plyr)
library(lubridate)


# read in data
data_dir = "data"
train = read.csv(file.path(data_dir, "train.csv"))
weather = read.csv(file.path(data_dir, "weather.csv"))
spray = read.csv(file.path(data_dir, "spray.csv"))
test = read.csv(file.path(data_dir, "test.csv"))

# preprocessing of weather.csv data
# parses description, fill missing data ,etc.
source("weather_preprocess.R")

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

for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  month_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(27)), ]
  train[train$Date == obs_date, "Tavg1_mAv"] = mean(month_subset$Tavg)
}





