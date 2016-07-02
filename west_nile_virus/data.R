# need to arange data to be usable
library(plyr)
library(lubridate)

data_dir = "/data"

train = read.csv(file.path(data_dir, "train.csv.zip"))
weather = read.csv(file.path(data_dir, "weather.csv.zip"))
spray = read.csv(file.path(data_dir, "spray.csv.zip"))
test = read.csv(file.path(data_dir, "test.csv.zip"))
# will also need to prep test -> move unspecified species; culex to 
# culex arriticus -- test

# now need to figure out how to get features as what the predictions will be

# first need labels / y-values
labels = train["WnvPresent"]

source("weatherfix.R")

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

#train$Date = levels(train$Date)[as.numeric(train$Date)]
#train$Date = as.Date(train$Date)
#train$Year = year(train$Date)
#train$Month = month(train$Date)
#train$Day = day(train$Date)
#train$Yday = yday(train$Date)
#train$Week = week(train$Date)

#test$Date = levels(test$Date)[as.numeric(test$Date)]
#test$Date = as.Date(test$Date)
#test$Year = year(test$Date)
#test$Month = month(test$Date)
#test$Day = day(test$Date)
#test$Yday = yday(test$Date)
#test$Week = week(test$Date)


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

# want to try creating dummy variables for each of the 7 species + an additional column of 1's for unspecified culex
# this will replace Species2 in the analysis

train$CE = 0
train$CE[train$Species == "CULEX ERRATICUS"] = 1
train$CP = 0
train$CP[train$Species == "CULEX PIPIENS"] = 1
train$CLXPR = 0
train$CLXPR[train$Species == "CULEX PIPIENS/RESTUANS"] = 1
train$CR = 0
train$CR[train$Species == "CULEX RESTUANS"] = 1
train$CS = 0
train$CS[train$Species == "CULEX SALINARIUS"] = 1
train$CTAR = 0
train$CTAR[train$Species == "CULEX TARSALIS"] = 1
train$CTER = 0
train$CTER[train$Species == "CULEX TERRITANS"] = 1
train$CUNSP = 1

test$CE = 0
test$CE[test$Species == "CULEX ERRATICUS"] = 1
test$CP = 0
test$CP[test$Species == "CULEX PIPIENS"] = 1
test$CLXPR = 0
test$CLXPR[test$Species == "CULEX PIPIENS/RESTUANS"] = 1
test$CR = 0
test$CR[test$Species == "CULEX RESTUANS"] = 1
test$CS = 0
test$CS[test$Species == "CULEX SALINARIUS"] = 1
test$CTAR = 0
test$CTAR[test$Species == "CULEX TARSALIS"] = 1
test$CTER = 0
test$CTER[test$Species == "CULEX TERRITANS"] = 1
test$CUNSP = 1




# average of 1 month temperature for each obs
# the 1 refers to this being of station1 data
# this was 28 in best model
for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  month_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(27)), ]
  train[train$Date == obs_date, "Tavg1_mAv"] = mean(month_subset$Tavg)
}

# standard deviation of average temperatures for 1 month of tavg
# also just station1 data
for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  month_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(28)), ]
  train[train$Date == obs_date, "Tavg1_mS"] = sd(month_subset$Tavg)
}


# going to try out station2
for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  month_subset = station2[station2$Date <= obs_date & (station2$Date >= obs_date - days(24)), ]
  train[train$Date == obs_date, "Tavg2_mAv"] = mean(month_subset$Tavg)
}

# going to try out station2
for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  month_subset = station2[station2$Date <= obs_date & (station2$Date >= obs_date - days(24)), ]
  test[test$Date == obs_date, "Tavg2_mAv"] = mean(month_subset$Tavg)
}


# insert for test

for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  month_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(27)), ]
  test[test$Date == obs_date, "Tavg1_mAv"] = mean(month_subset$Tavg)
}

for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  month_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(28)), ]
  test[test$Date == obs_date, "Tavg1_mS"] = sd(month_subset$Tavg)
}

# need to add previous week statistics


# will try adding some rain data from station1
# will start with average precip total from previous month
# then will add sd
# and previous week mean + sd

for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  month_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(7)), ]
  train[train$Date == obs_date, "Tavg1_prcpAv"] = mean(month_subset$PrecipTotal)
}

for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  month_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(7)), ]
  train[train$Date == obs_date, "Tavg1_prcpS"] = sd(month_subset$PrecipTotal)
}


# test

for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  month_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(14)), ]
  test[test$Date == obs_date, "Tavg1_prcpAv"] = mean(month_subset$PrecipTotal)
}

for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  month_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(14)), ]
  test[test$Date == obs_date, "Tavg1_prcpS"] = sd(month_subset$PrecipTotal)
}


# week min and max temperatures
for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(7)), ]
  train[train$Date == obs_date, "Tmin1_wk"] = min(data_subset$Tmin)
}


for (i in 1:length(unique(train$Date))){
  obs_date = unique(train$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(7)), ]
  train[train$Date == obs_date, "Tmax1_wk"] = max(data_subset$Tmax)
}




# test week min and max temperatures
for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(7)), ]
  test[test$Date == obs_date, "Tmin1_wk"] = min(data_subset$Tmin)
}


for (i in 1:length(unique(test$Date))){
  obs_date = unique(test$Date)[i]
  data_subset = station1[station1$Date <= obs_date & (station1$Date >= obs_date - days(7)), ]
  test[test$Date == obs_date, "Tmax1_wk"] = max(data_subset$Tmax)
}

# could include a binary variable for
# rain in past week, 

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




# ===================================
# so will say there are over 50 mosquitos if:
# Multiple entries of SAME: Trap, Date and Species









# this would be easier if there was an observation ID
# will have to create one

# create an ID so that we can record rows of interest
# ids = seq(1, dim(train)[1])
# train$Id = ids
# 
# # will want a new column of 0s
# train$MosDup = 0
# traps = unique(train$Trap)
# for (i in 1:length(traps)){
#   # subset will contain only one specific Trap
#   trap_subset = train[train$Trap == traps[i], ]
#   # current_trap = traps[i]
#   # get unique dates of specific trap
#   dates = unique(trap_subset$Date)
#   for (j in 1:length(dates)){
#     # subset of date within trap i
#     date_subset = trap_subset[trap_subset$Date == dates[j], ]
#     # current_date = dates[j]
#     # species contained in date subset
#     species = unique(date_subset$Species2)
#     for (k in 1:length(species)){
#       # will need to find out if there are duplicate species
#       species_subset = date_subset[date_subset$Species2 == species[k], ]
#       if (dim(species_subset)[1] > 1){
#         # should record Id's to check this
#         train[train$Trap == traps[i] & train$Date == dates[j] & train$Species2 == species[k], "MosDup"] = 1
#       }
#     }
#   }
# }
# 
# 
# 
# # will want a new column of 0s
# train$MosDup2 = 0
# traps = unique(train$Trap)
# for (i in 1:length(traps)){
#   # subset will contain only one specific Trap
#   trap_subset = train[train$Trap == traps[i], ]
#   # current_trap = traps[i]
#   # get unique dates of specific trap
#   dates = unique(trap_subset$Date)
#   for (j in 1:length(dates)){
#     # subset of date within trap i
#     date_subset = trap_subset[trap_subset$Date == dates[j], ]
#     # current_date = dates[j]
#     # species contained in date subset
#     species = unique(date_subset$Species2)
#     for (k in 1:length(species)){
#       # will need to find out if there are duplicate species
#       species_subset = date_subset[date_subset$Species2 == species[k], ]
#       train[train$Trap == traps[i] & train$Date == dates[j] & train$Species2 == species[k], "MosDup2"] = dim(species_subset)[1]
#     }
#   }
# }
# 
# 
# test$MosDup2 = 0
# traps = unique(test$Trap)
# for (i in 1:length(traps)){
#   # subset will contain only one specific Trap
#   trap_subset = test[test$Trap == traps[i], ]
#   # current_trap = traps[i]
#   # get unique dates of specific trap
#   dates = unique(trap_subset$Date)
#   for (j in 1:length(dates)){
#     # subset of date within trap i
#     date_subset = trap_subset[trap_subset$Date == dates[j], ]
#     # current_date = dates[j]
#     # species contained in date subset
#     species = unique(date_subset$Species2)
#     for (k in 1:length(species)){
#       # will need to find out if there are duplicate species
#       species_subset = date_subset[date_subset$Species2 == species[k], ]
#       test[test$Trap == traps[i] & test$Date == dates[j] & test$Species2 == species[k], "MosDup2"] = dim(species_subset)[1]
#     }
#   }
# }
# 
# 
# 
# 
# 
# test$MosDup = 0
# traps = unique(test$Trap)
# for (i in 1:length(traps)){
#   # subset will contain only one specific Trap
#   trap_subset = test[test$Trap == traps[i], ]
#   # current_trap = traps[i]
#   # get unique dates of specific trap
#   dates = unique(trap_subset$Date)
#   for (j in 1:length(dates)){
#     # subset of date within trap i
#     date_subset = trap_subset[trap_subset$Date == dates[j], ]
#     # current_date = dates[j]
#     # species contained in date subset
#     species = unique(date_subset$Species2)
#     for (k in 1:length(species)){
#       # will need to find out if there are duplicate species
#       species_subset = date_subset[date_subset$Species2 == species[k], ]
#       if (dim(species_subset)[1] > 1){
#         # should record Id's to check this
#         test[test$Trap == traps[i] & test$Date == dates[j] & test$Species2 == species[k], "MosDup"] = 1
#       }
#     }
#   }
# }
# would be good to save a copy since this takes so long?
# options("scipen"=100, "digits"=8)
# write.csv(test,"testsave.csv",row.names=FALSE,quote=FALSE)
# write.csv(train, "trainsave.csv", row.names=FALSE, quote=FALSE)




# need to get some other features back

sctrain =  read.csv("C:\\Users\\Daniel\\Dropbox\\Projects\\applied\\mozzys\\kaggle_scripts\\scripttrain.csv")
sctest =  read.csv("C:\\Users\\Daniel\\Dropbox\\Projects\\applied\\mozzys\\kaggle_scripts\\scripttest.csv")

#train = train_save
train$Tmax = sctrain$Tmax
train$Tmin = sctrain$Tmin
train$Tavg = sctrain$Tavg
train$DewPoint = sctrain$DewPoint
train$WetBulb = sctrain$WetBulb
train$PrecipTotal = sctrain$PrecipTotal
train$StnPressure = sctrain$StnPressure
train$BR = sctrain$BR
train$SeaLevel = sctrain$SeaLevel
train$SnowFall = sctrain$SnowFall
train$AvgSpeed = sctrain$AvgSpeed
train$hSunrise = sctrain$hSunrise
train$hSunset = sctrain$hSunset
train$mSunset = sctrain$mSunset
train$mSunrise = sctrain$mSunrise
train$Depart = sctrain$Depart

test$Tmax = sctest$Tmax
test$Tmin = sctest$Tmin
test$Tavg = sctest$Tavg
test$DewPoint = sctest$DewPoint
test$WetBulb = sctest$WetBulb
test$PrecipTotal = sctest$PrecipTotal
test$StnPressure = sctest$StnPressure
test$BR = sctest$BR
test$SeaLevel = sctest$SeaLevel
test$SnowFall = sctest$SnowFall
test$AvgSpeed = sctest$AvgSpeed
test$hSunrise = sctest$hSunrise
test$hSunset = sctest$hSunset
test$mSunset = sctest$mSunset
test$mSunrise = sctest$mSunrise
test$Depart = sctest$Depart



# test/ train dump

# save(file = "traind.Rda", train)
# save(train, file = "traind.rds")
# saveRDS(test, "testd.rds")
# write.csv(train, file = "saveddf.csv")
# write.csv(test, file = "savetest.csv")
# 


load("traind.Rda")
# test = read.csv("savetest.csv")
# test variables
MosDup = read.csv("MosDup.csv")
MosDup2 = read.csv("MosDup2.csv")

test$MosDup = MosDup$x
test$MosDup2 = MosDup2$x




# The top traps in train with WnvPresent are:
# T900, T115, T002, T138, T003, T011, T128
train$T900 = as.numeric(train$Trap=='T900')
train$T115 = as.numeric(train$Trap=='T115')
train$T002 = as.numeric(train$Trap=='T138')
train$T138 = as.numeric(train$Trap=='T003')
train$T011 = as.numeric(train$Trap=='T011')
train$T128 = as.numeric(train$Trap=='T128')
train$T003 = as.numeric(train$Trap=='T003')

test$T900 = as.numeric(test$Trap=='T900')
test$T115 = as.numeric(test$Trap=='T115')
test$T002 = as.numeric(test$Trap=='T138')
test$T138 = as.numeric(test$Trap=='T003')
test$T011 = as.numeric(test$Trap=='T011')
test$T128 = as.numeric(test$Trap=='T128')
test$T003 = as.numeric(test$Trap=='T003')

# what about traps that are empty? 




# could try merging
temp_sub_s1 = subset(station1, select = c(Date, ConsRain))
train = merge(train, temp_sub_s1, by = c("Date"))
test = merge(test, temp_sub_s1, by = c("Date"))

# 
# save(file = "train2.Rda", train)
# save(file = "test2.Rda", test)
# 

#load("train2.Rda")
#load("test2.Rda")
