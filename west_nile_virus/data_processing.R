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



