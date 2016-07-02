library(Metrics)
library(MASS)
library(gam)

set.seed(2001)

# select data to be used for train
my.train = subset(train, select = c(Yday, Day, Month, Week, Latitude, Longitude, 
                                    WnvPresent, Species2, Tavg1_mAv, Tavg1_mS,
                                    Tavg1_prcpAv, Tavg1_prcpS, Tmin1_wk, Tmax1_wk,
                                    rain_wk, MosDup, MosDup2, Tmin, Tmax, Tavg,
                                    WetBulb, PrecipTotal, StnPressure, BR, SeaLevel,
                                    SnowFall, AvgSpeed, hSunrise, hSunset, mSunset, mSunrise, NMprd))
# T900, T115, T002, T138, T003, T011, T128

# should try include dayofyear
# model = formula(WnvPresent ~ poly(Yday,2) + poly(Week,2) + I(Longitude^2) +
#                Species2 + Tavg2_mAv  + I(1/log(Tavg1_mS)) + poly(Tmin1_wk,2) + I(log1p(rain_wk)^(2/3)) +
#                  I(Tavg1_prcpAv^(1/10)) + 1/log1p(PrecipTotal)  + 1/AvgSpeed
#                + MosDup + 1/(log1p(MosDup2)) + T900 + T003
#                + log1p(ConsRain) + I(dew_wk^5) + speed_wk + wb_wk + speed_sd + sunlight)



# model = formula(WnvPresent ~ poly(Yday,2) + poly(Week, 2) + Month + lo(Latitude, Longitude) +
#                   Species2 + Tavg2_mAv  + Tavg1_mS + poly(Tmin1_wk,2) + rain_wk + 
#                   I(Tavg1_prcpAv^(1/4)) + MosDup + log(MosDup2)+ poly(Tmin, 5)  +
#                   log1p(PrecipTotal + 1) + I(Tavg^4) + 
#                   T900  + speed_wk + speed_sd + sunlight + wb_wk + I(dew_wk^5) +
#                   T003 + 
#                   I(1/NMprd^(2/3)) + I(1/AvgSpeed) + log1p(ConsRain))


model = formula(WnvPresent ~ poly(Yday,2) + poly(Week, 2) + Month + lo(Latitude, Longitude) +
                  Species2 + Tavg1_mAv  + Tavg1_mS + poly(Tmin1_wk,2) + rain_wk + 
                  I(Tavg1_prcpAv^(1/4)) + MosDup + log(MosDup2)+ poly(Tmin, 5)  +
                  log1p(PrecipTotal + 1) + I(Tavg^4) +  
                  +  I(1/NMprd^(2/3)) )

#    0.84145 (by year) and 0.85538 by (30/70) I(1/NMprd^(2/3)) +
# I(1/NMprd^(0.25))  0.844030 (by year) & 0.85608 (by 30/70)

# 


submit = FALSE
fname = "submitglm33.csv"
fam = binomial



# removed poly(Tmin, 2) # removwd Tmin Tavg I(1/log(Tavg1_mS)) I(abs(Week-31))

# I(log1p(ConsRain)^(1/10)
# 138, 003, 
# + I(Tavg1_mAv - hSunset)

#fitSubmit = glm(model, data = my.train, family = "binomial")
#summary(fitSubmit)
# + I(1/NMprd^(2/3)) + I(1/AvgSpeed)

#log(NMprd)
# + I((NMprd - mean(NMprd))/(sd(NMprd))) 
# It looks like both of these distributions need to be shifted back by about 10-20? NMprd
# There aren't enough traps with very low frequency, compare to 



#+ MosDup + log(MosDup2)

# + MosDup + I(1/MosDup^(2.5))
# Tmax doesn't work with polynomial
# + WetBulb + PrecipTotal +  StnPressure  + BR
# + (1/Tavg)



#formula 

# # create data frame to hold results
# rounds = 1:20
# cv = data.frame(matrix(0, ncol=4, nrow=length(rounds)+2))
# names(cv) = c("2007", "2009", "2011", "2013")
# 
# 
# 
# #2007 cv
x1 = my.train[train$Year != 2007, ]
x2 = my.train[train$Year == 2007, ]
glmcv = glm(model, data = x1, family = fam, control=glm.control(maxit=100))
p2 = predict(glmcv, newdata = x2, type = "response")
auc07 = auc(x2$WnvPresent, p2)

#2009 cv
x1 = my.train[train$Year != 2009, ]
x2 = my.train[train$Year == 2009, ]
glmcv = glm(model, data = x1, family = fam, control=glm.control(maxit=100))
p2 = predict(glmcv, newdata = x2, type = "response")
auc09 = auc(x2$WnvPresent,p2)

# 2011 cv
x1<-my.train[train$Year!=2011,]
x2<-my.train[train$Year==2011,]
glmcv = glm(model, data = x1, family = fam, control=glm.control(maxit=100))
p2 = predict(glmcv, newdata = x2, type = "response")
auc11 = auc(x2$WnvPresent,p2)
   
# 2013 cv
x1<-my.train[train$Year!=2013,]
x2<-my.train[train$Year==2013,]#   
glmcv = glm(model, data = x1, family = fam, control=glm.control(maxit=100))
p2 = predict(glmcv, newdata = x2, type = "response")
auc13 = auc(x2$WnvPresent,p2)



# # # # ## now fit a new model to all the data
if (submit == TRUE){
  fitSubmit = glm(model, data = my.train, family = fam)
  pSubmit<-predict(fitSubmit, newdata = test, type = "response")
  ## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
  summary(pSubmit)
  
  submissionFile<-cbind(test$Id,pSubmit)
  colnames(submissionFile)<-c("Id","WnvPresent")
  options("scipen"=100, "digits"=8)
  write.csv(submissionFile, fname, row.names=FALSE,quote=FALSE)
}


#another split and test option
rounds2 = 1:10
auc3070 = rep(0, length(rounds2))
for (i in rounds2){
  sampleIndex = sample(nrow(my.train), size = 2500, replace = TRUE)
  OOBIndex = setdiff(1:nrow(my.train), sampleIndex)
  x1 = my.train[sampleIndex,]
  x2 = my.train[OOBIndex, ]
  glmcv = glm(model, data = x1, family = fam, control=glm.control(maxit=100))
  p2 = predict(glmcv, newdata = x2, type = "response")
  tmp = auc(x2$WnvPresent,p2)
  auc3070[i] = tmp
}


cat(paste0("30-70 split with ", max(rounds2), " of cv has mean: ", mean(auc3070), " and sd: ", sd(auc3070), "\n",
           "07: ", auc07, "\n09: ", auc09, "\n11: ", auc11, "\n13: ", auc13, ",\nmean: ", mean(c(auc07,auc09,auc11,auc13))))



