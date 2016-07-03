# exploration of distributions
# for transformation ideas...

# Response
hist(train$WnvPresent, breaks = 2)
# this clearly follows the poisson quite closely rather than a binomial dist
# could be useful to consider this in final model - remember 2012 though

# will look at variables in here
# model = formula(WnvPresent ~ poly(Yday,2) + poly(Week, 2) + Month + lo(Latitude, Longitude) +
#                   Species2 + Tavg1_mAv  + Tavg1_mS + poly(Tmin1_wk, 2) + rain_wk)


hist(train$Yday) 
# this looks really quite normal


hist(train$Week)
# this looks quite flat

hist(train$Month, breaks = 8)
# looks like a normal distribution


hist(train$Tavg1_mAv)
# looks like a skewed normal

hist(train$Tavg1_mS)
# looks like a skewed normal


hist(train$Tavg1_prcpAv)
# looks like exponential dist
hist(train$Tavg1_prcpAv^(1/4))


hist(train$Tavg1_prcpS)
# looks exponential
hist(1/(train$Tavg1_prcpS^(1/30)))
hist(train$Tavg1_prcpS^(1/3))



hist(train$Tmin1_wk^2)


hist(train$Tmax1_wk)
hist(1/(train$Tmax1_wk^(1/10)))
max(train$Tmax1_wk)






