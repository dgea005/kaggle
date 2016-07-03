library(Metrics)
library(MASS)
library(gam)

set.seed(2001)

# select data so it can to be used for train
my.train = subset(train, select = c(Yday, Month, Week, Latitude, Longitude, 
                                    WnvPresent, Species2, Tavg1_mAv, Tavg1_mS,
                                    Tavg1_prcpAv, Tmin1_wk,rain_wk, MosDup, 
                                    MosDup2, Tmin, Tavg,PrecipTotal, NMprd))
# create model formula
# transforming variables was rather successful here
model = formula(WnvPresent ~ poly(Yday,2) + poly(Week, 2) + Month + lo(Latitude, Longitude) +
                  Species2 + Tavg1_mAv  + Tavg1_mS + poly(Tmin1_wk,2) + rain_wk + 
                  I(Tavg1_prcpAv^(1/4)) + MosDup + log(MosDup2)+ poly(Tmin, 5)  +
                  log1p(PrecipTotal + 1) + I(Tavg^4) +  I(1/NMprd^(2/3)) )


submit = FALSE
fname = "submitglm33.csv"
fam = binomial

#formula 


## crossvalidation of model

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



