vars = subset(train, select = -c(Date, Address, Species, Street, AddressNumberAndStreet,
                                 NumMosquitos, WnvPresent, Id))
vars$Trap = as.numeric(vars$Trap)
vars$Species2 = as.numeric(vars$Species2)

var_names = names(vars)
m = data.frame(matrix(0, nrow = length(var_names)-1, ncol = length(var_names)))
names(m) = var_names


for(i in 1:(length(var_names)-1)){
  for(j in (i+1):length(var_names)){
    d = vars[,var_names[i]] - vars[,var_names[j]]
    m[i,j] = cor(d,train$WnvPresent)
  }
}

for (i in 1:length(var_names)){
  par(ask=T)
  plot(m[,var_names[i]], ylab = as.character(var_names[i]), ylim = c(-1,1))
}


# Have a look at...
# suspect most of this is with 0s...
# CE, CP, CS, Tavg1_prcpAv, Tavg1_prcpS, raink_wk, PrecipTotal, StnPressure,
# BR, SeaLevel, SnowFall, 


# What is the other variable?
m$hSunset[20]
cor( abs(vars[,var_names[20]] - vars[,"hSunset"]) , train$WnvPresent)
var_names[20]

# could try difference of Tavg1_mAv and CE
# could try differenc of Tavg1_mAv and hSunset

# could look at which traps have Wnv and incllude only those as factors?
table(train$Trap, by = train$WnvPresent)
unique(train$Trap[train$WnvPresent == 1])
# there are still quite a few, we want more than 1 case
table(train$Trap, by = train$WnvPresent)

# traps with WnvPresent
# will need to see if the traps are the same in the test

# interested to try making a model with just these vars?
#hsunset
#NMprd for min


#interested to KNN (as a feature)

# interested in running some time series stuff to find pacf, acf relationsips
# how to encode data to do so?

# # or just move on to nnnet?
# https://kaggle2.blob.core.windows.net/forum-message-attachments/40922/1144/HelloWorld_Documentation.pdf?sv=2012-02-12&se=2015-06-13T14%3A17%3A54Z&sr=b&sp=r&sig=lxxCqndIbKOq8IfXOPrbExPLNfpCEGe%2FiGyuIvhqdU4%3D
# 
# http://stats.stackexchange.com/questions/17565/choosing-the-best-model-from-among-different-best-models
# http://cran.r-project.org/web/packages/gbm/gbm.pdf
# http://cran.r-project.org/web/packages/ROSE/ROSE.pdf
# knn

# could also play around with logit classification level, 0.5, 0.55, etc










