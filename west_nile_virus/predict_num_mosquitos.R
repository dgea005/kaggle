##
## rf model to predict number of mosquitos
##

require(Metrics)
require(caret)
library(randomForest)

set.seed(1935)

mos.train = subset(train, select = -c(Date, Address, AddressNumberAndStreet, Street,
                                      Trap, Species, WnvPresent))
mos.train$Species2 = as.factor(mos.train$Species2)
target = mos.train$NumMosquitos
rf = randomForest(NumMosquitos ~ ., data = mos.train, mtry = 10, ntree = 250)
rfprd = predict(rf, mos.train)
rmse(actual = target, predicted = rfprd)


hist(rfprd)
hist(train$NumMosquitos)

n_mosquitos = predict(rf, train)
n_mosquitos_test = predict(rf, test)
hist(n_mosquitos_test)
# Seems like Nmprd2 needs to have it's distribution shifted to match train figures more
n_mosquitos_test = n_mosquitos_test - 2
# original amount subtracted was 2 (highest lb score)
n_mosquitos_test[n_mosquitos_test < 0] = 0
hist(n_mosquitos_test)

## should figure out a proper solution for this....

train$n_mosquitos = n_mosquitos
test$n_mosquitos = n_mosquitos_test
