##Data Loading
cell.data <- read.csv("D:\\MKT 591 - Marketing Analytics\\Telco Customer Churn\\Telco Customer Churn.csv",, header=TRUE)
for(i in 1:ncol(cell.data)){
  cell.data[is.na(cell.data[,i]), i] <- mean(cell.data[,i], na.rm = TRUE)
}

train.prop = 0.75
train.cases = sample(nrow(cell.data), nrow(cell.data)*train.prop)

cell.train = cell.data[train.cases, -1]
cell.test = cell.data[-train.cases, -1]



####CART
library(rpart)
set.seed(100)
CART_fit.cell <- rpart(Churn ~.,data=cell.train, method="class")
printcp(CART_fit.cell) 
pred.cart.cell<-predict(CART_fit.cell, newdata=cell.test,type="class")
CrossTvCart=table(cell.test$Churn, pred.cart.cell)
CrossTvCart[2,2]/(dim(cell.test)[1]-CrossTvCart[1,1])

###RANDOM FOREST
library(randomForest);
set.seed(100)

RF_fit.cell <- randomForest(Churn ~ .,data=cell.train, ntree=500)
cell.predict <-predict(RF_fit.cell, cell.test, predict.all=TRUE)
cell.rf.class<-cell.predict$aggregate
table(cell.test$Churn, cell.rf.class)
CrossTvRF=table(cell.test$Churn, cell.rf.class)
CrossTvRF[2,2]/(dim(cell.test)[1]-CrossTvRF[1,1])


###RANDOM FOREST 1000 Trees
library(randomForest);
set.seed(100)

RF_fit.cell <- randomForest(Churn ~ .,data=cell.train, ntree=1000)
cell.predict <-predict(RF_fit.cell, cell.test, predict.all=TRUE)
cell.rf.class<-cell.predict$aggregate
table(cell.test$Churn, cell.rf.class)
CrossTvRF=table(cell.test$Churn, cell.rf.class)
CrossTvRF[2,2]/(dim(cell.test)[1]-CrossTvRF[1,1])

