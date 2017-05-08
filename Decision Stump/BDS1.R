install.packages('MASS')
library(MASS)

#label: medv(median house value for each neighbourhood)
#attr1: lstat(percent of households with low socioeconomic status)
#attr2: rm(average number of rooms per house)

data(Boston)
fix(Boston)
names(Boston)
attach(Boston)

#split into training and test sets.
set.seed(0102)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
train.X <- Boston[train,c("lstat","rm")]
train.Y <- Boston[train,c("medv")]
test.X <- Boston[-train,c("lstat","rm")]
test.Y <- Boston[-train,c("medv")]

s.lstat = 1.8:27.9
s.rm = 3.6:8.6

#program DS(decision stumps)
#1. Train your DS implementation on the training set. Find the MSE on the test set. Include it in your report.
train.rss.ls <- rep(0,length(s.lstat))
for(i in 1:length(s.lstat)){
  y.hat.small = mean(train.Y[train.X["lstat"]< s.lstat[i]])
  y.hat.big = mean(train.Y[train.X["lstat"]>= s.lstat[i]])
  
  yi.small = train.Y[train.X["lstat"]< s.lstat[i]]
  yi.big = train.Y[train.X["lstat"]>= s.lstat[i]]
  
  train.rss.ls[i] = sum((yi.small - y.hat.small)^2) + sum((yi.big- y.hat.big)^2)
}

train.rss.rm <- rep(0,length(s.rm))
for(i in 1:length(s.rm)){
  y.hat.small = mean(train.Y[train.X["rm"]< s.rm[i]])
  y.hat.big = mean(train.Y[train.X["rm"]>= s.rm[i]])
  
  yi.small = train.Y[train.X["rm"]< s.rm[i]]
  yi.big = train.Y[train.X["rm"]>= s.rm[i]]
  
  train.rss.rm[i] = sum((yi.small - y.hat.small)^2) + sum((yi.big- y.hat.big)^2)
}

best.treshold <- 0
attrnm <- ""
if(min(train.rss.ls)<min(train.rss.rm)){
  best.treshold <- s.lstat[which.min(train.rss.ls)]
  attrnm <- "lstat"
  
}else{
  best.treshold <- s.rm[which.min(train.rss.rm)]
  attrnm <- "rm"
}

pred.y.small <- mean(train.Y[train.X[attrnm]< best.treshold])
pred.y.big <- mean(train.Y[train.X[attrnm]>= best.treshold])

y.hat <- test.X[attrnm]<best.treshold
y.hat <- ifelse(y.hat, pred.y.small, pred.y.big)

sum(test.Y - y.hat)^2 / length(test.Y) #test RSS
