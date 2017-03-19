#----------------------------------------
# Import libraries
#----------------------------------------
library(corrplot)
library(tree)
library(randomForest)
library(leaps)
library(caret)
library(e1071)
#----------------------------------------
# Function
#----------------------------------------
classErr <- function(predicted.Y, test.Y){
  #Compare predicted label and true label
  compare <- cbind(predicted.Y, test.Y, (predicted.Y==test.Y))
  percent <- sum(compare[,3])/nrow(compare)*100
  
  print(table(predicted.Y, test.Y))
  cat('The percentage of correct predictions :', percent, '\n')
}

#----------------------------------------
# Data Exploration
#----------------------------------------
red <- read.csv("~/Desktop/Career/AXA/winequality-red.csv", sep=";")
str(red)
summary(red)

red$quality <- as.integer(red$quality)

#Ratio of quality
histogram(red$quality)

#Correlation between attributes - manual feature selection
par(mfrow=c(1,1))
corrplot(cor(red), method="number")

#Check outliers
names <- names(red)
par(mfrow=c(3,4))
for(i in 1:11){
  name <- names[i]
  boxplot(red[,i], xlab=name,col='cyan')
}

#Add new attributes for classification
myred <- red
myred$quality.class <- ifelse(myred$quality<=4, "bad", ifelse(myred$quality>=7,"good","medium"))
myred$quality.class <- as.factor(myred$quality.class)

#Subsets
set.seed(1)
trains <- sample(nrow(red), size =(nrow(red)/2))

train.set <- red[trains,]
test.set <- red[-trains,]

#----------------------------------------
# Automatic Feature selection
#----------------------------------------
subset <- regsubsets(quality~., train.set[,1:12], nvmax=11)
subset.summary <- summary(subset)

par(mfrow=c(2,2))
plot(subset.summary$rss,xlab="Number of attributes",ylab="RSS",type ="l")
plot(subset.summary$adjr2,xlab="Number of attributes",ylab ="Adjusted RSq",type="l")
plot(subset.summary$cp,xlab="Number of attributes",ylab ="cp",type="l")
plot(subset.summary$bic,xlab="Number of attributes",ylab ="bic",type="l")
which.min(subset.summary$bic)
which.max(subset.summary$adjr2)

coef(subset,7)

#----------------------------------------
# Random-forest
#----------------------------------------
#K-Folds cross validation
k = 5 

# sample from 1 to k, nrow times (the number of observations in the data)
id <- sample(1:k, nrow(red), replace = TRUE)
error <- 0

rf.model <- randomForest(quality~., data = train.set)
rf.model
rf.test.yhat <- predict(rf.model, newdata =test.set[,1:11])
k.mse = (sum((rf.test.yhat-test.set[,12])^2)/nrow(test.set))
error <- cbind(error, k.mse)

for(i in 1:5){
  cv.test = myred[(id==i),];
  cv.train = myred[(id!=i),];
  
  rf.model <- randomForest(quality~ volatile.acidity+chlorides+free.sulfur.dioxide
                           +total.sulfur.dioxide+pH+sulphates+alcohol, data = cv.train)
  
  #training set model
  rf.model
  
  #apply to test set
  rf.test.yhat <- predict(rf.model, newdata =cv.test)
  
  #test set prediction rate
  k.mse = (sum((rf.test.yhat-cv.test[,12])^2)/nrow(cv.test))
  error <- cbind(error, k.mse)
}

#K-fold Cross-validation error rate
sum(error)/k

#----------------------------------------
# Decision Tree
#----------------------------------------
#Build a tree model
class.tree.wine <- tree(quality.class~.-quality, myred, subset=trains)
reg.tree.wine <- tree(quality~., red, subset=trains)

summary(class.tree.wine)
par(mfrow=c(1,1))
plot(class.tree.wine)
text(class.tree.wine,pretty=0)

summary(reg.tree.wine)
par(mfrow=c(1,1))
plot(reg.tree.wine)
text(reg.tree.wine,pretty=0)

#Predict test set
class.tree.yhat <- predict(class.tree.wine, test.set, type="class")
test.Y <- myred[-trains,'quality.class']
classErr(class.tree.yhat, test.Y)

reg.tree.yhat <- predict(reg.tree.wine, test.set)
test.Y <- myred[-trains,'quality']
sum((reg.tree.yhat-test.Y)^2)/nrow(test.set)

#Tree pruning
#prune tree with a certain number of final nodes
prune.wine <- prune.tree(tree.wine, best=5)
plot(prune.wine)
text(prune.wine, pretty=0)

prune.test.yhat <- predict(prune.wine, newdata = myred[-trains,], type="class")
classErr(prune.test.yhat, test.Y)

