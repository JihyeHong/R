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
white <- read.csv("~/Desktop/Career/AXA/winequality-white.csv", sep=";")
str(white)
summary(white)

white$quality <- as.integer(white$quality)

#Ratio of quality
histogram(white$quality)

#Correlation between attributes - manual feature selection
par(mfrow=c(1,1))
corrplot(cor(white), method="number")

#Check outliers
names <- names(white)
par(mfrow=c(3,4))
for(i in 1:11){
  name <- names[i]
  boxplot(white[,i], xlab=name,col='cyan')
}

#Add new attributes for classification
mywhite <- white
mywhite$quality.class <- ifelse(mywhite$quality<=4, "bad", ifelse(mywhite$quality>=7,"good","medium"))
mywhite$quality.class <- as.factor(mywhite$quality.class)

#Subsets
set.seed(1)
trains <- sample(nrow(white), size =(nrow(white)/2))

train.set <- white[trains,]
test.set <- white[-trains,]

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

coef(subset,8)

#----------------------------------------
# Random-forest
#----------------------------------------
#K-Folds cross validation
k = 5 

# sample from 1 to k, nrow times (the number of observations in the data)
id <- sample(1:k, nrow(white), replace = TRUE)
error <- 0

for(i in 1:5){
  cv.test = mywhite[(id==i),];
  cv.train = mywhite[(id!=i),];
  
  rf.model <- randomForest(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide
                           +density+pH+sulphates+alcohol, data = cv.train)
  
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
class.tree.wine <- tree(quality.class~.-quality, mywhite, subset=trains)
reg.tree.wine <- tree(quality~., white, subset=trains)

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
test.Y <- mywhite[-trains,'quality.class']
classErr(class.tree.yhat, test.Y)

reg.tree.yhat <- predict(reg.tree.wine, test.set)
test.Y <- mywhite[-trains,'quality']
sum((reg.tree.yhat-test.Y)^2)/nrow(test.set)

#Tree pruning
#prune tree with a certain number of final nodes
prune.wine <- prune.tree(tree.wine, best=5)
plot(prune.wine)
text(prune.wine, pretty=0)

prune.test.yhat <- predict(prune.wine, newdata = mywhite[-trains,], type="class")
classErr(prune.test.yhat, test.Y)

