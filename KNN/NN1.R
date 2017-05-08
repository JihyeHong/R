# Implementation on iris dataset ####################################################
# 1. load the dataset
iris <- read.csv("~/Desktop/UK/RHUL Term1/R/iris.txt", header=FALSE)

# 2. split the dataset into the training and test sets.
iris.train.X <- iris[1:70,-5]
iris.test.X <- iris[71:100,-5]

iris.train.Y <- iris[1:70,5]
iris.test.Y <- iris[71:100,5]

# 3. Predict KNN of iris
#k=1
iris.predicted.Y <- predictKnn(iris.train.X, iris.test.X, iris.train.Y, k=1)
compareRst(iris.predicted.Y, iris.test.Y)

#k=3
iris.predicted.Y <- predictKnn(iris.train.X,iris.test.X,iris.train.Y,k=3)
compareRst(iris.predicted.Y,iris.test.Y)

