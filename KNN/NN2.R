# Implementation on ionosphere dataset ##########################################
# 1. load the dataset
ionosphere <- read.csv("~/Desktop/UK/RHUL Term1/R/ionosphere.txt", header=FALSE)

# 2. split the dataset into the training and test sets.
iono.train.X <- ionosphere[1:200,-35]
iono.test.X <- ionosphere[201:nrow(ionosphere),-35]

iono.train.Y <- ionosphere[1:200,35]
iono.test.Y <- ionosphere[201:nrow(ionosphere),35]

# 3. Predict KNN of ionosphere
#k=1
iono.predicted.Y <- predictKnn(iono.train.X,iono.test.X,iono.train.Y,k=1)
compareRst(iono.predicted.Y,iono.test.Y)

#k=3
iono.predicted.Y <- predictKnn(iono.train.X,iono.test.X,iono.train.Y,k=3)
compareRst(iono.predicted.Y,iono.test.Y)