
#------------------------------------------------------------------------------------
#iris
iris <- read.csv("~/Desktop/RHUL Term1/ML_Assignment2/iris.txt", header=FALSE)

set.seed(1)
trains <- sample(nrow(iris), size =70)
train.X=iris[trains,1:4]
train.Y=iris[trains,5]
test.X=iris[-trains,1:4]
test.Y=iris[-trains,5] 

yhat1<- p1.lda(train.X, train.Y, test.X)
sum(ifelse(yhat1!=test.Y,1,0))/nrow(test.X) #Classification Error Rate

yhat2<- p2.lda(train.X, train.Y, test.X, 2)
sum(ifelse(yhat2!=test.Y,1,0))/nrow(test.X) #Classification Error Rate


#------------------------------------------------------------------------------------
#parkinsons
parkinsons <- read.csv("~/Desktop/RHUL Term1/ML_Assignment2/parkinsons.data", header=TRUE)

#Define Training and Test Set
set.seed(2)
trains <- sample(nrow(parkinsons), size =120)
train.X=parkinsons[trains,-c(1,18)] 
train.Y=parkinsons[trains,18]

test.X=parkinsons[-trains,-c(1,18)]
test.Y=parkinsons[-trains,18]

yhat1<- p1.lda(train.X, train.Y, test.X)
sum(ifelse(yhat1!=test.Y,1,0))/nrow(test.X) #Classification Error Rate

yhat2<- p2.lda(train.X, train.Y, test.X, 2)
sum(ifelse(yhat2!=test.Y,1,0))/nrow(test.X) #Classification Error Rate
