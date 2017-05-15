
#Ex. 8 -------------------------------------------------------------
p <- 4; #Number of attribute of object X[i]
M <- 20; #Number of Hidden Neuron as a parameter 
ep <- 100; #Epoch
learningRate <- 0.1; #Learning Rate

sigm <- function(x){
  return(1 / (1+exp(-x)))
}
sigmT <- function(x) {
  return(exp(x) / ((exp(x) + 1)^2))
}

data <- scale(Auto[c("horsepower","weight","year","origin")])
var(data) #Normalization Check

set.seed(0102)
train <- sample(nrow(data), nrow(data)/2)
train.X <- data[train,]
test.X <- data[-train,]
train.Y <- Auto[train,"mpg"]
test.Y <- Auto[-train,"mpg"]

test.MSE <- rep(0,100)

for(x in 1:100){ # 100 times 
  
  randNumA <-runif(M*p, min=-0.7, max=0.7) 
  randNumB <-runif(M, min=-0.7, max=0.7)
  
  aWeight <- matrix(c(randNumA),M,p,byrow=TRUE) #a(m,l) m x l matrix
  bWeight <- matrix(c(randNumB),M,1,byrow=TRUE)
  
  n <- nrow(train.X)
  pred.Y <- rep(0,n)
  
  for(e in 1:ep){
    for(i in 1:n){
      X <- matrix(train.X[i,])
      Y <- train.Y[i]
      
      Z <- sigm(aWeight%*%X)
      pred.Y[i] <- t(bWeight)%*%Z #Learning Machine
      error <- 2*(Y-pred.Y[i])
      S <- error*bWeight*sigmT((aWeight%*%X))
      
      bWeight <- bWeight + (learningRate*error[1]*Z)/n
      aWeight <- aWeight + (learningRate*S%*%t(X))/n
      
    }
  }
  train.MSE = sum((pred.Y-train.Y)^2)/n
  
  # Apply this model to test set 
  yhat <- rep(0,nrow(test.X))
  
  for(i in 1:nrow(test.X)){
    zhat <- sigm(aWeight%*%test.X[i,])
    yhat[i] <- t(bWeight)%*%zhat
  }
  
  test.MSE[x] <- sum((yhat-test.Y)^2)/n
}

boxplot(test.MSE, xlab='MSE',col='cyan',
        border='blue',names='Test MSE for NN',
        main='Test MSE for NN',horizontal=TRUE)

#--------------------------------------------------------------------------
