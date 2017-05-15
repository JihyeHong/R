#Neural network for regression
p <- 4; #Number of attribute of object X[i]
M <- 5; #Number of Hidden Neuron as a parameter!! 
ep <- 1000;
learningRate <- 0.1;

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

set.seed(1);
randNumA <-runif(M*p, min=-0.7, max=0.7) 
randNumB <-runif(M, min=-0.7, max=0.7)

aWeight <- matrix(c(randNumA),M,p,byrow=TRUE) #a(m,l) m x l matrix
bWeight <- matrix(c(randNumB),M,1,byrow=TRUE)

n <- nrow(train.X)
pred.Y <- rep(0,n)

a0 <- rep(1,p)
b0 <- 1

for(e in 1:ep){
  for(i in 1:n){
    X <- matrix(train.X[i,])
    Y <- train.Y[i]
    
    Z <- sigm(a0[1] + aWeight%*%X)
    pred.Y[i] <- b0 + t(bWeight)%*%Z #Learning Machine
    error <- 2*(Y-pred.Y[i])
    
    S <- error*bWeight*sigmT(a0[1]+(aWeight%*%X))
    s0 <- error*b0*sigmT(a0[1]+(a0%*%X))
    
    bWeight <- bWeight + (learningRate*error[1]*Z)/n
    b0 <- b0 + (learningRate*error[1]*1)/n 
    
    aWeight <- aWeight + (learningRate*S%*%t(X))/n
    a0 <- a0 + (learningRate*s0)*1/n 
  
  }
}
train.MSE = sum((pred.Y-train.Y)^2)/n

# Apply this model to test set ---------------------------#
yhat <- rep(0,nrow(test.X))

for(i in 1:nrow(test.X)){
  zhat <- sigm(a0[1] + aWeight%*%test.X[i,])
  yhat[i] <- b0 + t(bWeight)%*%zhat
}

test.MSE = sum((yhat-test.Y)^2)/n

print(train.MSE)
print(test.MSE)

# Linear Regression --------------------------------------------------
lm.fit <- lm(mpg~horsepower+weight+year+origin, data=Auto[train,])
lm.train.yhat <- predict(lm.fit, Auto[train,])
lm.train.MSE <- sum((lm.train.yhat-train.Y)^2)/n

lm.test.yhat <- predict(lm.fit, Auto[-train,])
lm.test.MSE <- sum((lm.test.yhat-test.Y)^2)/n
#----------------------------------------------------------------------
