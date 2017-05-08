
#program BDS(boosted decision stumps)
#2. Train your BDS implementation on the training set for learning rate 0.01 and B = 1000 trees.
# Find the MSE on the test set. Include it in your report.

calculateBDS <- function (learningRate, B){
  set.seed(0102)
  train <- sample(1:nrow(Boston), nrow(Boston)/2)
  train.X <- Boston[train,c("lstat","rm")]
  train.Y <- Boston[train,c("medv")]
  test.X <- Boston[-train,c("lstat","rm")]
  test.Y <- Boston[-train,c("medv")]
  
  resid.Y <- train.Y
  y.hat <- 0
  
  s.lstat = 1.8:27.9
  s.rm = 3.6:8.6
  
  for(j in 1:B){
    train.rss.ls <- rep(0,length(s.lstat))
    for(i in 1:length(s.lstat)){
      y.hat.small = mean(resid.Y[train.X["lstat"]< s.lstat[i]])
      y.hat.big = mean(resid.Y[train.X["lstat"]>= s.lstat[i]])
      
      yi.small = resid.Y[train.X["lstat"]< s.lstat[i]]
      yi.big = resid.Y[train.X["lstat"]>= s.lstat[i]]
      
      train.rss.ls[i] = sum((yi.small - y.hat.small)^2) + sum((yi.big- y.hat.big)^2)
    }
    
    train.rss.rm <- rep(0,length(s.rm))
    for(i in 1:length(s.rm)){
      y.hat.small = mean(resid.Y[train.X["rm"]< s.rm[i]])
      y.hat.big = mean(resid.Y[train.X["rm"]>= s.rm[i]])
      
      yi.small = resid.Y[train.X["rm"]< s.rm[i]]
      yi.big = resid.Y[train.X["rm"]>= s.rm[i]]
      
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
    
    pred.y.small <- mean(resid.Y[train.X[attrnm]< best.treshold])
    pred.y.big <- mean(resid.Y[train.X[attrnm]>= best.treshold])
    
    y.hat.b <- test.X[attrnm]<best.treshold
    y.hat.b <- ifelse(y.hat.b, pred.y.small, pred.y.big)
    
    resid.Y <- resid.Y - learningRate*y.hat.b
    y.hat <- y.hat + learningRate*y.hat.b
  }
  
  return(sum(test.Y - y.hat)^2 / length(test.Y)) #test RSS
}

learningRate <- 0.01
B <- 1000
calculateBDS(learningRate, B)
