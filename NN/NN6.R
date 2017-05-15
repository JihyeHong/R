
# Ex6. K-fold cross-validation --------------------------------------------
ep<-100
learningRate <-0.001
K.MSE <- matrix(0,10,4)
for(k in 0:3){
  for(m in 15:24){ #Find the best M
      num <- 49*k+(1:49)
      fold.test.X <- train.X[num,]
      fold.test.Y <- train.Y[num]
      
      fold.train.X <- train.X[-num,]
      fold.train.Y <- train.Y[-num]
      
      aWeight <- matrix(c(randNumA),m,p,byrow=TRUE)
      bWeight <- matrix(c(randNumB),m,1,byrow=TRUE)
      
      n <- 196-49
      pred.Y <- rep(0,n)
      
      #Training
      for(e in 1:ep){
        for(l in 1:n){
          X <- matrix(fold.train.X[l,])
          Y <- fold.train.Y[l]
          
          Z <- sigm(aWeight%*%X)
          pred.Y[i] <- t(bWeight)%*%Z #Learning Machine
          error <- 2*(Y-pred.Y[l])
          S <- error*bWeight*sigmT((aWeight%*%X))
          
          bWeight <- bWeight + (learningRate*error[1]*Z)/n
          aWeight <- aWeight + (learningRate*S%*%t(X))/n
        }
      }
      
      # Apply this model to test set 
      yhat <- rep(0,nrow(fold.test.X))
      
      for(i in 1:nrow(fold.test.X)){
        zhat <- sigm(aWeight%*%fold.test.X[i,])
        yhat[i] <- t(bWeight)%*%zhat
      }
      
      validation.MSE = sum((yhat-fold.test.Y)^2)/49
      K.MSE[m-14,k+1] <- validation.MSE
  }
}
K.MSE <- cbind(c(15:24),K.MSE)


# ---------------------------------------------------------------------
