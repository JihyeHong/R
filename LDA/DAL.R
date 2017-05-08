p1.lda <- function(train.X,train.Y,test.X){
  class <- unique(train.Y)
  K <- 2
  N <- nrow(train.X)
  attr <- rep(0,ncol(train.X))
  for (i in 1:ncol(train.X)){
    mean.0 <- mean(train.X[(train.Y==class[1]),i])
    mean.1 <- mean(train.X[(train.Y==class[2]),i])
    st.dev <- sd(train.X[,i])
    attr[i] <- (mean.1-mean.0)/st.dev
  }
  p <- order(abs(attr), decreasing=TRUE)[1]
  
  NK1 <- nrow(train.X[(train.Y==class[1]),])
  NK2 <- nrow(train.X[(train.Y==class[2]),])
  
  prior1 <- NK1/N
  prior2 <- NK2/N
  
  mean1 <- mean(train.X[(train.Y==class[1]),p])
  mean2 <- mean(train.X[(train.Y==class[2]),p])
  
  cov <- (sum((train.X[(train.Y==class[1]),p]-mean1)^2)+sum((train.X[(train.Y==class[2]),p]-mean2)^2))/(N-K)

  delta1 <- (mean1/cov)*test.X[,p] - mean1^2/(2*cov) + log(prior1)
  delta2 <- (mean2/cov)*test.X[,p] - mean2^2/(2*cov) + log(prior2)

  pred.label <- ifelse(delta1>delta2,class[1],class[2])
  return(pred.label)
}

#------------------------------------------------------------------------------
#p=2
p2.lda <- function(train.X,train.Y,test.X,pp){
  p <- pp
  class <- unique(train.Y)
  K <- 2
  N <- nrow(train.X)
  
  attr <- rep(0,ncol(train.X))
  for (i in 1:ncol(train.X)){
    mean.0 <- mean(train.X[(train.Y==class[1]),i])
    mean.1 <- mean(train.X[(train.Y==class[2]),i])
    st.dev <- sd(train.X[,i])
    attr[i] <- (mean.1-mean.0)/st.dev
  }
  
  attr.p <- rep(0,pp)
  for(b in 1:pp){
    attr.p[b] <- order(abs(attr), decreasing=TRUE)[b]
  }
  
  NK1 <- nrow(train.X[(train.Y==class[1]),])
  NK2 <- nrow(train.X[(train.Y==class[2]),])
  
  prior1 <- NK1/N
  prior2 <- NK2/N
  
  mean1 <- colMeans(train.X[(train.Y==class[1]),attr.p])
  mean2 <- colMeans(train.X[(train.Y==class[2]),attr.p])
  meanVec <- rbind(mean1,mean2)
  
  covMat <- matrix(0,p,p)
  for(l in 1:p){
    for(j in 1:p){
      innerSigma1 <- sum((train.X[(train.Y==class[1]),l] - meanVec[1,l])*(train.X[(train.Y==class[1]),j]-meanVec[1,j]))
      innerSigma2 <- sum((train.X[(train.Y==class[2]),l] - meanVec[2,l])*(train.X[(train.Y==class[2]),j]-meanVec[2,j]))
      covMat[l,j] <- sum(innerSigma1+innerSigma2)/(N-K)
    }
  }
  
  meanMat1 <- as.matrix(mean1)
  meanMat2 <- as.matrix(mean2)
  xTrans <- as.matrix(test.X[,attr.p])
  
  delta1 <- (xTrans%*%solve(covMat)%*%meanMat1)-((1/2)*t(meanMat1)%*%solve(covMat)%*%meanMat1)[1,1]+log(prior1)
  delta2 <- (xTrans%*%solve(covMat)%*%meanMat2)-((1/2)*t(meanMat2)%*%solve(covMat)%*%meanMat2)[1,1]+log(prior2)
  
  pred.label <- ifelse(delta1>delta2,class[1],class[2])
  return(pred.label)
}
