# K Nearest Neighbors 

# Functions ####################################################
#start findKnn function--------------------------------
findKnn <- function(dist, train.Y, k){
 
  #sorting
  n <- nrow(dist)
  #dist[,1] is distance
  #dist[,2] is index
  for(i in 1:n){
    min.index <- which.min(dist[,1][i:n])
    t <- dist[i,] #temp value
    dist[i,] <- dist[min.index+(i-1),] #swap
    dist[min.index+(i-1),] <- t #swap
  }
  
  #finding knn index
  knn.index <- c()
  for(i in 1:k){
    knn.index <- c(knn.index, dist[,2][i])
  }
  
  knn <- train.Y[knn.index]
  
  #voting to predict a label
  neg.count <-0
  pos.count <-0
  
  for(i in 1:k){
    if(knn[i]==-1){
      neg.count <- neg.count+1
    }else{
      pos.count <- pos.count+1
    }
  }
  
  if(neg.count > pos.count){
    return(-1)
  }else{
    return(1)
  }
}
#end findKnn function--------------------------------

#start predictKnn function--------------------------------
predictKnn <- function(train.X, test.X, train.Y, k){
    if(k/2==0){
      print("Recommended K is odd number.")
    }
    n.test <- nrow(test.X)
    n.train <- nrow(train.X)
    distance <- matrix(0, nrow=n.train, ncol=2)
    predicted.Y <- matrix(0, nrow=n.test, ncol=1)
    
    for(i in 1:n.test){
      #Test object 'x'
      x <- test.X[i,]
      for(j in 1:n.train){
        #Calculate euclidean distance
        distance[j,1]<- sqrt(sum((train.X[j,]-x)^2)) #Euclidean distance
        distance[j,2]<- j #index
      }
      
      #findKNN function call
      predicted.Y[i,]<- findKnn(distance, train.Y, k)
    }
    
    return (predicted.Y)
}
#end predictKnn function--------------------------------

#start compareRst function--------------------------------
compareRst <- function(predicted.Y, test.Y){
  #Compare predicted label and true label
  compare <- cbind(predicted.Y, test.Y, (predicted.Y==test.Y))
  colnames(compare) <- c("yhat", "y", "same")
  percent <- sum(compare[,3])/nrow(compare)*100
  
  print(compare)
  cat('The percentage of correct predictions :', percent, '\n')
}
#end compareRst function--------------------------------

