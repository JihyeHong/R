
# 3. Plot the test MSE for a fixed value of learning rate as a function of B(the number of trees).
x = seq(800,1700,100)
y = rep(0,10)
for(i in 1:10){
  y[i] = calculateBDS(0.01,x[i])
}

plot(x,y,xlab="B",ylab="test MSE")