
# Ex 7. Linear Regression ---------------------------------------------
lm.fit <- lm(mpg~horsepower+weight+year+origin, data=Auto[train,])
lm.train.yhat <- predict(lm.fit, Auto[train,])
lm.train.MSE <- sum((lm.train.yhat-train.Y)^2)/n

lm.test.yhat <- predict(lm.fit, Auto[-train,])
lm.test.MSE <- sum((lm.test.yhat-test.Y)^2)/n
#----------------------------------------------------------------------