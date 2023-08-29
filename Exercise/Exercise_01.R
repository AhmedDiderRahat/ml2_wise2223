# check the package
find.package("glmnet")

# install it
install.packages("glmnet", dep=T)


require(glmnet)
# Load package
library("glmnet")


# check the package
find.package("ISLR2")

# install it
install.packages("ISLR2", dep=T)

# Load package
library("ISLR2")


library(MASS)

?mcycle

summary(mcycle)

names(mcycle)

mcycle

library(ggplot2)
# Basic scatter plot
ggplot(mcycle, aes(x=times, y=accel)) + geom_point()


# ----------------------------------- x ----------------------------------- 

df <- mcycle
#randomly shuffle data
df.shuffled <- df[sample(nrow(df)),]

#define number of folds to use for k-fold cross-validation
K <- 10 

#define degree of polynomials to fit
degree <- 4

#create k equal-sized folds
folds <- cut(seq(1, nrow(df.shuffled)), breaks = K, labels = FALSE)

#create object to hold MSE's of models
mse = matrix(data = NA, nrow = K, ncol = degree)

#Perform K-fold cross validation
for(i in 1:K){
  
  #define training and testing data
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df.shuffled[testIndexes, ]
  trainData <- df.shuffled[-testIndexes, ]
  
  #use k-fold cv to evaluate models
  for (j in 1:degree){
    fit.train = lm(accel ~ poly(times,j), data=trainData)
    fit.test = predict(fit.train, newdata=testData)
    mse[i,j] = mean((fit.test-testData$accel)^2) 
  }
}

#find MSE for each degree 
colMeans(mse)

