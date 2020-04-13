install.packages("randomForest")
library(randomForest)
install.packages("MASS")
library(MASS)

data <- read.csv(file.choose())

set.seed(17)
data.rf <- randomForest(target ~., data=data,
                       mtry=2, importance= TRUE,
                       do.trace= 100)
print(data.rf)


#model comparison
install.packages("ipred")
library(ipred)
set.seed(131)
error.RF <- numeric(10)
for (i in 1:10) {
  error.RF[i] <- errorest(type ~. , data = fgl, model= randomForest, mtry=2)$error
}
summary(error.RF)

library(e1071)
set.seed(563)
error.SVM <- numeric(10)
for (i in 1:10) {
  error.SVM[i] <- errorest(type ~., data = fgl, model= svm, cost=10, gamma=1.5)$error
}
summary(error.SVM)

# figure of random forest
par(mfrow= c(2,2))
for (i in 1:2) {
  plot(sort(data.rf$importance[,i], dec= TRUE), type = "h", main = paste("Measure",i))
}
