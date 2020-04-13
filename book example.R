#read the file
credit <- read.csv(file.choose())

#explor the data set
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
table(credit$default)

#creating random training and test datasets
set.seed(123)
train_sample <- sample(1000, 900)
str(train_sample)
credit_train <- credit[train_sample, ] 
credit_test <- credit[-train_sample, ]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
credit_train$default<-as.factor(credit_train$default)

#training a model on the data
install.packages("C50")
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model
summary(credit_model)

#evaluating model performance
credit_pred <- predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual default', 'predicted default'))

#improving model performance
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
          prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
          dnn = c('actual default', 'predicted default'))
