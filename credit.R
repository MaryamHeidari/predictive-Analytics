#read the file
credit <- read.csv(file.choose())

#explor the data set
str(credit)
credit$default <- factor(credit$default, levels = c(1, 2), labels = c("No", "Yes"))

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

## Random Forest
install.packages("randomForest")
library(randomForest)
install.packages("MASS")
library(MASS)
set.seed(17)
mtry <- sqrt(ncol(credit_train))
mtry
credit.rf <- randomForest(default ~., data=credit_train,
                        mtry=mtry, importance= TRUE,
                        do.trace= 100)
print(credit.rf)

library(gmodels)
rf <- randomForest(default ~ .,data=credit_train)
pr <- predict(rf,df)
CrossTable(pr, credit_test$default,
           prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))

## Naive Bayes
#training a model
install.packages("e1071")
library(e1071)
Naive_Bayes_Model=naiveBayes(default ~., data=credit_train)
Naive_Bayes_Model
#evaluating model performance
NB_Predictions=predict(Naive_Bayes_Model,credit_test)
library(gmodels)
CrossTable(NB_Predictions, credit_test$default,
           prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))


##linear regression
install.packages("smbinning")
library(smbinning)

factor_vars <- c ("checking_balance", "credit_history", "purpose", "savings_balance", 
                  "employment_length", "personal_status", "other_debtors", "property",
                  "installment_plan","housing","telephone","foreign_worker","job")
continuous_vars <- c("months_loan_duration", "amount","installment_rate", 
                     "residence_history", "age", "existing_credits","dependents")
iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(20))
# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(credit_train, y="default", x=factor_var) 
  if(class(smb) != "character"){ #
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}
# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(credit_train, y="default", x=continuous_var, p = 0.05)
  if(class(smb) != "character"){
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}
iv_df <- iv_df[order(-iv_df$IV), ]
iv_df
#Build Logit Models and Predict
logitMod <- glm(default ~ .,data = credit_train, family=binomial)
predicted <- plogis(predict(logitMod, credit_test))
library(InformationValue)
optCutOff <- optimalCutoff(credit_test$default, predicted)[1] 
optCutOff
summary(logitMod)

install.packages("car")
library(car)
vif(logitMod)
misClassError(credit_test$default, predicted, threshold = optCutOff)

library(ggplot2)
plotROC(credit_test$default, predicted)

Concordance(credit_test$default, predicted)

coef(summary(logitMod))

sensitivity(credit_test$default, predicted, threshold = optCutOff)
specificity(credit_test$default, predicted, threshold = optCutOff)

confusionMatrix(credit_test$default, predicted, threshold = optCutOff)

