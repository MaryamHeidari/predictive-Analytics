#read data set
adult <- read.csv(file.choose())
str(adult)

#Check Class bias
table(adult$ABOVE50K)

#Create Training and Test Samples
adult_ones <- adult[which(adult$ABOVE50K == 1), ] 
adult_zeros <- adult[which(adult$ABOVE50K == 0), ]

#Training
set.seed(100) 
adult_ones_training_rows <- sample(1:nrow(adult_ones), 0.7*nrow(adult_ones))  
adult_zeros_training_rows <- sample(1:nrow(adult_zeros), 0.7*nrow(adult_ones))
training_ones <- adult_ones[adult_ones_training_rows, ]  
training_zeros <- adult_zeros[adult_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  

# Testing
test_ones <- adult_ones[-adult_ones_training_rows, ]
test_zeros <- adult_zeros[-adult_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros) 

#Compute Information Values
install.packages("smbinning")
library(smbinning)
# segregate continuous and factor variables
factor_vars <- c ("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")
continuous_vars <- c("AGE", "FNWGT","EDUCATIONNUM", "HOURSPERWEEK", "CAPITALGAIN", "CAPITALLOSS")
iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(14))
# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="ABOVE50K", x=factor_var) 
  if(class(smb) != "character"){ #
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}


# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="ABOVE50K", x=continuous_var, p = 0.05)
  if(class(smb) != "character"){
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]
iv_df

#Build Logit Models and Predict
logitMod <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=trainingData, family=binomial(link="logit"))
predicted <- plogis(predict(logitMod, testData))

library(InformationValue)
optCutOff <- optimalCutoff(testData$ABOVE50K, predicted)[1] 
optCutOff

#Model Diagnostics
summary(logitMod)

install.packages("car")
library(car)
vif(logitMod)

misClassError(testData$ABOVE50K, predicted, threshold = optCutOff)

library(ggplot2)
plotROC(testData$ABOVE50K, predicted)

Concordance(testData$ABOVE50K, predicted)

sensitivity(testData$ABOVE50K, predicted, threshold = optCutOff)
specificity(testData$ABOVE50K, predicted, threshold = optCutOff)

confusionMatrix(testData$ABOVE50K, predicted, threshold = optCutOff)
