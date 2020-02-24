#read file
wbcd <- read.csv( file.choose()) 
str(wbcd)

# view diagnosis 
table(wbcd$M)
  
# rename it and round  it
wbcd$M <- factor(wbcd$M, levels = c("B", "M"), labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$M)) * 100, digits = 1)

# normalize these features
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
wbcd_n <- as.data.frame(lapply(wbcd[3:32], normalize))

#split data to tarin and test
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:568, ]
wbcd_train_labels <- wbcd[1:469, 2] 
wbcd_test_labels <- wbcd[470:568, 2]

#making the model
install.packages("class")
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)
#visulize the model
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

#improving model performance
#z-score standardization
wbcd_z <- as.data.frame(scale(wbcd[-2]))

# split the data to train and test
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:568, ]
wbcd_train_labels <- wbcd[1:469, 2]
wbcd_test_labels <- wbcd[470:568, 2]

# make a model and visulize it
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

