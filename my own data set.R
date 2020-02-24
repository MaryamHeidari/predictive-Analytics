#read file
dt <- read.csv(file.choose())
str(dt)

#missing value
sum(is.na(dt))

#view target
table(dt$target)

# rename it and round  it
dt$target <- factor(dt$target, levels = c(0, 1), 
                    labels = c("no heart disease", "heart disease"))
round(prop.table(table(dt$target)) * 100, digits = 1)

# normalize these features
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dt_n <- as.data.frame(lapply(dt[1:13], normalize))

#split data to tarin and test
train_index <- sample(1:nrow(dt_n), 0.8 * nrow(dt_n))
test_index <- setdiff(1:nrow(dt_n), train_index)
train <- dt_n[train_index,]
test <- dt_n[test_index,]
dt_train_labels <- dt[train_index,14]
dt_test_labels <- dt[test_index,14]

#making the model
install.packages("class")
library(class)


dt_test_pred <- knn(train = train, test = test,
                      cl = dt_train_labels, k = 17)
#visulize the model
library(gmodels)
CrossTable(x = dt_test_labels, y = dt_test_pred, prop.chisq=FALSE)

#improving model performance
#z-score standardization
dt_z <- as.data.frame(scale(dt[-14]))

# split the data to train and test
train_index <- sample(1:nrow(dt_z), 0.8 * nrow(dt_z))
test_index <- setdiff(1:nrow(dt_z), train_index)
train <- dt_z[train_index,]
test <- dt_z[test_index,]
dt_train_labels <- dt[train_index,14]
dt_test_labels <- dt[test_index,14]

# make a model and visulize it
dt_test_pred <- knn(train = train, test = test,
                    cl = dt_train_labels, k = 21)
CrossTable(x = dt_test_labels, y = dt_test_pred, prop.chisq=FALSE)



