#read the file
mushrooms <- read.csv(file.choose(), stringsAsFactors = TRUE)

#explor the data set
str(mushrooms)
mushrooms$veil.type <- NULL
table(mushrooms$class)

#creating random training and test datasets
install.packages("OneR")
library(OneR)
mushroom_1R <- OneR(class ~ ., data = mushrooms)
mushroom_1R
#evaluating model performance
summary(mushroom_1R)

#improving model performance
install.packages("rJava", type = "source")
library(rJava)
install.packages("RWeka",, type = "source")
library(RWeka)
mushroom_JRip <- JRip(class ~ ., data = mushrooms)
mushroom_JRip

