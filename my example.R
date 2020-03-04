#read data set
amazonlabell <- read.csv(file.choose())
str(amazonlabell)

# cleaning the data set and make my own one
amazonlabell<-amazonlabell[!(amazonlabell$X0 !=1 & amazonlabell$X0 !=0),]
amazonlabell <- amazonlabell[, c(1,2)]
names(amazonlabell) <- c("text","type")
amazonlabell$type <- factor(amazonlabell$type, levels = c(0, 1), 
                            labels = c("great", "bad"))
amazonlabell$type <- factor(amazonlabell$type)
str(amazonlabell)
table(amazonlabell$type)


#cleaning and standardizing text data
install.packages("tm")
library(tm)
amazon_corpus <- VCorpus(VectorSource(amazonlabell$text))
print(amazon_corpus)
#all lower case
amazon_corpus_clean <- tm_map(amazon_corpus, content_transformer(tolower))
#remove numbers
amazon_corpus_clean <- tm_map(amazon_corpus_clean, removeNumbers)
# remove filler words such as to, and, but, and or
amazon_corpus_clean <- tm_map(amazon_corpus_clean, removeWords, stopwords())
#eliminate any punctuation 
amazon_corpus_clean <- tm_map(amazon_corpus_clean, removePunctuation)
#stemming
install.packages("SnowballC")
library(SnowballC)
amazon_corpus_clean <- tm_map(amazon_corpus_clean, stemDocument)
amazon_corpus_clean <- tm_map(amazon_corpus_clean, stripWhitespace)

#Creating a DTM sparse matrix and split the data set
amazon_dtm <- DocumentTermMatrix(amazon_corpus_clean)
amazon_dtm
View(amazon_dtm)

#creating training and test datasets
amazon_dtm_train <- amazon_dtm[1:577, ]
amazon_dtm_test <- amazon_dtm[577:770, ]
amazon_train_labels <- amazonlabell[1:577, ]$type
amazon_test_labels <- amazonlabell[577:770, ]$type

prop.table(table(amazon_train_labels))
prop.table(table(amazon_test_labels))


#creating indicator features for frequent words
#Finding frequent words
findFreqTerms(amazon_dtm_train, 5)
amazon_freq_words <- findFreqTerms(amazon_dtm_train, 5)
str(amazon_freq_words)

amazon_dtm_freq_train<- amazon_dtm_train[ , amazon_freq_words] 
amazon_dtm_freq_test <- amazon_dtm_test[ , amazon_freq_words]

# Naive Bayes classifier
#convert counts to Yes/No strings
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

amazon_train <- apply(amazon_dtm_freq_train, MARGIN = 2, convert_counts)
amazon_test <- apply(amazon_dtm_freq_test, MARGIN = 2, convert_counts)

#training a model
install.packages("e1071")
library(e1071)
amazon_classifier <- naiveBayes(amazon_train, amazon_train_labels)

#evaluating model performance
amazon_test_pred <- predict(amazon_classifier, amazon_test)
library(gmodels)
CrossTable(amazon_test_pred, amazon_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))


#improving the model
amazon_classifier2 <- naiveBayes(amazon_train, amazon_train_labels,
                              laplace = 1)
amazon_test_pred2 <- predict(amazon_classifier2, amazon_test)
CrossTable(amazon_test_pred2, amazon_test_labels, prop.chisq = FALSE, prop.t = FALSE, 
           prop.r = FALSE, dnn = c('predicted', 'actual'))


