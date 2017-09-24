#install.packages('tuneR', dependencies = T)
#install.packages('seewave', dependencies = T)
#install.packages('fftw', dependencies = T)
#install.packages('caTools', dependencies = T)
#install.packages('randomForest', dependencies = T)
#install.packages('gbm', dependencies = T)
#install.packages('caret', dependencies = T)
#install.packages('warbleR', dependencies = T)
#install.packages('mice', dependencies = T)
#install.packages('e1071', dependencies = T)
#install.packages('rpart', dependencies = T)
#install.packages('rpart-plot', dependencies = T)
#install.packages('xgboost', dependencies = T)

library(tuneR)
library(seewave)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(warbleR)
library(gbm)
library(caret)
library(mice)
library(xgboost)
library(e1071)

audioFolder <- function(folderName) {
  audiolist <- data.frame()
  list <- list.files(folderName, '\\.wav')  # Get list of files in the folder.
  
  for (fileName in list) {                  # Add file list to data.frame for processing.
    row <- data.frame(fileName, 0, 0, 20)
    audiolist <- rbind(audiolist, row)
  }
  
  names(audiolist) <- c('sound.files', 'selec', 'start', 'end')   # Set column names.
  setwd(folderName)   # Move into folder for processing.
  acoustics <- specan(audiolist, parallel=1)   # Call specan function from warbleR package to Process files.
  setwd('..')   # Move back into parent folder.
  
  acoustics
}

# Load data
males <- audioFolder('C:/UCONN MBA/Fall 2016/OPIM5503 Data Analytics using R/Project 2/Speech Analysis/male')
females <- audioFolder('C:/UCONN MBA/Fall 2016/OPIM5503 Data Analytics using R/Project 2/Speech Analysis/female')

# Set labels.
males$label <- 1
females$label <- 2
alldata <- rbind(males, females)
alldata$label <- factor(alldata$label, labels=c('male', 'female'))

# Remove unused columns.
alldata$duration <- NULL
alldata$sound.files <- NULL
alldata$selec <- NULL
alldata$peakf <- NULL
alldata$startdom <- NULL
alldata$enddom <- NULL
alldata$dfslope <- NULL

# Remove rows containing NA's.
alldata <- alldata[complete.cases(alldata),] 

# Create a train and test data set.
set.seed(777)
spl <- sample.split(alldata$label, 0.7)
train <- subset(alldata, spl == TRUE) # Get the training data set
test <- subset(alldata, spl == FALSE) # Get the test data set

# Build models.
genderLog1 <- glm(label ~ meandom, data=train, family='binomial')
genderLog <- glm(label ~ ., data=train, family='binomial') # This is the full logistic regresion model using all measured acoustic properties
genderCART <- rpart(label ~ ., data=train, method='class')  # This is the CART model
prp(genderCART)  #Plot the rpart tree
genderForest <- randomForest(label ~ ., data=train) #This is the randomForest model
genderBoosted <- train(label ~ ., data=train, method='gbm') #This is the Boosted Tree model
genderSvm <- svm(label ~ ., data=train, gamma=0.21, cost=8) # This is the SVM model

# Train the XGBoost Model
trainx <- sapply(train, as.numeric)
trainx[,21] <- trainx[,21] - 1
set.seed(777)
genderXG <- xgboost(data = trainx[,-21], label = trainx[,21], eta=0.2, nround = 500, subsample = 0.5, colsample_bytree = 0.5, objective = "binary:logistic")

# A basline model of always predicting male.
table(test$label)
15 / nrow(test)

#Logistic regression model against the "meandom" parameter
predictLog1 <- predict(genderLog1, newdata=test, type='response')
table(test$label, predictLog1 >= 0.5)
(4+10)/nrow(test)

# Full Logistical Regression Model
predictLog <- predict(genderLog, newdata=test, type='response')
table(test$label, predictLog >= 0.5)
(2+12) / nrow(test)

#CART Model
predictCART <- predict(genderCART, newdata=test)
predictCART2.prob <- predictCART2[,2]
table(test$label, predictCART2.prob >= 0.5)
(2 + 14) / nrow(test)

#Random Forest Model
predictForest <- predict(genderForest, newdata=test)
table(test$label, predictForest)
(12 + 14) / nrow(test)

#Boosted tree model.

set.seed(777)
predictBoosted <- predict(genderBoosted, newdata=test)
confusionMatrix(predictBoosted, test$label)

# SVM Model
set.seed(777)
predictSvm <- predict(genderSvm, test)
table(predictSvm, test$label)
(15+14)/nrow(test)

# XGBoost Model.

testx <- sapply(test, as.numeric)
testx[,21] <- testx[,21] - 1
results <- predict(genderXG, testx)
table(testx[,21], results >= 0.5)
(4 + 14) / nrow(test)

# Stacking models in an ensemble.
results1 <- predict(genderSvm, newdata=test)
results2 <- predict(genderForest, newdata=test)
results3 <- predict(genderBoosted, newdata=test)
combo <- data.frame(results1, results2, results3, y = test$label)

set.seed(777)
genderStacked <- tuneRF(combo[,-4], combo[,4], stepFactor=.5, doBest=TRUE)
predictStacked <- predict(genderStacked, newdata=combo)
table(predictStacked, test$label)
(15+14) / nrow(test)
