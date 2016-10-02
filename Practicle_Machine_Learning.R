library(caret)
library(rattle)
library(parallel)
library(doParallel)


setwd("C:/Users/simon.monk/Documents/Data Science Course/Practicle Machine Learning")

#### Get & Parition Data ####

trainingDataSet <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
validationDataSet <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

## Partition Training Data into Training and Testing Data sets with 60/40 split ##

inTrain <- createDataPartition(y=trainingDataSet$classe, p=0.6, list=FALSE)
training <- trainingDataSet[inTrain, ]
testing <- trainingDataSet[-inTrain, ]


#### Data Cleaning ####

  ##Convert NAs and "" to 0
  training[is.na(training)] <- 0
  testing[is.na(testing)] <- 0
  validationDataSet[is.na(validationDataSet)] <- 0

  ## Remove variables with near zero variance.
  ## Run the nearZeroVar on the training set to find the variables we want to remove
  ## Then remove these variables from both the training set and the test set
  training.nearZero <- nearZeroVar(training, saveMetrics=TRUE)
  myTraining <- training[!training.nearZero$nzv]
  myTesting <- testing[!training.nearZero$nzv]
  myValidation <- validationDataSet[!training.nearZero$nzv]
  head(myTraining)

  ## We're left with:
  ## 19622 observations of 59 covariates in the training set (down from 160 covariates)
  ## 20 observations of 59 covariates in the test set
  dim(myTraining)
  dim(myTesting)
  dim(myValidation)
  
  
  ## drop the first column (the index column) ## not sure this is necessary actually)
  myTraining <- myTraining[c(-1)]
  myTesting <- myTesting[c(-1)]
  myValidation <- myValidation[c(-1)]

### Classification Tree ###

#### Fitting: Classification Tree####
##control <- trainControl(method = "cv", number = 5)
mod.ClassificationTree <- train(classe ~ ., data=myTraining, method="rpart")##, trControl = control)
##print(mod.tree, digits = 4)
  
fancyRpartPlot(mod.tree$finalModel)

#### Predicting ####
pred.classificationTree <- predict(mod.ClassificationTree, myTesting)
confusionMatrix(predictions, myTesting$classe)$overall[1]
## Accuracy is about 50% so try different method


cluster <- makeCluster(detectCores()-1) # convention to leave 1 core for OS
registerDoParallel()

#### Fitting: Random Forest####
##mod.tree <- train(classe ~ ., data=myTraining, method="rf")
pred.rf <- predict(mod.tree, myTesting)
confusionMatrix(pred.rf, myTesting$classe)$overall[1]

pred.rf.validation <- predict(mod.tree, myValidation)

