TrainingSet <- read.csv("pml-training.csv")
summary(TrainingSet)
str(TrainingSet)
library(caret)
library(plyr)

CleanedTrainingSet <- TrainingSet[ , colSums(is.na(TrainingSet)) < 15000] 
CleanedTrainingSet <- CleanedTrainingSet[,-c(1:7)]
CleanedTrainingSet1 <- CleanedTrainingSet[,-86]
str(CleanedTrainingSet1)
CleanedTrainingSet1 <- CleanedTrainingSet1[, !sapply(CleanedTrainingSet1, is.factor)]
CleanedTrainingSet1 <- cbind(CleanedTrainingSet1, CleanedTrainingSet$classe)
CleanedTrainingSet1 <- rename(CleanedTrainingSet1, c("CleanedTrainingSet$classe"="classe"))

set.seed(21)
Sample1<- sample(1:dim(CleanedTrainingSet1)[1],size=dim(CleanedTrainingSet1)[1]/2,replace=F)
SampleTrain <- CleanedTrainingSet1[Sample1,]
CVTrain <- CleanedTrainingSet1[-Sample1,]
RFTrain2 <- train(classe ~., method="rf", ntree=50, data=SampleTrain)
Prediction1 <- predict(RFTrain2$finalModel, SampleTrain)
Prediction1.Summary <- confusionMatrix(SampleTrain$classe, Prediction1)

set.seed(51)
Sample2 <- sample(1:dim(CVTrain)[1],size=dim(CVTrain)[1]/20,replace=F)
CVSample1 <- CVTrain[Sample2,]
Prediction2 <- prediRct(RFTrain2$finalModel, CVSample1)
Prediction2.Summary <- confusionMatrix(CVSample1$classe, Prediction2)

set.seed(81)
Sample3 <- sample(1:dim(CVTrain)[1],size=dim(CVTrain)[1]/20,replace=F)
CVSample2 <- CVTrain[Sample3,]
Prediction3 <- predict(RFTrain2$finalModel, CVSample2)
Prediction3.Summary <- confusionMatrix(CVSample2$classe, Prediction3)

set.seed(111)
Sample4 <- sample(1:dim(CVTrain)[1],size=dim(CVTrain)[1]/10,replace=T)
CVSample3 <- CVTrain[Sample4,]
Prediction4 <- predict(RFTrain2$finalModel, CVSample3)
Prediction4.Summary <- confusionMatrix(CVSample3$classe, Prediction4)


TestingSet <- read.csv("pml-testing.csv")
CleanedTestingSet <- TestingSet[ , colSums(is.na(TestingSet)) < 10] 
CleanedTestingSet <- CleanedTestingSet[,-c(1:7)]
CleanedTestingSet <- CleanedTestingSet[, !sapply(CleanedTestingSet, is.factor)]

PredictionFinal <- predict(RFTrain2$finalModel, CleanedTestingSet)


###################################


set.seed(13)
Sample <- sample(1:dim(CleanedTrainingSet1)[1],size=dim(CleanedTrainingSet1)[1]/20,replace=F)
PrelimTrain2 <- CleanedTrainingSet1[Sample,]
RFTrain1 <- train(classe ~., method="rf", data=PrelimTrain2)
PrelimPrediction <- predict(RFTrain1$finalModel, PrelimTrain2)
PrelimPrediction.Summary <- confusionMatrix(PrelimTrain2$classe, PrelimPrediction)

set.seed(191)
Sample10 <- sample(1:dim(CVTrain)[1],size=dim(CVTrain)[1]/20,replace=F)
CVSample10 <- CVTrain[Sample10,]
TestPredict <- predict(RFTrain1$finalModel, CVSample10)
TestPredict.Summary <- confusionMatrix(CVSample10$classe, TestPredict)

set.seed(32)
TreeTrain <- train(classe ~., method="rpart", data=PrelimTrain2)
TreeSample <- sample(1:dim(CleanedTrainingSet1)[1],size=dim(CleanedTrainingSet1)[1]/2, replace=F)
PrelimTree <- CleanedTrainingSet1[TreeSample,]
TreeTrain <- train(classe ~., method="rpart", data=PrelimTree)
TreePredict <- predict(TreeTrain$finalModel, PrelimTree)
TreePredict.Summary <- confusionMatrix(PrelimTrain$classe, TreePredict)

RFTrain3 <- train(classe ~., method="rf", ntree=50, data=SampleTrain)
