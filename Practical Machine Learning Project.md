---
title: "Predicting Exerice Movements Using a Random Forest Algorithm"
output: word_document
---
***Ben Piggot***
  
***September 17, 2014***

### Synopsis
This brief analysis makes use of data used by Velloso et al. in their 2013 study, "Qualitative Activity Recognition of Weight Lifting Exercises." (See: http://groupware.les.inf.puc-rio.br/har for more information). Using this data, it creates a model designed to predict the manner in which individuals perform weightlifting exercises.

### Data Processing
My first step is to load in the training set data I use to build my model from the url below. Additionally, I load the plyr, caret, and randomForest packages.

```{r, echo=TRUE}
myurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(url=myurl, destfile="pml-training.csv", method="curl")
TrainingSet <- read.csv("pml-training.csv")
library(plyr)
library(caret)
library(randomForest)
```
Once the data is loaded into my working directory, I clean the original training set. I remove variables that largely consist of missing data and variables that do not measure physical activity. I also eliminate variables that R falsely recognizes to be factor variables, but which in fact mostly contain missing or garbled information in the original .csv file. I make sure to remove and then re-attach the "classe" variability to my training set before eliminating the rest of the factors in the Training Set. The "classe" variable measures the outcome of interest in my analysis. As a result of my cleaning, the number of variables I will use in my model is reduced from 161 to 53.

```{r, echo=TRUE}
CleanedTrainingSet <- TrainingSet[ , colSums(is.na(TrainingSet)) < 15000] 
CleanedTrainingSet <- CleanedTrainingSet[,-c(1:7)]
CleanedTrainingSet1 <- CleanedTrainingSet[,-86]
CleanedTrainingSet1 <- CleanedTrainingSet1[, !sapply(CleanedTrainingSet1, is.factor)]
CleanedTrainingSet1 <- cbind(CleanedTrainingSet1, CleanedTrainingSet$classe)
CleanedTrainingSet1 <- rename(CleanedTrainingSet1, c("CleanedTrainingSet$classe"="classe"))
```
Next, I subset the cleaned training set into two halves. The first half will be used to construct my model; the second half will be used to cross-validate the model I build.

```{r, echo=TRUE}
set.seed(21)
Sample1<- sample(1:dim(CleanedTrainingSet1)[1],size=dim(CleanedTrainingSet1)[1]/2,replace=F)
SampleTrain <- CleanedTrainingSet1[Sample1,]
CVTrain <- CleanedTrainingSet1[-Sample1,]
```
### Building a Model
I then build my model, predicting the classe variable utilizing a Random Forest algorithm as called inside the train() function. I then use this model to predict the values of the sample from which I constructed my model (SampleTrain). The results are then stored and presented in a confusion matrix.

```{r, echo=TRUE}
RFTrain <- train(classe ~., method="rf", data=SampleTrain)
Prediction1 <- predict(RFTrain$finalModel, SampleTrain)
Prediction1.Summary <- confusionMatrix(SampleTrain$classe, Prediction1)
Prediction1.Summary
```

### Cross-Validating the Model
As the results above suggest, the model I have built predicts the classe outcome with perfect accuracy. However, this level of accuracy could be misleading as it might be the result of overfitting. Therefore, I cross-validate my model on two samples drawn at random from the half of the training set I did not use to construct my model (CVTrain).

```{r, echo=TRUE}
set.seed(51)
Sample2 <- sample(1:dim(CVTrain)[1],size=dim(CVTrain)[1]/20,replace=F)
CVSample1 <- CVTrain[Sample2,]
Prediction2 <- predict(RFTrain$finalModel, CVSample1)
Prediction2.Summary <- confusionMatrix(CVSample1$classe, Prediction2)
Prediction2.Summary
```
As the numbers above indicate, the model proves highly accurate in its first out-of-sample test: it correctly predicts 99.2% of the actual values. Below, the second out-of-sample prediction shows similar levels of accuracy: it correctly predicts 98.8% of the actual values. Accordingly, I expect the out-of-sample error for my model to be approximately 1%.

```{r, echo=TRUE}
set.seed(81)
Sample3 <- sample(1:dim(CVTrain)[1],size=dim(CVTrain)[1]/20,replace=F)
CVSample2 <- CVTrain[Sample3,]
Prediction3 <- predict(RFTrain$finalModel, CVSample2)
Prediction3.Summary <- confusionMatrix(CVSample2$classe, Prediction3)
Prediction3.Summary
```
### Testing the Model
Now that I am now quite confident my model accurately predicts exercise style as measured by the "classe" variable, I load in the testing set data. I then clean the testing set using the same procedures I used to clean the training set. 

```{r, echo=TRUE}
myurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url=myurl, destfile="pml-testing.csv", method="curl")
TestingSet <- read.csv("pml-testing.csv")
CleanedTestingSet <- TestingSet[ , colSums(is.na(TestingSet)) < 10] 
CleanedTestingSet <- CleanedTestingSet[,-c(1:7)]
CleanedTestingSet <- CleanedTestingSet[, !sapply(CleanedTestingSet, is.factor)]
```
Once the testing set has been properly prepared, I predict its "classe" values using my model. The 20 predicted outcomes can be seen below. All 20 predictions are correct: the model has performed very well on the testing set.

```{r, echo=TRUE}
PredictionFinal <- predict(RFTrain$finalModel, CleanedTestingSet)
PredictionFinal
```
