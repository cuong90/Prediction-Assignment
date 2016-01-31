# Prediction Assignment
Manh Cuong  
January 31, 2016  

# Summary
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. This project will use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. We were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

The goal of this assignment is to predict the manner in which they did the exercise. This assignment will:

- create a report describing how the model is built
- how cross validation is use 
- what is the expected out of sample error and why we made the choices we did 
- use prediction model to predict 20 different test cases

# Data Preparation

## Getting Data
The following script was used to make sure the data was available for loading and processing. If the files are not in the working directory, R will download them.


```r
if (!file.exists("pml-training.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                  destfile = "pml-training.csv")
}
if (!file.exists("pml-testing.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                  destfile = "pml-testing.csv")
}

train_data = read.csv("pml-training.csv", na.strings=c("", "NA", "NULL"))
test_data = read.csv("pml-testing.csv", na.strings=c("", "NA", "NULL"))
dim(train_data)
```

```
## [1] 19622   160
```

```r
dim(test_data)
```

```
## [1]  20 160
```

## Cleaning Data

Remove variables that we believe have too many NA values.

```r
train_data <- train_data[ , colSums(is.na(train_data)) == 0]
dim(train_data)
```

```
## [1] 19622    60
```

Remove unrelevant variables There are some unrelevant variables that can be removed as they are unlikely to be related to dependent variable.

```r
train_data<- train_data[, -which(names(train_data) %in% c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window'))]
dim(train_data)
```

```
## [1] 19622    53
```

Check the variables that have extremely low variance (this method is useful nearZeroVar() )

```r
library(caret)
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```r
zero_variables <- nearZeroVar(train_data[sapply(train_data, is.numeric)], saveMetrics = TRUE)
train_data <- train_data[,zero_variables[, 'nzv']==0]
```

Split data to train and test for cross validation

```r
in_train <- createDataPartition(y=train_data$classe, p=0.7, list=FALSE)
in_train <- createDataPartition(y=train_data$classe, p=0.7, list=FALSE)
train <- train_data[in_train,]
test <- train_data[-in_train,]
dim(train)
```

```
## [1] 13737    53
```

```r
dim(test)
```

```
## [1] 5885   53
```

# Building Prediction Model

We try to build the prediction model with different ways. Here, we apply regresson tree, Rpart from Caret, Random Forests to build three different models
## Regression Tree

```r
library(tree)
set.seed(12345)
tree_train <- tree(classe~.,data=train)
```

## Rpart from Caret

```r
library(caret)
modFit <- train(classe ~ .,method="rpart",data=train)
```

```
## Loading required package: rpart
```

## Random Forests

```r
require(randomForest)
```

```
## Loading required package: randomForest
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```r
set.seed(12345)
rf_train=randomForest(classe~.,data=train,ntree=100, importance=TRUE)
```

## Cross Validation
We are going to check the performance of models on the test data by cross validation.

```r
tree_predictor <- predict(tree_train,test,type="class")
predMatrix <- with(test,table(tree_predictor,classe))
sum(diag(predMatrix))/sum(as.vector(predMatrix))
```

```
## [1] 0.6598131
```


```r
rpart_predictor <- predict(modFit,test)
predMatrix = with(test,table(rpart_predictor,classe))
sum(diag(predMatrix))/sum(as.vector(predMatrix))
```

```
## [1] 0.4997451
```


```r
rf_predictor=predict(rf_train,test,type="class")
predMatrix = with(test,table(rf_predictor,classe))
sum(diag(predMatrix))/sum(as.vector(predMatrix)) # error rate
```

```
## [1] 0.9926933
```

## Out-of Sample Accuracy

From checking three models by using cross validation, we can see that random_forests_based model has the highest accuracy. That gives us a reason to choose that model.

# Conclusions
After building three different prediction models, we check them by using cross validation. Finally, we get the best model which is random_forests_based model.

Now we apply that model for test_data.

```r
answers <- predict(rf_train, test_data)
answers
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
