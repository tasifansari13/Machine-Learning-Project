// Load the data

train <- read.csv("training.csv", na.strings=c("NA","#DIV/0!",""))
test <- read.csv("testing.csv", na.strings=c("NA","#DIV/0!",""))

names(train)
str(train)
summary(train)
summary(train$classe)#this is the outcome we want to predict

// Split traning and testing data set

inTrain <- createDataPartition(y=train$classe, p=0.6, list=FALSE)
myTrain <- train[inTrain, ]
myTest <- train[-inTrain, ]
dim(myTrain)

// Feature Selection

#first we will remove variables with mostly NAs (use threshold of >75%)
mytrain_SUB <- myTrain
for (i in 1:length(myTrain)) {
  if (sum(is.na(myTrain[ , i])) / nrow(myTrain) >= .75) {
    for (j in 1:length(mytrain_SUB)) {
      if (length(grep(names(myTrain[i]), names(mytrain_SUB)[j]))==1) {
        mytrain_SUB <- mytrain_SUB[ , -j]
      }
    }
  }
}

dim(mytrain_SUB)
#remove columns that are obviously not predictors
mytrain_SUB2 <- mytrain_SUB[,8:length(mytrain_SUB)]

#remove variables with near zero variance
NZV <- nearZeroVar(mytrain_SUB2, saveMetrics = TRUE)
NZV #all false, none to remove

//Random Forest Model

I decided to use the random forest model to build my machine learning algorithm as it is appropriate for a classification problem as we have and based on information provided in class lectures this model tends to be more accurate than some other classification models.

Below I fit my model on my training data and then use my model to predict classe on my subset of data used for cross validation.

#fit model- RANDOM FOREST
set.seed(223)

modFit <- randomForest(classe~., data = mytrain_SUB2)
print(modFit)

#cross validation on my testing data
#out of sample error
predict1 <- predict(modFit, myTest, type = "class")
confusionMatrix(myTest$classe, predict1)

#in sample error
predict_train <- predict(modFit, myTrain, type = "class")
confusionMatrix(myTrain$classe, predict_train)

//Apply to final test set

Finally, we apply our model to the final test data. Upon submission all predictions were correct!

predict_FINAL <- predict(modFit, test, type = "class")
print(predict_FINAL)

##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E

pml_write_files = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file=filename, quote=FALSE,row.names=FALSE, col.names=FALSE)
  }
}

pml_write_files(predict_FINAL)
