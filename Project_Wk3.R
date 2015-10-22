# Week 3 project for Practical Machine Learning

library(caret)
library(ggplot2)
library(randomForest)

all_data <- read.csv(file = "pml-training.csv")
final_test <- read.csv(file = "pml-testing.csv")

#####################################
### Removing near zero variance data
#####################################

## some variables have no variance.  Let's get rid of those now

nzv_data <- nearZeroVar(all_data, saveMetrics=TRUE)
good_data <- all_data[ , !nzv_data$nzv]


## I'll also remove the index column, "X"
good_data <- good_data[, -1] 
GD1 <- good_data



#########################################
### Removing variables with too many NAs
#########################################

## Some variables have > 19000 NAs. I'm going to eliminate these here

good_list <- c()
for(i in 1:dim(good_data)[2]){
        num_NA <- sum(is.na(good_data[ ,i]))
        if(num_NA < 18000){
                good_list <- c(good_list, i)
        }
}

good_data <- good_data[, good_list]


## I'll also remove timestamp, num_window, and username data since they do not help my analysis
##  and can cause the program to hang up

good_data <- good_data[,-c(1:5)]

GD2 <- good_data
##################################################
### Create my own training and testing data sets
##################################################

inTrain <- createDataPartition(y = good_data$classe, p = 0.7, list = FALSE)

training <- good_data[inTrain, ]
testing <- good_data[-inTrain, ]



##################################################
### Training and fitting a model
##################################################

## Using the caret package I train the model using the random forest method, 'rf'
## I also perform 5-fold cross-validation using the trControl command

#model_fit<-train(classe ~ .,data=training , method="rf", prox = TRUE, allowParallel = TRUE,
#                 trControl=trainControl(method="cv",number=5))

## This model takes ~ 45 min to run on my computer. Here I save the file so I can
##  load it again in the future and skip the remodeling if not needed
save(model_fit, file = "model.RData")
load("model.RData")

## Comparing the model fit to the training data as applied to the testing data
pred <- predict(model_fit, newdata = testing)

print(confusionMatrix(pred, testing$classe))




#######################################
### Helpful figures
######################################

## plot of importance of variables.  Shows decrease in Gini for each variable used in fit
varImpPlot(model_fit$finalModel)

## plot of error
#plot(model_fit$finalModel, log="y")

## looking at individual trees
#getTree(model_fit$finalModel)

## Plot of two factors with greatest decrease in Gini
plot(training$roll_belt, training$pitch_forearm, col = training$classe)
legend(50,0, legend = names(summary(training$classe)), col = 1:5, lty = 1, lwd = 2)

## Plot of four factors with greatest decrease in Gini
#pairs(training[,c(1,41,3,2)], col = training$classe)


#########################################
### Final testing data analysis
#########################################

## Removing zero variance data 
nzv_data_test <- nearZeroVar(final_test, saveMetrics=TRUE)
good_test <- final_test[ , !nzv_data_test$nzv]


## I'll also remove the index column, "X"
good_test <- good_test[, -1] 


## Some variables have > 10 NAs. I'm going to eliminate these here
good_list_test <- c()
for(i in 1:dim(good_test)[2]){
        num_NA <- sum(is.na(good_test[ ,i]))
        if(num_NA < 10){
                good_list_test <- c(good_list_test, i)
        }
}
good_test <- good_test[, good_list_test]

## removing timestamps and usernames
good_test <- good_test[, -c(1:5)] 


## Finally, I predict the classes of the the test data

test_pred <- predict(model_fit, newdata = good_test)


