data.training = read.csv("pml-training.csv", na.strings=c("", "NA"))
data.testing = read.csv("pml-testing.csv", na.strings=c("", "NA"))
data.training <- data.training[, colSums(is.na(data.training)) == 0] 
data.testing <- data.testing[, colSums(is.na(data.testing)) == 0] 
data.training <- data.training[, -c(1:7)] 
data.testing <- data.testing[, -c(1:7)] 

set.seed(4042018)

inTrain <- createDataPartition(y=data.training$classe, p = 0.7, list = FALSE)
training <- data.training[inTrain,]
testing  <- data.training[-inTrain,]

library(randomForest)
library(caret)
library(ggplot2)

forestTrain <- randomForest(classe~ ., data=training,mtry=27,ntree=500)
order(varImp(forestTrain))
varImp(forestTrain)
varImpPlot(forestTrain,type=2,main="Variable importance")

tc <- trainControl(method="cv")
model.rf <- train(classe ~ ., data=training, method="rf", trControl=tc)
model.rf
confusionMatrix(testing$classe,predict(model.rf, newdata=testing))

test <- predict(model.rf,data.testing)
test
