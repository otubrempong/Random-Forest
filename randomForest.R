diabet <- read.csv("C:/Users/OWUSU/Desktop/ML n DL/kaggle/diabetes.csv",header = TRUE)
View(diabet)

set.seed(2)
id <- sample(2,nrow(diabet),prob = c(0.7,0.3),replace = TRUE)
diabet_train <- diabet[id==1,]
diabet_test <- diabet[id==2,]

install.packages("randomForest")
library(randomForest)

diabet$Outcome <- as.factor(diabet$Outcome)
diabet_train$Outcome <- as.factor(diabet_train$Outcome)
#optimized value of mtry
bestmtry <- tuneRF(diabet_train,diabet_train$Outcome,stepFactor = 1.2,improve = 0.01,trace = T,plot = T)

diabet_forest <- randomForest(diabet_train$Outcome~.,data = diabet_train)
diabet_forest

#give Gini index ( priority of variables)
diabet_forest$importance
varImpPlot(diabet_forest)

pred_diabet <- predict(diabet_forest,newdata = diabet_test,type = 'class')
library(caret)
confusionMatrix(table(pred_diabet,diabet_test$Outcome))
