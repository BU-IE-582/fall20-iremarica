# Homework4
# libraries
suppressMessages(library(readr))
suppressMessages(library(readxl))
suppressMessages(library(glmnet))
suppressMessages(library(Metrics))
suppressMessages(library(rpart))
suppressMessages(library(rattle))
suppressMessages(library(stats))
suppressMessages(library(e1071))
suppressMessages(library(caret))
suppressMessages(library(randomForest))
suppressMessages(library(gbm))

# reading dataset 1
health_data <- suppressMessages(read_csv("/Users/iremarica/Desktop/Homework4/FetalHealth.csv"))
health_data$fetal_health <- as.factor(health_data$fetal_health)

# creating train and test sets for dataset 1
set.seed(1)
health_index <- sample(1:nrow(health_data), (2/3)*nrow(health_data))
health_train <- health_data[health_index, ] 
health_test <- health_data[-health_index, ]
paste("Total:", nrow(health_data), "  Train:", nrow(health_train), "  Test:", nrow(health_test))

### Penalized Regression Approaches (PRA)  
set.seed(2)
health_cv_fit <- cv.glmnet(as.matrix(health_train[,-22]), health_train$fetal_health, family='multinomial', nfolds=10)
health_cv_fit
cat(" Lambda min:", health_cv_fit$lambda.min, "\n", "Lambda 1se:", health_cv_fit$lambda.1se)
plot(health_cv_fit)

health_pra <- glmnet(as.matrix(health_train[,-22]), health_train$fetal_health, family="multinomial", lambda=health_cv_fit$lambda.min)
health_pra_pred <- data.frame(predict(health_pra, as.matrix(health_test[,-22]), type="class"))
health_pra_pred$s0 <- as.factor(health_pra_pred$s0)
confusionMatrix(health_pra_pred$s0, health_test$fetal_health)

### Decision Trees (DT)  
set.seed(3)
health_dt_minbucket <- tune.rpart(fetal_health~., data=health_train, minbucket=seq(10,35,5))
plot(health_dt_minbucket,main="Performance of rpart vs. minbucket")
health_dt_minbucket$best.parameters$minbucket

health_dt_cp <- tune.rpart(fetal_health~., data=health_train, cp=seq(0.005,0.03,0.005))
plot(health_dt_cp,main="Performance of rpart vs. cp")
health_dt_cp$best.parameters$cp

health_dt <- rpart(fetal_health~., data=health_train, method='class', control=rpart.control(minbucket=health_dt_minbucket$best.parameters$minbucket, cp=health_dt_cp$best.parameters$cp))
fancyRpartPlot(health_dt)

health_dt_pred <- data.frame(predict(health_dt, health_test[,-22], type="class"))
colnames(health_dt_pred) <- "s0"
health_dt_pred$s0 <- as.factor(health_dt_pred$s0)
confusionMatrix(health_dt_pred$s0, health_test$fetal_health)

### Random Forests (RF)  
set.seed(4)
health_rf <- randomForest(data.matrix(health_train[,-22]), health_train$fetal_health, ntree=500, nodesize=5)
health_rf$mtry

fitControl <- trainControl(method = 'repeatedcv', number=3, repeats=2, search='grid')
tunegrid <- expand.grid(.mtry=seq(2,12,2))
health_rf <- train(fetal_health~., data=health_train, method = "rf", metric="Accuracy", trControl=fitControl, tuneGrid=tunegrid)
print(health_rf)
plot(health_rf)

health_rf <- randomForest(health_train[,-22],health_train$fetal_health, ntree=500, nodesize=5, mtry=6)
health_rf

varImpPlot(health_rf)

health_rf_pred <- predict(health_rf, health_test[,-22], type="class")
confusionMatrix(health_rf_pred, health_test$fetal_health)

### Stochastic Gradient Boosting (SGB)  
set.seed(5)
fitControl <- trainControl(method='repeatedcv', number=5, repeats=3, verboseIter=FALSE, summaryFunction=multiClassSummary, allowParallel=FALSE)
tunegrid <- expand.grid(interaction.depth=c(1,2,3), shrinkage=c(0.001,0.005,0.01), n.trees=c(50,100,150), n.minobsinnode=10)
garbage <- capture.output(
health_gbm <- train(fetal_health~., data=health_train, method = "gbm", trControl=fitControl, tuneGrid=tunegrid))
print(health_gbm)
plot(health_gbm)

health_gbm_final=suppressWarnings(gbm(fetal_health~., data=health_train, distribution="multinomial", n.trees=150, interaction.depth=3, n.minobsinnode=10, shrinkage=0.01))
summary(health_gbm_final)

health_gbm_pred <- predict(health_gbm, health_test[,-22], type="raw")
confusionMatrix(health_gbm_pred, health_test$fetal_health)

## Dataset 2
# reading dataset 2
churn_data <- suppressMessages(read_csv("/Users/iremarica/Desktop/Homework4/BankChurners.csv"))

# substracting non-predictive features
churn_data <- churn_data[,-c(1,2,23,24)]

# defining data types
for(i in 1:nrow(churn_data)){
    if(churn_data[i,1] == "Attrited Customer"){
        churn_data[i,1] <- 1
    }
    if(churn_data[i,1] == "Existing Customer"){
        churn_data[i,1] <- 0
    }
}
churn_data$Attrition_Flag <- as.factor(churn_data$Attrition_Flag)
churn_data$Gender <- as.factor(churn_data$Gender)
churn_data$Marital_Status <- as.factor(churn_data$Marital_Status)
churn_data$Education_Level <- as.factor(churn_data$Education_Level)
churn_data$Income_Category <- as.factor(churn_data$Income_Category)
churn_data$Card_Category <- as.factor(churn_data$Card_Category)

# creating train and test sets for dataset 2
set.seed(6)
churn_index <- sample(1:nrow(churn_data), (2/3)*nrow(churn_data))
churn_train <- churn_data[churn_index, ] 
churn_test <- churn_data[-churn_index, ]
paste("Total:", nrow(churn_data), "  Train:", nrow(churn_train), "  Test:", nrow(churn_test))


### Penalized Regression Approaches (PRA)  
set.seed(7)
churn_cv_fit <- cv.glmnet(data.matrix(churn_train[,-1]), as.matrix(churn_train[,1]), family='binomial', nfolds=10)
churn_cv_fit
cat(" Lambda min:", churn_cv_fit$lambda.min, "\n", "Lambda 1se:", churn_cv_fit$lambda.1se)
plot(churn_cv_fit)

churn_pra <- glmnet(data.matrix(churn_train[,-1]), as.matrix(churn_train[,1]), family="binomial", lambda=churn_cv_fit$lambda.min)
churn_pra_pred <- data.frame(predict(churn_pra, data.matrix(churn_test[,-1]), type="class"))
confusionMatrix(churn_pra_pred[,1], churn_test$Attrition_Flag)

### Decision Trees (DT)  
set.seed(8)
churn_dt_minbucket <- tune.rpart(Attrition_Flag~., data=churn_train, minbucket=seq(5,30,5))
plot(churn_dt_minbucket,main="Performance of rpart vs. minbucket")
churn_dt_minbucket$best.parameters$minbucket

churn_dt_cp <- tune.rpart(Attrition_Flag~., data=churn_train, cp=seq(0.005,0.03,0.005))
plot(churn_dt_cp,main="Performance of rpart vs. cp")
churn_dt_cp$best.parameters$cp

churn_dt <- rpart(Attrition_Flag~., data=churn_train, method="class", control=rpart.control(minbucket=churn_dt_minbucket$best.parameters$minbucket, cp=churn_dt_cp$best.parameters$cp))
fancyRpartPlot(churn_dt)

churn_dt_testpred <- predict(churn_dt, churn_test[,-1], type="class")
confusionMatrix(churn_dt_testpred, churn_test$Attrition_Flag)

### Random Forests (RF)  
set.seed(9)
churn_rf <- randomForest(churn_train[,-1],churn_train$Attrition_Flag, ntree=500, nodesize=5)
churn_rf$mtry

fitControl <- trainControl(method = 'repeatedcv', number=3, repeats=2, search='grid')
tunegrid <- expand.grid(.mtry=seq(2,12,2))
churn_rf <- train(Attrition_Flag~., data=churn_train, method = "rf", metric="Accuracy", trControl=fitControl, tuneGrid=tunegrid)
print(churn_rf)
plot(churn_rf)

churn_rf <- randomForest(churn_train[,-1],churn_train$Attrition_Flag, ntree=500, nodesize=5, mtry=12)
churn_rf

varImpPlot(churn_rf)

churn_rf_pred <- predict(churn_rf, churn_test[,-1], type="class")
confusionMatrix(churn_rf_pred, churn_test$Attrition_Flag)

### Stochastic Gradient Boosting (SGB)  
set.seed(10)
fitControl <- trainControl(method='repeatedcv', number=5, repeats=3, verboseIter=FALSE, allowParallel=FALSE)
tunegrid <- expand.grid(interaction.depth=c(1,2,3), shrinkage=c(0.001,0.005,0.01), n.trees=c(50,100,150), n.minobsinnode=10)
garbage <- capture.output(
    churn_gbm <- train(Attrition_Flag~., data=churn_train, method="gbm", trControl=fitControl, tuneGrid=tunegrid))
print(churn_gbm)
plot(churn_gbm)

churn_gbm_pred <- predict(churn_gbm, churn_test[,-1], type="raw")
confusionMatrix(churn_gbm_pred, churn_test$Attrition_Flag)

## Dataset 3
# reading dataset 3
article_data <- suppressMessages(read_csv("/Users/iremarica/Desktop/Homework4/SportsArticles.csv"))

# substracting non-predictive features (X1, TextID and URL)
article_data <- article_data[,-c(1,2,3)]

# defining data types
article_data$Label <- as.factor(article_data$Label)

# creating train and test sets for dataset 3
set.seed(11)
article_index <- sample(1:nrow(article_data), (2/3)*nrow(article_data))
article_train <- article_data[article_index, ] 
article_test <- article_data[-article_index, ]
paste("Total:", nrow(article_data), "  Train:", nrow(article_train), "  Test:", nrow(article_test))


### Penalized Regression Approaches (PRA)  
set.seed(12)
article_cv_fit <- cv.glmnet(data.matrix(article_train[,-1]), as.matrix(article_train[,1]), family='binomial', nfolds=10)
article_cv_fit
cat(" Lambda min:", article_cv_fit$lambda.min, "\n", "Lambda 1se:", article_cv_fit$lambda.1se)
plot(article_cv_fit)

article_pra <- glmnet(data.matrix(article_train[,-1]), as.matrix(article_train[,1]), family="binomial", lambda=article_cv_fit$lambda.min)
article_pra_pred <- data.frame(predict(article_pra, data.matrix(article_test[,-1]), type="class"))
article_pra_pred$s0 <- as.factor(article_pra_pred$s0)
confusionMatrix(article_pra_pred$s0, article_test$Label)

### Decision Trees (DT)  
set.seed(13)
article_dt_minbucket <- tune.rpart(Label~., data=article_train, minbucket=seq(5,30,5))
plot(article_dt_minbucket,main="Performance of rpart vs. minbucket")
article_dt_minbucket$best.parameters$minbucket

article_dt_cp <- tune.rpart(Label~., data=article_train, cp=seq(0.005,0.03,0.005))
plot(article_dt_cp,main="Performance of rpart vs. cp")
article_dt_cp$best.parameters$cp

article_dt <- rpart(Label~., data=article_train, method="class", control=rpart.control(minbucket=article_dt_minbucket$best.parameters$minbucket, cp=article_dt_cp$best.parameters$cp))
fancyRpartPlot(article_dt)

article_dt_pred <- predict(article_dt, article_test[,-1], type="class")
confusionMatrix(article_dt_pred, article_test$Label)

### Random Forests (RF)  
set.seed(14)
article_rf <- randomForest(article_train[,-1],article_train$Label, ntree=500, nodesize=5)
article_rf$mtry

fitControl <- trainControl(method = 'repeatedcv', number=5, repeats=3, search='grid')
tunegrid <- expand.grid(.mtry=seq(5,15,2))
article_rf <- train(Label~., data=article_train, method = "rf", metric="Accuracy", trControl=fitControl, tuneGrid=tunegrid)
print(article_rf)
plot(article_rf)

article_rf <- randomForest(article_train[,-1],article_train$Label, ntree=500, nodesize=5, mtry=9)
article_rf

varImpPlot(article_rf)

article_rf_pred <- predict(article_rf, article_test[,-1], type="class")
confusionMatrix(article_rf_pred, article_test$Label)

### Stochastic Gradient Boosting (SGB)  
set.seed(15)
fitControl <- trainControl(method='repeatedcv', number=5, repeats=3, verboseIter=FALSE, summaryFunction=twoClassSummary, classProbs=TRUE, allowParallel=FALSE)
tunegrid <- expand.grid(interaction.depth=c(1,2,3), shrinkage=c(0.001,0.005,0.01), n.trees=c(50,100,150), n.minobsinnode=10)
garbage <- suppressWarnings(capture.output(
article_gbm <- train(Label~., data=article_train, method = "gbm", trControl=fitControl, tuneGrid=tunegrid)))
print(article_gbm)
plot(article_gbm)

article_gbm_pred <- predict(article_gbm, article_test[,-1], type="raw")
confusionMatrix(article_gbm_pred, article_test$Label)

## Dataset 4
# reading dataset 4
cond_data <- suppressMessages(read_csv("/Users/iremarica/Desktop/Homework4/superconductor.csv"))
set.seed(16)
cond_index <- sample(1:nrow(cond_data), 5000)
cond_data <- cond_data[cond_index, ]

# creating train and test sets for dataset 4
set.seed(17)
cond_index <- sample(1:nrow(cond_data), (2/3)*nrow(cond_data))
cond_train <- cond_data[cond_index, ] 
cond_test <- cond_data[-cond_index, ]
paste("Total:", nrow(cond_data), "  Train:", nrow(cond_train), "  Test:", nrow(cond_test))

### Penalized Regression Approaches (PRA) 
set.seed(18)
cond_cv_fit <- cv.glmnet(data.matrix(cond_train[,-82]), data.matrix(cond_train[,82]), family='gaussian', nfolds=10)
cond_cv_fit
cat(" Lambda min:", cond_cv_fit$lambda.min, "\n", "Lambda 1se:", cond_cv_fit$lambda.1se)
plot(cond_cv_fit)

cond_pra <- glmnet(data.matrix(cond_train[,-82]), cond_train$critical_temp, family="gaussian", lambda=cond_cv_fit$lambda.min)
cond_pra_pred <- data.frame(predict(cond_pra, data.matrix(cond_test[,-82])))
colnames(cond_pra_pred) <- "s0"
rmse(cond_test$critical_temp, cond_pra_pred$s0)

### Decision Trees (DT)  
set.seed(19)
cond_dt_minbucket <- tune.rpart(critical_temp~., data=cond_train, minbucket=c(5,6,7))
plot(cond_dt_minbucket,main="Performance of rpart vs. minbucket")
cond_dt_minbucket$best.parameters$minbucket

cond_dt_cp <- tune.rpart(critical_temp~., data=cond_train, cp=c(0.005,0.01,0.015))
plot(cond_dt_cp,main="Performance of rpart vs. cp")
cond_dt_cp$best.parameters$cp

cond_dt <- rpart(critical_temp~., data=cond_train, method='anova', control=rpart.control(minbucket=cond_dt_minbucket$best.parameters$minbucket, cp=cond_dt_cp$best.parameters$cp))
fancyRpartPlot(cond_dt)

cond_dt_pred <- data.frame(predict(cond_dt, cond_test[,-82]))
colnames(cond_dt_pred) <- "s0"
rmse(cond_test$critical_temp, cond_dt_pred$s0)

### Random Forests (RF)  
set.seed(20)
cond_rf <- randomForest(data.matrix(cond_train[,-82]), cond_train$critical_temp, ntree=500, nodesize=5)
cond_rf$mtry

fitControl <- trainControl(method = 'repeatedcv', number=3, repeats=2, search='grid')
tunegrid <- expand.grid(.mtry=seq(25,27,29))
cond_rf <- train(critical_temp~., data=cond_train, method = "rf", trControl=fitControl, tuneGrid=tunegrid)
print(cond_rf)

cond_rf <- randomForest(cond_train[,-82],cond_train$critical_temp, ntree=500, nodesize=5, mtry=25)
varImpPlot(cond_rf)

cond_rf_pred <- data.frame(predict(cond_rf, cond_test[,-82]))
colnames(cond_rf_pred) <- "s0"
rmse(cond_test$critical_temp, cond_rf_pred$s0)

### Stochastic Gradient Boosting (SGB)  
set.seed(15)
fitControl <- trainControl(method='repeatedcv', number=5, repeats=3, verboseIter=FALSE, allowParallel=FALSE)
tunegrid <- expand.grid(interaction.depth=c(1,2,3), shrinkage=c(0.001,0.005,0.01), n.trees=c(50,100,150), n.minobsinnode=10)
garbage <- suppressWarnings(capture.output(
cond_gbm <- train(critical_temp~., data=cond_train, method = "gbm", trControl=fitControl, tuneGrid=tunegrid)))
print(cond_gbm)
plot(cond_gbm)

cond_gbm_pred <- data.frame(predict(cond_gbm, cond_test[,-82]))
colnames(cond_rf_pred) <- "s0"
rmse(cond_test$critical_temp, cond_rf_pred$s0)



