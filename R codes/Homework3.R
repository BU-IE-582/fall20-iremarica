# Homework 3
# libraries
library(readr, warn.conflicts=FALSE)
library(Metrics, warn.conflicts=FALSE)
library(Matrix, warn.conflicts=FALSE)
library(data.table, warn.conflicts=FALSE)
library(glmnet, warn.conflicts=FALSE)
library(dplyr, warn.conflicts=FALSE)

# reading the data
dataset <- read.csv2("~/Desktop/Homework3/GercekZamanliTuketim-01012016-30112020.csv", stringsAsFactors=FALSE)
colnames(dataset) <- c("Date", "Hour", "Consumption")
str(dataset)

dataset$Date <- as.Date(dataset$Date, format="%d.%m.%Y")
dataset$Hour <- as.numeric(substr(dataset$Hour, 1, 2))
dataset$Consumption <- as.numeric(sub(",", ".", dataset$Consumption, fixed = T))
summary(dataset)

# Part A
dataset$Lag_48 <- lag(dataset$Consumption, 48)
dataset$Lag_168 <- lag(dataset$Consumption, 168)
naive_test <- dataset[which(dataset$Date >= "2020-11-01"),]

for (i in 1:nrow(naive_test)) {
    naive_test$mape_Lag_48[i] <- mape(naive_test$Consumption[i], naive_test$Lag_48[i])
}
for (i in 1:nrow(naive_test)) {
    naive_test$mape_Lag_168[i] <- mape(naive_test$Consumption[i], naive_test$Lag_168[i])
}
mape(naive_test$Consumption, naive_test$Lag_48)
mape(naive_test$Consumption, naive_test$Lag_168)
cat("MAPE results of Naive Approach with Lag_48 values:", "\n")
summary(naive_test$mape_Lag_48)
cat("MAPE results of Naive Approach with Lag_48 values:", "\n")
summary(naive_test$mape_Lag_168)

# Part B
dataset_long <- dataset[complete.cases(dataset),]
for (i in 0:23){
    print(sum(dataset_long$Hour==i))
}

linear_dataset <- dataset_long[-which(dataset_long$Date=="2016-03-27"),]
linear_train <- linear_dataset[which(linear_dataset$Date < "2020-11-01"),]
linear_test <- linear_dataset[which(linear_dataset$Date >= "2020-11-01"),]

lm_fit <- lm(Consumption ~ Lag_48 + Lag_168, data=linear_train)
summary(lm_fit)
linear_prediction <- as.data.frame(predict(lm_fit, linear_test))
colnames(linear_prediction) <- c("Prediction")
linear_test <- cbind(linear_test, linear_prediction)

for (i in 1:nrow(linear_test)) {
    linear_test$mape_linear[i] <- mape(linear_test$Consumption[i], linear_test$Prediction[i])
}
mape(linear_test$Consumption, linear_test$Prediction)
summary(linear_test$mape_linear)

# Part C
test_hourly <- c()
for(i in 0:23){
    train_hour <- linear_train[which(linear_train$Hour==i),]
    test_hour <- linear_test[which(linear_test$Hour==i),]
    lm_hour <- lm(Consumption ~ Lag_48 + Lag_168, data = train_hour)
    prediction_hour <- as.data.frame(predict(lm_hour, test_hour))
    colnames(prediction_hour) <- "Prediction_Hour"
    test_hour <- cbind(test_hour, prediction_hour)
    test_hourly <- rbind(test_hourly, test_hour)
}

for (i in 1:nrow(test_hourly)) {
    test_hourly$mape_hourly[i] <- mape(test_hourly$Consumption[i], test_hourly$Prediction_Hour[i])
}
mape(test_hourly$Consumption, test_hourly$Prediction_Hour)
summary(test_hourly$mape_hourly)

# Part D
dataset_wide_Lag_48 <- dcast(linear_dataset, Date ~ Hour, value.var="Lag_48")
colnames(dataset_wide_Lag_48) <- c("Date", paste0("Lag_day2_hour_",0:23))
dataset_wide_Lag_168 <- dcast(linear_dataset, Date ~ Hour, value.var="Lag_168")
colnames(dataset_wide_Lag_168) <- c("Date", paste0("Lag_day7_hour_",0:23))
dataset_wide_Consumption <- dcast(linear_dataset, Date ~ Hour, value.var="Consumption")
colnames(dataset_wide_Consumption) <- c("Date", paste0("Consumption_hour_",0:23))
dataset_wide <- cbind(dataset_wide_Consumption, dataset_wide_Lag_48[,-1], dataset_wide_Lag_168[,-1])
cv_dataset <- cbind(dataset_wide_Lag_48, dataset_wide_Lag_168[,-1])

cv_train_full <- dataset_wide[which(dataset_wide$Date < "2020-11-01"),]
cv_test_full <- dataset_wide[which(dataset_wide$Date >= "2020-11-01"),]
cv_train <- cv_dataset[which(cv_dataset$Date < "2020-11-01"),]
cv_test <- cv_dataset[which(cv_dataset$Date >= "2020-11-01"),]

set.seed(1)
lambda <- c()
for(i in 0:23){
    cv_fit <- cv.glmnet(as.matrix(cv_train[,-1]), cv_train_full[,i+2], family="gaussian", nfolds=10)
    lambda <- c(lambda, cv_fit$lambda.min)
    lasso <- glmnet(as.matrix(cv_train[,-1]), cv_train_full[,i+2], family="gaussian", lambda=lambda[i+1])
    prediction_lasso <- as.data.frame(predict(lasso, as.matrix(cv_test[,-1])))
    colnames(prediction_lasso) <- paste0("Prediction_lasso_", i)
    cv_test_full <- cbind(cv_test_full, prediction_lasso)
}

lambda

for (i in 1:nrow(cv_test_full)){
    cv_test_full$mape_hour_0[i] <- mape(cv_test_full[i,2], cv_test_full[i,74])
    cv_test_full$mape_hour_1[i] <- mape(cv_test_full[i,3], cv_test_full[i,75])    
    cv_test_full$mape_hour_2[i] <- mape(cv_test_full[i,4], cv_test_full[i,76])    
    cv_test_full$mape_hour_3[i] <- mape(cv_test_full[i,5], cv_test_full[i,77])    
    cv_test_full$mape_hour_4[i] <- mape(cv_test_full[i,6], cv_test_full[i,78])    
    cv_test_full$mape_hour_5[i] <- mape(cv_test_full[i,7], cv_test_full[i,79])    
    cv_test_full$mape_hour_6[i] <- mape(cv_test_full[i,8], cv_test_full[i,80])    
    cv_test_full$mape_hour_7[i] <- mape(cv_test_full[i,9], cv_test_full[i,81])    
    cv_test_full$mape_hour_8[i] <- mape(cv_test_full[i,10], cv_test_full[i,82])    
    cv_test_full$mape_hour_9[i] <- mape(cv_test_full[i,11], cv_test_full[i,83])    
    cv_test_full$mape_hour_10[i] <- mape(cv_test_full[i,12], cv_test_full[i,84])    
    cv_test_full$mape_hour_11[i] <- mape(cv_test_full[i,13], cv_test_full[i,85])    
    cv_test_full$mape_hour_12[i] <- mape(cv_test_full[i,14], cv_test_full[i,86])    
    cv_test_full$mape_hour_13[i] <- mape(cv_test_full[i,15], cv_test_full[i,87])    
    cv_test_full$mape_hour_14[i] <- mape(cv_test_full[i,16], cv_test_full[i,88])
    cv_test_full$mape_hour_15[i] <- mape(cv_test_full[i,17], cv_test_full[i,89])    
    cv_test_full$mape_hour_16[i] <- mape(cv_test_full[i,18], cv_test_full[i,90])    
    cv_test_full$mape_hour_17[i] <- mape(cv_test_full[i,19], cv_test_full[i,91])    
    cv_test_full$mape_hour_18[i] <- mape(cv_test_full[i,20], cv_test_full[i,92])    
    cv_test_full$mape_hour_19[i] <- mape(cv_test_full[i,21], cv_test_full[i,93])    
    cv_test_full$mape_hour_20[i] <- mape(cv_test_full[i,22], cv_test_full[i,94])    
    cv_test_full$mape_hour_21[i] <- mape(cv_test_full[i,23], cv_test_full[i,95])    
    cv_test_full$mape_hour_22[i] <- mape(cv_test_full[i,24], cv_test_full[i,96])    
    cv_test_full$mape_hour_23[i] <- mape(cv_test_full[i,25], cv_test_full[i,97])
}

for(i in 0:23){
    cat( "\n", "The MAPE results for Hour ", i, "\n")
    print(summary(cv_test_full[,i+98]))
}

cv_mape_full <- c(cv_test_full$mape_hour_0,cv_test_full$mape_hour_1,cv_test_full$mape_hour_2,
                  cv_test_full$mape_hour_3,cv_test_full$mape_hour_4,cv_test_full$mape_hour_5,
                  cv_test_full$mape_hour_6,cv_test_full$mape_hour_7,cv_test_full$mape_hour_8,
                  cv_test_full$mape_hour_9,cv_test_full$mape_hour_10,cv_test_full$mape_hour_11,
                  cv_test_full$mape_hour_12,cv_test_full$mape_hour_13,cv_test_full$mape_hour_14,
                  cv_test_full$mape_hour_15,cv_test_full$mape_hour_16,cv_test_full$mape_hour_17,
                  cv_test_full$mape_hour_18,cv_test_full$mape_hour_19,cv_test_full$mape_hour_20,
                  cv_test_full$mape_hour_21,cv_test_full$mape_hour_22,cv_test_full$mape_hour_23)
summary(cv_mape_full)

# Part F
boxplot(naive_test$mape_Lag_48, naive_test$mape_Lag_168, linear_test$mape_linear,
        test_hourly$mape_hourly, cv_mape_full, names=c("Lag_14", "Lag_168", "Part B", 
                                                       "Part C", "Part D"))
