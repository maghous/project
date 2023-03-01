library(tidyverse)
library(gridExtra)
library(caret)
library(ROCR)
library(party)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(corrplot)
library(e1071)
df<-read.csv('avocado.csv')
names(df)
table(df$region)
table(df$type)
sum(is.null(df))
df$year <- as.factor(df$year)
summary(df)

dev.new(width = 550, height = 330, unit = "px")
corrplot(cor(df[,3:10]))
corrplot(cor(df[,3:10]),"number")

#new colonne pour mois

df$month <- format(as.Date(df$Date), "%B")
#avocado_df

# add month number like 01, 02...12
df$month_number <- format(as.Date(df$Date), "%m")


df <-df %>% mutate(season = case_when(month_number %in% c("03", "04","05") ~ "Spring",
                                                       month_number %in% c("06","07" ,"08") ~ "Summer",
                                                       month_number %in% c("09","10" ,"11") ~ "Autumn",
                                                       # month_number %in% c("12","01" ,"02") ~ "Winter"
                                                       TRUE ~ "Winter"))


#changer le type 
df$avocado_type <- ifelse(df$type == "conventional", 0, 1)
tail(df)


df$type <- as.factor(df$type)
df$season <- as.factor(df$season)
df$avocado_type <- as.factor(df$avocado_type)

str(df)



#comparaison en les models de classifications 

set.seed(123)
idx<-sample(2,nrow(df),prob = c(0.7,0.3),replace = T)
train <- df[idx==1,]
test<-df[idx==2,]

dim(train)
dim(test)

#logistic regression 

mod_glm<-glm(type ~.,data=train[,3:12],family ="binomial")
summary(mod_glm)

pred_proba <- predict(mod_glm, test, type = "response")
test$pred_type <- ifelse(pred_proba > 0.5,  "organic", "conventional")
test$pred_type <- as.factor(test$pred_type)
confusionMatrix(test$pred_type, test$type, positive = "organic")


test$pred_type1 <- ifelse(pred_proba > 0.3,  "organic", "conventional")
#Convert to factor
test$pred_type1 <- as.factor(test$pred_type1)
#test
confusionMatrix(test$pred_type1, test$type, positive = "organic")


test$pred_type1 <- ifelse(pred_proba > 0.6,  "organic", "conventional")
#Convert to factor
test$pred_type1 <- as.factor(test$pred_type1)
#test
confusionMatrix(test$pred_type1, test$type, positive = "organic")


test$pred_type1 <- ifelse(pred_proba > 0.7,  "organic", "conventional")
#Convert to factor
test$pred_type1 <- as.factor(test$pred_type1)
#test
confusionMatrix(test$pred_type1, test$type, positive = "organic")



ROCRpred <- prediction(pred_proba,test$type)
ROCRpred
ROCRperf1 <- performance(ROCRpred,"tpr","fpr")
ROCRperf1

plot(ROCRperf1, colorize=TRUE, lwd = 2)
# add line diagonaly
lines(x = c(0, 1), y = c(0, 1), col = "black", lwd = 1)


auc <- performance(ROCRpred, measure = "auc")
auc
auc <- auc@y.values[[1]] %>% print

#Decision tree 


tree_model <- ctree(type ~.,data=train[,3:12])
plot(tree_model)

test_predict <- predict(tree_model, test)

confusionMatrix(test_predict, test$type, positive = "organic")


a<- 1-unlist(treeresponse(tree_model, test), use.names=F)[seq(1, nrow(test)*2,2)]

ROCRpred <- prediction(a,test$type)
ROCRperf2 <- performance(ROCRpred,"tpr","fpr")

plot(ROCRperf2, colorize=TRUE, lwd = 2)
# add line diagonaly
lines(x = c(0, 1), y = c(0, 1), col = "black", lwd = 1)

auc <- performance(ROCRpred, measure = "auc")
auc
auc <- auc@y.values[[1]] %>% print


tree_model1 <- rpart(type ~.,data=train[,3:12],method = "class")
rpart.plot(tree_model1, extra = 4)
p <- predict(tree_model1, test, type = "class")
confusionMatrix(p, test$type, positive = "organic")

prob_predict <- predict(tree_model1, test, type = "prob")[,2]
ROCRpred <- prediction(prob_predict,test$type)
ROCRperf3 <- performance(ROCRpred,"tpr","fpr")

plot(ROCRperf3, colorize=TRUE, lwd = 2)
# add line diagonaly
lines(x = c(0, 1), y = c(0, 1), col = "black", lwd = 1)


auc <- performance(ROCRpred, measure = "auc")
auc
auc <- auc@y.values[[1]] %>% print


#random forest

rf_model <- randomForest(type ~.,data=train[,3:12])
plot(rf_model)

importance(rf_model)
varImpPlot(rf_model)


rf_predict <- predict(rf_model, test)


confusionMatrix(rf_predict, test$type, positive = "organic")

prob_p <- predict(rf_model, test, type = "prob")[,2]
ROCRpred <- prediction(prob_p,test$type)
ROCRperf4 <- performance(ROCRpred,"tpr","fpr")

plot(ROCRperf4, colorize=TRUE, lwd = 2)
# add line diagonaly
lines(x = c(0, 1), y = c(0, 1), col = "black", lwd = 1)


auc <- performance(ROCRpred, measure = "auc")
auc
auc <- auc@y.values[[1]] %>% print

#KNN
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)

knn <- train(type ~., 
             data=train[,3:12],
             method = 'knn',
             tuneLength = 20,
             trControl = trControl,
             preProc = c("center", "scale"))

predicti<-predict(knn,test)
confusionMatrix(predicti,test$type,positive = "organic")

p <- predict(knn, test, type = "prob")[,2]
ROCRpred <- prediction(p,test$type)

ROCRperf5 <- performance(ROCRpred,"tpr","fpr")

plot(ROCRperf5, colorize=TRUE, lwd = 2)
lines(x = c(0, 1), y = c(0, 1), col = "black", lwd = 1)

auc <- performance(ROCRpred, measure = "auc")
auc
auc <- auc@y.values[[1]] %>% print


#SVM

svm_model<- svm(type ~., data=train[,3:12], kernel="linear")
summary(svm_model)

plot(svm_model, train, AveragePrice ~ Total.Volume, slice= list(AveragePrice=3, Total_Volume=4))

predict <- predict(svm_model, test)
predict

confusionMatrix(predict, test$type, positive = "organic")



#Neural network
trControl <- trainControl(method = "repeatedcv",
                          #number of folds is 10 by default
                          repeats = 3, 
                          savePredictions = T)


nnFit <- train(type ~., 
               data = train[,3:12],
               method = "avNNet",  
               trControl = trControl, 
               linout = T)

pred<- predict(nnFit, test)

confusionMatrix(pred, test$type, positive = "organic")


p <- predict(nnFit, test, type = "prob")[,2]
#p

ROCRpred <- prediction(p,test$type)

# 2. True Positive and Negative Rate
ROCRperf7 <- performance(ROCRpred,"tpr","fpr")

plot(ROCRperf7, colorize=TRUE, lwd = 2)
# add line diagonaly
lines(x = c(0, 1), y = c(0, 1), col = "black", lwd = 1)

auc <- performance(ROCRpred, measure = "auc")
auc
auc <- auc@y.values[[1]] %>% print


