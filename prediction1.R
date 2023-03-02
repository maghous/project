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
library(stepaic)
library(MASS)

df<-read.csv("german_bank.csv",header=T)
attach(df)
dim(df)
df<-df[,2:32]
names(df)
summary(df)

dev.new(width = 550, height = 330, unit = "px")
corrplot(cor(df))
corrplot(cor(df),"number")
corrplot(cor(df),"color")

str(df)


#comparaison en les models de classifications 

set.seed(123)
idx<-sample(2,nrow(df),prob = c(0.7,0.3),replace = T)
train <- df[idx==1,]
test<-df[idx==2,]

dim(train)
dim(test)


#logistic regression 

df$RESPONSE <- as.factor(df$RESPONSE)


mod_glm<-glm(RESPONSE ~.,data=train,family ="binomial")
summary(mod_glm)

stepAIC(mod_glm)

#new_mod_glm<-glm(formula = RESPONSE ~ DURATION + AMOUNT  + AGE +
#                   MALE_SINGLE + GUARANTOR + PROP_UNKN_NONE , family = "binomial", 
#                data = train)
new_mod_glm<-glm(formula = RESPONSE ~ CHK_ACCT + DURATION + HISTORY + NEW_CAR + 
                   USED_CAR + EDUCATION + AMOUNT + SAV_ACCT + INSTALL_RATE + 
                   MALE_SINGLE + GUARANTOR + PROP_UNKN_NONE + AGE + OTHER_INSTALL + 
                   NUM_CREDITS + NUM_DEPENDENTS + FOREIGN, family = "binomial", 
                 data = train)

pred_proba <- predict(new_mod_glm, test, type = "response")
test$pred_type <- ifelse(pred_proba > 0.5,  "0", "1")
test$pred_type <- as.factor(test$pred_type)
confusionMatrix(test$pred_type, as.factor(test$RESPONSE), positive = "0")

test$pred_type1 <- ifelse(pred_proba > 0.3,  "0", "1")
#Convert to factor
test$pred_type1 <- as.factor(test$pred_type1)
#test
confusionMatrix(test$pred_type1, as.factor(test$RESPONSE), positive = "0")



test$pred_type1 <- ifelse(pred_proba > 0.6,  "0", "1")
#Convert to factor
test$pred_type1 <- as.factor(test$pred_type1)
#test
confusionMatrix(test$pred_type1, as.factor(test$RESPONSE), positive = "0")


test$pred_type1 <- ifelse(pred_proba > 0.9,  "0", "1")
#Convert to factor
test$pred_type1 <- as.factor(test$pred_type1)
#test
confusionMatrix(test$pred_type1, as.factor(test$RESPONSE), positive = "0")


ROCRpred <- prediction(pred_proba,test$RESPONSE)
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


tree_model <- ctree(formula = RESPONSE ~ CHK_ACCT + DURATION + HISTORY + NEW_CAR + 
                      USED_CAR + EDUCATION + AMOUNT + SAV_ACCT + INSTALL_RATE + 
                      MALE_SINGLE + GUARANTOR + PROP_UNKN_NONE + AGE + OTHER_INSTALL + 
                      NUM_CREDITS + NUM_DEPENDENTS + FOREIGN, 
                    data = train)

plot(tree_model)

test_predict <- predict(tree_model, test)
confusionMatrix(test_predict, as.factor(test$RESPONSE), positive = "0")

a<- 1-unlist(treeresponse(tree_model, test), use.names=F)[seq(1, nrow(test)*2,2)]

aa<-rep(0,295)
aa[1:148]=a[1:148]
ROCRpred <- prediction(aa,test$RESPONSE)
ROCRperf2 <- performance(ROCRpred,"tpr","fpr")

plot(ROCRperf2, colorize=TRUE, lwd = 2)
# add line diagonaly
lines(x = c(0, 1), y = c(0, 1), col = "black", lwd = 1)

auc <- performance(ROCRpred, measure = "auc")
auc
auc <- auc@y.values[[1]] %>% print


tree_model1 <- rpart(RESPONSE ~ CHK_ACCT + DURATION + HISTORY + NEW_CAR + 
                       USED_CAR + EDUCATION + AMOUNT + SAV_ACCT + INSTALL_RATE + 
                       MALE_SINGLE + GUARANTOR + PROP_UNKN_NONE + AGE + OTHER_INSTALL + 
                       NUM_CREDITS + NUM_DEPENDENTS + FOREIGN,data=train,method = "class")

rpart.plot(tree_model1, extra = 4)
p <- predict(tree_model1, test, type = "class")
confusionMatrix(p, as.factor(test$RESPONSE), positive = "0")


prob_predict <- predict(tree_model1, test, type = "prob")[,2]
ROCRpred <- prediction(prob_predict,test$RESPONSE)
ROCRperf3 <- performance(ROCRpred,"tpr","fpr")

plot(ROCRperf3, colorize=TRUE, lwd = 2)
# add line diagonaly
lines(x = c(0, 1), y = c(0, 1), col = "black", lwd = 1)


auc <- performance(ROCRpred, measure = "auc")
auc
auc <- auc@y.values[[1]] %>% print



#random forest

rf_model <- randomForest(RESPONSE ~ CHK_ACCT + DURATION + HISTORY + NEW_CAR + 
                           USED_CAR + EDUCATION + AMOUNT + SAV_ACCT + INSTALL_RATE + 
                           MALE_SINGLE + GUARANTOR + PROP_UNKN_NONE + AGE + OTHER_INSTALL + 
                           NUM_CREDITS + NUM_DEPENDENTS + FOREIGN,data=train)
plot(rf_model)

importance(rf_model)
varImpPlot(rf_model)

rf_predict <- predict(rf_model, test)

confusionMatrix(rf_predict, as.factor(test$RESPONSE), positive = "0")


prob_p <- predict(rf_model, test, type = "prob")[,2]
ROCRpred <- prediction(prob_p,test$type)
ROCRperf4 <- performance(ROCRpred,"tpr","fpr")


plot(ROCRperf4, colorize=TRUE, lwd = 2)
# add line diagonaly
lines(x = c(0, 1), y = c(0, 1), col = "black", lwd = 1)


auc <- performance(ROCRpred, measure = "auc")
auc
auc <- auc@y.values[[1]] %>% print



trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)

knn <- train(RESPONSE ~., 
             data=train,
             method = 'knn',
             tuneLength = 20,
             trControl = trControl,
             preProc = c("center", "scale"))

predicti<-predict(knn,test)
confusionMatrix(predicti,as.factor(test$RESPONSE),positive = "0")

p <- predict(knn, test, type = "prob")[,2]
ROCRpred <- prediction(p,test$type)

ROCRperf5 <- performance(ROCRpred,"tpr","fpr")

plot(ROCRperf5, colorize=TRUE, lwd = 2)
lines(x = c(0, 1), y = c(0, 1), col = "black", lwd = 1)

auc <- performance(ROCRpred, measure = "auc")
auc
auc <- auc@y.values[[1]] %>% print












