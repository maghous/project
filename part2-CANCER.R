library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(pROC)
library(MASS)
library(caTools)
library(caret)
library(caretEnsemble)
df<-read.csv("data.csv")
str(df)
summary(df)
prop.table(table(df$diagnosis))
mat_corr<-cor(df[,3:(ncol(df)-1)])
corrplot(mat_corr, method="circle")
corrplot(mat_corr, method="pie")
corrplot(mat_corr, method="color")
corrplot(mat_corr, method="number")
corrplot(mat_corr, type="upper")
df$diagnosis <- as.factor(df$diagnosis)
df[,33] <- NULL
#modeling
set.seed(123)
index<-createDataPartition(df$diagnosis,p=0.5,list=F)
df_train<-df[index,-1]
df_test<-df[-index,-1]
###PCA
respca<-prcomp(df[,3:ncol(df)],center = T,scale =T )
plot(respca,type="l",col='red')
datapca<-as.data.frame(respca$x)
ggplot(datapca,aes(x=PC1,y=PC2,col=df$diagnosis))+geom_point(alpha=0.5)
####
ggplot(datapca,aes(x=PC1,fill=df$diagnosis))+geom_density(alpha=0.3)
ggplot(datapca,aes(x=PC2,fill=df$diagnosis))+geom_density(alpha=0.3)
#Applying machine learnif models

fit <- trainControl(method="cv",
                           number = 5,
                           preProcOptions = list(thresh = 0.99), # threshold for pca preprocess
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
#Random Forest 
rf<-train(diagnosis~.,df_train,method="ranger",metric="ROC",preProcess=c("center","scale"),trControl=fit)
pred_rf <- predict(rf, df_test)
cm_rf <- confusionMatrix(pred_rf, df_test$diagnosis, positive = "M")
cm_rf
#Random forest pca
pca_rf <- train(diagnosis~.,
                df_train,
                method="ranger",
                metric="ROC",
                preProcess = c('center', 'scale', 'pca'),
                trControl=fit)
pred_pca_rf <- predict(pca_rf, df_test)
cm_pca_rf <- confusionMatrix(pred_pca_rf, df_test$diagnosis, positive = "M")
cm_pca_rf

#KNN
knn <- train(diagnosis~.,
                   df_train,
                   method="knn",
                   metric="ROC",
                   preProcess = c('center', 'scale'),
                   tuneLength=10,
                   trControl=fit)

pred_knn <- predict(knn, df_test)
cm_knn <- confusionMatrix(knn, df_test$diagnosis, positive = "M")
cm_knn

## SVM
svm <- train(diagnosis~.,
                   df_train,
                   method="svmRadial",
                   metric="ROC",
                   preProcess=c('center', 'scale'),
                   trace=FALSE,
                   trControl=fit)

pred_svm <- predict(svm, df_test)
cm_svm <- confusionMatrix(pred_svm, df_test$diagnosis, positive = "M")
cm_svm


#on compare le modèle

model_list <- list(RF=rf, PCA_RF=pca_rf,KNN = knn, SVM=svm)
resamples <- resamples(model_list)

modelcor <- modelCor(resamples)
corrplot(modelcor)
corrplot(modelcor, method="pie")
corrplot(modelcor, method="color")
corrplot(modelcor, method="number")
corrplot(modelcor, type="upper")



