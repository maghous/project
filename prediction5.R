library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(lubridate)
library(tidytext)
library(randomForest)
library(e1071)
library(ggpubr)

df<-read.csv("heart_data.csv")
names(df)

df %>% summarise(n_age = n_distinct(age), n_sex = n_distinct(sex),
                                 n_cp = n_distinct(cp), n_trestbps = n_distinct(trestbps),
                                 n_chol = n_distinct(chol), n_fbs = n_distinct(fbs),
                                 n_restecg = n_distinct(restecg), n_thalach = n_distinct(thalach),
                                 n_exang = n_distinct(exang), n_oldpeak = n_distinct(oldpeak),
                                 n_slope = n_distinct(slope), n_ca = n_distinct(ca), n_thal = n_distinct(thal),
                                 n_condition = n_distinct(condition))


df %>% group_by(age, condition) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(age, count,   fill = as.factor(condition)), stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Age") + labs(fill = "Condition")

df %>% filter(condition == 1) %>% group_by(age, cp) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(age, count,   fill = as.factor(cp)),stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Age") + labs(fill = "Condition") + 
  ggtitle("Age vs. Count (disease only) for various chest pain conditions") +
  scale_fill_manual(values=c("orange", "gray", "green", "pink"))


set.seed(123)
test_index <- createDataPartition(y = df$condition, times = 1, p = 0.2, list= FALSE)
train_set <- df[-test_index, ]
validation <- df[test_index, ]


train_set$condition <- as.factor(train_set$condition)
validation$condition <- as.factor(validation$condition)

#LDA
lda_fit <- train(condition ~ ., method = "lda", data = train_set)
lda_predict <- predict(lda_fit, validation)
confusionMatrix(lda_predict, validation$condition)

#QDA
qda_fit <- train(condition ~ ., method = "qda", data = train_set)
qda_predict <- predict(qda_fit, validation)
confusionMatrix(qda_predict, validation$condition)

#SVM
ctrl <- trainControl(method = "cv", verboseIter = FALSE, number = 5)

grid_svm <- expand.grid(C = c(0.01, 0.1, 1, 10, 20))

svm_fit <- train(condition ~ .,data = train_set,
                 method = "svmLinear", preProcess = c("center","scale"),
                 tuneGrid = grid_svm, trControl = ctrl)

svm_predict <- predict(svm_fit, newdata = validation)
confusionMatrix(svm_predict, validation$condition)

#rANDOFOREST
control<- trainControl(method = "cv", number = 5, verboseIter = FALSE)
grid <-data.frame(mtry = seq(1, 10, 2))
rf_fit <- train(condition ~ ., method = "rf", data = train_set, ntree = 20, trControl = control,
                tuneGrid = grid)
rf_predict <- predict(rf_fit, newdata = validation)
confusionMatrix(rf_predict, validation$condition)


#KNN
ctrl <- trainControl(method = "cv", verboseIter = FALSE, number = 5)
knnFit <- train(condition ~ ., 
                data = train_set, method = "knn", preProcess = c("center","scale"),
                trControl = ctrl , tuneGrid = expand.grid(k = seq(1, 20, 2)))

knnPredict <- predict(knnFit,newdata = validation )
confusionMatrix(knnPredict, validation$condition )





