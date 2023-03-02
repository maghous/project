library(tidyverse)
library(PRROC)
library(highcharter)
library(plotly)
library(cowplot)
library(treemap)
library(xgboost)
library(caTools)
library(caret)

df<-read.csv("heart.csv")
attach(df)
names(df)
table(df$sex)
summary(df)

dev.new(width = 550, height = 330, unit = "px")
corrplot(cor(df))
corrplot(cor(df),"number")
corrplot(cor(df),"color")

set.seed(123)

idx<-createDataPartition(df$output,p=0.75,list=FALSE)

train<-df[idx,]
test<-df[-idx,]

drops <- c('output')

X_train = train[ , !(names(train) %in% drops)]                                             
y_train = train$output


X_test = test[ , !(names(test) %in% drops)]
y_test = test$output

negative_cases <- sum(y_train == 0)
postive_cases <- sum(y_train == 1)
cat('train test split')

dtrain <- xgb.DMatrix(data = as.matrix(X_train), label= y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label= y_test)

xgb_model <- xgboost(data = dtrain,       
                     max.depth = 4,
                     nround = 90,
                     early_stopping_rounds = 3,
                     objective = "binary:logistic",
                     scale_pos_weight = negative_cases/postive_cases,
                     gamma = 1
)

pred <- predict(xgb_model, dtest)
xgbpred <- ifelse(pred > 0.50, 1, 0)

err <- mean(xgbpred != y_test)
print(paste("test-error=", err))


confusionMatrix(as.factor(xgbpred), as.factor(y_test))


pr_curve <- pr.curve(scores.class0 = xgbpred, weights.class0=y_test, curve=TRUE)
#pr_curve

roc_curve <- roc.curve(scores.class0 = xgbpred, weights.class0=y_test, curve=TRUE)
#roc_curve

par(mfrow=c(1,2))
plot(roc_curve, col=2, bty="n")
plot(pr_curve, col=2, bty="n")

mat <- xgb.importance(feature_names = colnames(y_train), model = xgb_model)
mat

xgb.plot.importance(mat,col=3)


