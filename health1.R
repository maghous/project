library(tidyverse)
library(corrplot)
df<-read.csv("fetal_health.csv")
attach(df)
view(df)
names(df)
tail(df)
str(df)
summary(df)

x<-'fetal_health'
df[,x] <- as.factor(df[,x])
plot(df[,x],col="red",main="Fetal healt")

features <- setdiff(colnames(df),"fetal_health")
features

options(repr.plot.width=15,repr.plot.height=9)
for (i in features) {
  hist(df[,i],main=i,col="red")
}

dev.new(width = 550, height = 330, unit = "px")
corrplot(cor(df[,features]))
corrplot(cor(df),"number")
corrplot(cor(df),"color")


a=seq(0,1,0.1)
for (i in features) {
  aa<-unique(quantile(df[,i],a))
  plot(cut(df[,i], aa, include_lowest=TRUE), df[,x], main=paste0('target vs ',i))
}


df[df$severe_decelerations !=0,]



perc_train <- 0.7
n <- nrow(df)
# add artificial row id
df$id <- 1:n

set.seed(142)
df_train <- dplyr::sample_frac(df, perc_train)
idx_train <- df_train$id
df_test <- df[-idx_train,]

options(repr.plot.width = 8, repr.plot.height = 4) # => smaller plots for the following
plot(df[,x], main='Target distribution - Overall')
plot(df_train[,x], main='Target distribution - Training')
plot(df_test[,x], main='Target distribution - Test')

summary(as.factor(df_train$severe_decelerations))



















