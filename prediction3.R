library(ggplot2)
library(car)
library(dplyr)
library(lattice)
library(tidyr)
library(caret)
library(MASS)
library(broom)
library(ROCR)

df<-read.csv("heart.csv")

df$sex <- as.character(df$sex)
df$sex <- ifelse(df$sex == "0","female","male")

df$cp <- as.factor(df$cp)

summary(df)


ggplot(df,aes(x = age)) + geom_histogram(bins =30,fill ="dodgerblue4") 
+ theme_bw() + theme_classic() +ggtitle("age distribution") +ylab("number of people")

ggplot(df,aes(x = age)) + geom_density(fill ="red") + theme_bw() + theme_classic() +ggtitle("age distribution") +ylab("number of people")
boxplot(heart_data$age,main ="boxplot of age for normality check",col ="dodgerblue4",notch = T)
qqPlot(heart_data$age,main ="normality check for age",grid = F)

ggplot(df,aes(x =sex)) + geom_bar(width = 0.2,fill ="green") + geom_text(stat = 'count',aes(label =..count..),vjust =-0.5) +
  theme_bw() + theme_classic() +ylab("number of count") + ggtitle("sex") 

table(df$cp)

ggplot(df,aes(x = cp)) + geom_bar(width =0.2,fill ="red") 
+ geom_text(stat = 'count',aes(label =..count..),vjust = -0.5)  + theme_bw()+theme_classic() + ggtitle("chest pain type")


set.seed(123)
train.index <- df$output %>% createDataPartition(p=0.8,list=F)
train.data <- df[train.index,]
test.data <- df[-train.index,]

full.mod <- glm(output~.,data =train.data,family =binomial)
summary(full.mod)

prob <- full.mod %>% predict(test.data,type ="response")

predicted.class1 <- ifelse(prob>0.5,1,0)
mean(predicted.class1==test.data$output)


step.model <- full.mod %>% stepAIC(trace = F)
summary(step.model)
prob.step <- step.model %>% predict(test.data,type ="response")
predicted.class2 <- ifelse(prob.step>0.5,1,0) 
mean(predicted.class2==test.data$output)

model_check <- glm(output~.,data =df,family = binomial)
prob.check <- predict(model_check,type ="response")
my_data <- df %>% select_if(is.numeric)
predictors <- colnames(my_data)
my_data <- my_data%>% mutate(logit = log(prob.check/(1-prob.check))) %>%
  gather(key = "predictors",value = "predicted.value",-logit)

ggplot(my_data,aes(x =logit,y =predicted.value)) + geom_point() + geom_smooth(method ="loess") + theme_classic() + theme_bw()+facet_wrap(~predictors,scale ="free_y")

plot(step.model,which=4,id.n =3)

model.data <- augment(step.model) %>% mutate(index =1:n())

model.data %>% top_n(3,.cooksd)

car::vif(step.model)

es <- predict(step.model,type ="response")
ROCR_Pred <- prediction(es,train.data$output)
ROCR_perf <- performance(ROCR_Pred,"tpr","fpr")
plot(ROCR_perf,colorize=T,print.cutoffs.at =seq(0.1,by =0.1))



final.model <- glm(output~.,data =train.data,family =binomial) %>%stepAIC(trash =FALSE)
prob.final <- predict(final.model,test.data,type ="response")
predicted.class_final <- ifelse(prob.final>0.6,1,0)
mean(predicted.class_final==test.data$output)



