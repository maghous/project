library(ggplot2)
df.train<-read.csv("train.csv")
df.test<-read.csv("test.csv")
head(df.train)
summary(df.train)
str(df.train)
sample(df.train)
view(df.test)
df.train$IsTrainSet <- T
df.test$IsTrainSet <- F
names(df.train)
names(df.test)
df.test$Survived <- NA
df.full <- rbind(df.train,df.test)
table(df.full$IsTrainSet)
table(df.full$Embarked)
df.full[df.full$Embarked == "",]
table(df.full$Sex,df.full$Embarked)
table(df.full$Pclass,df.full$Embarked)
df.full[df.full$Embarked == "", "Embarked"]<- "S"
table(df.full$Embarked)
table(is.na(df.full$Age))
median(df.full$Age,na.rm=T)
#graphs
ggplot(data=df.full, aes(x=Sex,fill=Survived)) 
 + geom_bar()+
  theme_grey(base_size = 14)


ggplot(data=df.train, aes(x=Pclass,fill=Survived)) + geom_bar()+
  theme_grey(base_size = 14)
