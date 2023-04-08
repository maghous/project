library(tidyverse)
library(lattice)
library(ggplot2)
library(car)
library(corrplot)
library(corrgram)
df<-read.csv("kc_house_data.csv")
attach(df)
dim(df)
class(df)
str(df)
df$date <- as.Date(as.Date(as.character(df$date),"%Y%m%d"))
table(is.na(df))
df$age<-as.numeric(format(df$date,"%Y")) - df$yr_built
df$renage <- as.numeric(format(df$date, "%Y")) - df$yr_renovated

table(is.na(df$renage))
table(df$waterfront)
table(df$view)
table(df$bathrooms)
table(df$age)

df$rate <- df$price/df$sqft_living
str(df)
summary(df)
bwplot(~bedrooms,data=df)
bwplot(~price,data=df)
bwplot(~condition,data=df)
bwplot(bedrooms~condition,data=df,horizontal = T)
bwplot(bedrooms~price,data=df,horizontal = T)
bwplot(bathrooms~price, data = df)
bwplot(sqft_living ~price ,data=df)
bwplot(bathrooms~price,data=df)
bwplot(floors ~ price,data=df)
bwplot(waterfront ~ price,data=df,horizontal = T)
bwplot(grade~price,data=df)

densityplot(df$price)
hist(df$price,breaks = 100,col = "red",xlab = "Price")
ggplot(df,aes(df$price))+stat_bin(bins = 100, colour="red", fill="green") + labs(x= "Price",y= "Frequency" , title = "Histogram of Price") + xlim(0,4000000) + scale_fill_discrete()+theme_light()
ggplot(df, aes(df$sqft_living)) + stat_bin(bins = 100, colour="red", fill="green") + labs(x= "square feet living",y= "Frequency" , title = "Histogram of sqft_living") + scale_fill_discrete()+theme_light()
ggplot(df, aes(df$sqft_lot)) + stat_bin(bins = 100, colour="red", fill="green")+theme_light()
qplot(df$view, main = "Plot of House View" , xlab = "View" , ylab = "Frequency")
qplot(df$floors)

scatterplot(x =df$price, y=df$bedrooms)       
scatterplot(x =df$price, y=df$sqft_above)

ggplot(data =df, mapping = aes(x = sqft_living, y = price)) + geom_point(colour = 'skyblue') + geom_smooth(method = 'lm')
hpricecor <- df[ ,c(3:14,17,22,23)]
cor(hpricecor)
corrplot(cor(hpricecor))
corrgram(hpricecor, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

cor.test(df$price,df$sqft_living)
t.test(df$price,df$sqft_living)
t.test(df$price,df$bathrooms)
t.test(df$price,df$sqft_above)
t.test(df$price,df$grade)
t.test(df$price,df$bedrooms)
t.test(df$price,df$sqft_basement)

df$bedrooms <- as.factor(df$bedrooms)
df$bathrooms <- as.factor(df$bathrooms)
df$grade <- as.factor(df$grade)
df$zipcode <- as.factor(df$zipcode)

str(df)

model1 <- lm(price~ sqft_living + bedrooms + bathrooms + grade + sqft_above,data =df)
summary(model1)

model1A <- lm(price~ sqft_living + bedrooms + bathrooms + grade + sqft_above + zipcode,data = df)
summary(model1A)

plot(model1A)

price_predict <- predict(model1A)
Prediction <-cbind(df$price, price_predict)

ggplot(data = df, mapping = aes(x = zipcode, y = price)) + geom_boxplot()
ggplot(data = df, mapping = aes(x = zipcode, y = price)) + geom_boxplot()
