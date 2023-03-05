library(tidyverse)
library(ggplot2)
library(e1071)



df<-read.csv("house_data.csv")
tail(df)

print(paste("nombre de ligne",dim(df)[1]))
print(paste("nombre de colonne",dim(df)[2]))

names(df)
str(df)
summary(df)

table(df$city)
table(df$bedrooms)
table(df$bathrooms)
table(df$floors)

new_df <- df[,c("price","bedrooms","sqft_living","floors",
                "sqft_lot", "condition", "view", "yr_built")]

tail(df)

sum(is.na(new_df))


new_df$oldbuilt <- as.integer(format(Sys.Date(), "%Y")) - new_df$yr_built


drops <- c("yr_built")
new_df = new_df[ , !(names(new_df) %in% drops)]
attach(new_df)

dev.new(width = 550, height = 330, unit = "px")
corrplot(cor(new_df))
corrplot(cor(new_df),"number")
corrplot(cor(new_df),"color")

pairs(~bedrooms + sqft_living + floors + condition, data = new_df,
      main = "Scatterplot Matrix")


par(mfrow=c(2, 3))  
boxplot(new_df$bedrooms, main="Bedrooms",col=2)
boxplot(new_df$sqft_living, main="sqft_living",col=2)
boxplot(new_df$floors, main="floors",col=2)
boxplot(new_df$condition, main="condition",col=2)
boxplot(new_df$view, main="view",col=2)
boxplot(new_df$oldbuilt, main="oldbuilt",col=2)


ggplot(new_df,aes(bedrooms,floors)) + geom_count(col="orange")+theme_light()+labs(title = "Bedrooms contre floors")

plot(new_df$sqft_living,new_df$sqft_lot,
     xlab="sqft_living",ylab="sqft_lot"
     ,xlim=c(0,3000),ylim=c(0,20000),main="sqft living contre sqft lot",col="red")



par(mfrow=c(2,3))
plot(density(new_df$bedrooms), main="Density Plot: Bedrooms", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(new_df$bedrooms), 2)))  
polygon(density(new_df$bedrooms), col=2)

plot(density(new_df$sqft_living), main="Density Plot: sqft_living", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(new_df$sqft_living), 2)))  
polygon(density(new_df$sqft_living), col=2)

plot(density(new_df$sqft_lot), main="Density Plot: sqft_lot", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(new_df$sqft_lot), 2)))  
polygon(density(new_df$sqft_lot), col=3)

plot(density(new_df$condition), main="Density Plot: condition", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(new_df$condition), 2)))  
polygon(density(new_df$condition), col=2)

plot(density(new_df$floors), main="Density Plot: floors", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(new_df$floors), 2)))  
polygon(density(new_df$floors), col=3)

plot(density(new_df$oldbuilt), main="Density Plot: oldbuilt", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(new_df$oldbuilt), 2)))  
polygon(density(new_df$oldbuilt), col=2)


modele=lm(price ~sqft_living,data = new_df)
coeff=modele$coefficients
plot(sqft_living,price,ylim=c(0,30*10^5),col="orange")
lines(sqft_living,sqft_living *coeff[2]+coeff[1],col="green")


linearmodel = lm(price~bedrooms + sqft_living + floors + sqft_lot + condition + view + oldbuilt,
                 data = new_df)
summary(linearmodel)

