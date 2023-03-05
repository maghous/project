


df<-read.csv("house_data.csv")
tail(df)
names(df)

df$date<-(substr(df$date, 1, 8))
df$date<- ymd(df$date)
df$date<-as.numeric(as.Date(df$date, origin = "1900-01-01"))




vec_price_sqftliving <-aggregate(price~sqft_living, FUN=mean, data=Training)
plot(vec_price_sqftliving)
scatterplot1<-recordPlot()

linear_model<-lm(vec_price_sqftliving$price~vec_price_sqftliving$sqft_living)



average_price_byBathrooms <-aggregate(price~bathrooms, FUN=mean, data=Training)
plot(average_price_byBathrooms,main="Avg. Price by # Bathroom",lwd=2)
lin_model_bathroom<-lm(price~bathrooms,data=average_price_byBathrooms)
summary(lin_model_bathroom)
abline(lin_model_bathroom,lty=2,col="red")


plot(log(vec_price_sqftliving$sqft_living),log(vec_price_sqftliving$price), main="Log of Sqft_Living vs. Log of Price of House", xlab="Log Sqft_Living", ylab="Log Price of House", pch=19,col="red")
scatterplot2<-recordPlot()


scatterplot1 ## Regular Sqft Living vs Price Graph
scatterplot2 ## Log of Sqft living vs Log of Price Graph


Model1 <- lm(data=Training,price~sqft_living)

Beta0_Model1<-coef(Model1)[1]
Beta1_Model1<-coef(Model1)[2]
R_Squared_Model1<-summary(Model1)$r.squared

cat("Model1 coefficients and R-Squared:\nBeta0:",Beta0_Model1,"\nBeta1:",Beta1_Model1,"\nR-squared:",R_Squared_Model1)



plot(Training$bedroom,log(Training$price), main="Bedrooms vs. Log Price of House", xlab="Bedrooms", ylab="Log Price of House", pch=19,col=2)
plot(log(Training$bedroom),log(Training$price), main="Log Bedrooms vs. Log Price of House", xlab="Log Bedrooms", ylab="Log Price of House", pch=19,col=3)


plot(Training$bathrooms,log(Training$price), main="Bathrooms vs. Log Price of House", xlab="Bathrooms", ylab="Log Price of House", pch=19,col="red")
plot(log(Training$bathrooms),log(Training$price), main="Log Bathrooms vs. Log Price of House", xlab="Log Bathrooms", ylab="Log Price of House", pch=19,col="green")




