library(ggplot2)
library(dplyr)
library(skmeans)
library(gridExtra)
df<-read.csv("Mall_Customers.csv")
attach(df)
names(df)
view(df)
dim(df)
str(df)
summary(df)
table(df$Gender)
table(df$Age)
head(df)
n_distinct(CustomerID)


as.data.frame(table(Gender))  %>% 
  ggplot(aes(x = Gender, y = Freq))  +
  geom_bar(stat = "identity", fill = "#F8766D") +
  geom_text(y = as.vector(table(Gender)), label = paste0((as.vector(table(Gender))/sum(as.vector(table(Gender))))*100, "%"))


summary(Age)
ggplot(as.data.frame(Age), aes(y = Age)) + geom_boxplot(fill='#F8766D') +theme_gray()

ggplot(df, aes( x = Age, fill = Gender)) + geom_density(alpha = 0.4)+theme_classic()


p1 <- ggplot(as.data.frame(Annual.Income..k..), aes(y = Annual.Income..k..)) + geom_boxplot(fill='#F8766D') + ylim(c(1,150))
p2 <- ggplot(as.data.frame(Spending.Score..1.100.), aes(y = Spending.Score..1.100.)) + geom_boxplot(fill='#00BFC4') + ylim(c(1,150))
grid.arrange(p1, p2, ncol = 2)

test <- df[,c(4,5)]

tot.withinss <- vector("numeric", length = 10)
for (i in 1:10){
  kDet <- kmeans(test, i)
  tot.withinss[i] <- kDet$tot.withinss
}

ggplot(as.data.frame(tot.withinss), aes(x = seq(1,10), y = tot.withinss)) + 
  geom_point(col = "#F8766D") +    
  geom_line(col = "#F8766D") + 
  theme(axis.title.x.bottom = element_blank()) +
  ylab("Within-cluster Sum of Squares") +
  xlab("Number of Clusters") +
  ggtitle("Elbow K Estimation")+theme_gray()


customerClusters <- kmeans(test, 5)
customerClusters


ggplot(test, aes(x = Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(customerClusters$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
  ggtitle("Mall Customer Segmens", subtitle = "K-means Clustering") +theme_light()









