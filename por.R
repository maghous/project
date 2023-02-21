library(hrbrthemes)
library(tidyverse)
library(ggridges)
library(ggthemes)
library(cowplot)
library(viridis)
library(GGally)
library(datasets)
library(ggplot2)
data=iris
attach(data)
summary(data)

tema = theme(plot.background = element_rect(fill="#FFDAB9"),
             plot.title = element_text(size=25, hjust=.5),
             axis.title.x = element_text(size=22, color = "black"),
             axis.text.x = element_text(size=20),
             axis.text.y = element_text(size=20))

options(repr.plot.width=14, repr.plot.height=10)

#histogramme 

a<-ggplot(data = data, mapping = aes(x = Sepal.Length)) +
  geom_histogram(bins=30, fill = "red", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Sepal Length") +
  ggtitle("Sepal Length Histogram") +
  tema

b<-ggplot(data = data, mapping = aes(x = Sepal.Width)) +
  geom_histogram(bins=30, fill = "blue", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Sepal Width") +
  ggtitle("Sepal Width Histogram") +
  tema

c<-ggplot(data = data, mapping = aes(x = Petal.Length)) +
  geom_histogram(bins=30, fill = "cyan", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Petal Length") +
  ggtitle("Petal Length Histogram") +
  tema
d<-ggplot(data = data, mapping = aes(x = Petal.Width)) +
  geom_histogram(bins=30, fill = "green", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Petal Width") +
  ggtitle("Petal Width Histogram") +
  tema


plot_grid(a,b,c,d, nrow=2, ncol=2)


#boxplot

a1<-ggplot(data = data, mapping = aes(x = Sepal.Length)) +
  geom_boxplot(fill = "red", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Sepal Length") +
  ggtitle("Sepal Length Histogram") +
  tema

b1<-ggplot(data = data, mapping = aes(x = Sepal.Width)) +
  geom_boxplot(fill = "blue", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Sepal Width") +
  ggtitle("Sepal Width Histogram") +
  tema

c1<-ggplot(data = data, mapping = aes(x = Petal.Length)) +
  geom_boxplot(fill = "cyan", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Petal Length") +
  ggtitle("Petal Length Histogram") +
  tema
d1<-ggplot(data = data, mapping = aes(x = Petal.Width)) +
  geom_boxplot(fill = "green", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Petal Width") +
  ggtitle("Petal Width Histogram") +
  tema

plot_grid(a1,b1,c1,d1, nrow=2, ncol=2)



#density

a2<-ggplot(data = data, mapping = aes(x = Sepal.Length)) +
  geom_density(fill = "red", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Sepal Length") +
  ggtitle("Sepal Length Histogram") +
  tema

b2<-ggplot(data = data, mapping = aes(x = Sepal.Width)) +
  geom_density(fill = "blue", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Sepal Width") +
  ggtitle("Sepal Width Histogram") +
  tema

c2<-ggplot(data = data, mapping = aes(x = Petal.Length)) +
  geom_density(fill = "cyan", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Petal Length") +
  ggtitle("Petal Length Histogram") +
  tema
d2<-ggplot(data = data, mapping = aes(x = Petal.Width)) +
  geom_density(fill = "green", color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Petal Width") +
  ggtitle("Petal Width Histogram") +
  tema

plot_grid(a2,b2,c2,d2, nrow=2, ncol=2)

#density avec les especes

a3<-ggplot(data = data, mapping = aes(x = Sepal.Length)) +
  geom_density(aes(fill =Species), color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Sepal Length") +
  ggtitle("Sepal Length Histogram") +
  tema

b3<-ggplot(data = data, mapping = aes(x = Sepal.Width)) +
  geom_density(aes(fill= Species), color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Sepal Width") +
  ggtitle("Sepal Width Histogram") +
  tema

c3<-ggplot(data = data, mapping = aes(x = Petal.Length)) +
  geom_density(aes(fill =Species), color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Petal Length") +
  ggtitle("Petal Length Histogram") +
  tema
d3<-ggplot(data = data, mapping = aes(x = Petal.Width)) +
  geom_density(aes(fill =Species), color = "black", size = 1.3, alpha = .8) +
  theme_wsj() +
  xlab("Petal Width") +
  ggtitle("Petal Width Histogram") +
  tema

plot_grid(a3,b3,c3,d3, nrow=2, ncol=2)

#VIOLIN 

a4<-ggplot(data = data, mapping = aes(x=Species,y = Sepal.Length,fill=Species)) +
  geom_violin(size=1.5) +
  theme_wsj() +
  xlab("Sepal Length") +
  ggtitle("Sepal Length Histogram") +
  tema

b4<-ggplot(data = data, mapping = aes(x=Species,y = Sepal.Width,fill=Species)) +
  geom_violin(size = 1.5) +
  theme_wsj() +
  xlab("Sepal Width") +
  ggtitle("Sepal Width Histogram") +
  tema

c4<-ggplot(data = data, mapping = aes(x=Species,y = Petal.Length,fill=Species)) +
  geom_violin(size = 1.3) +
  theme_wsj() +
  xlab("Petal Length") +
  ggtitle("Petal Length Histogram") +
  tema
d4<-ggplot(data = data, mapping = aes(x=Species,y = Petal.Width,fill=Species)) +
  geom_violin(size = 1.3) +
  theme_wsj() +
  xlab("Petal Width") +
  ggtitle("Petal Width Histogram") +
  tema

plot_grid(a4,b4,c4,d4, nrow=2, ncol=2)


#VIOLIN AND BOXPLOT


a5<-ggplot(data = data, mapping = aes(x=Species,y = Sepal.Length,fill=Species)) +
  geom_violin(size=1.5) +
  geom_boxplot(width=0.1,color="white")+
  theme_wsj() +
  xlab("Sepal Length") +
  ggtitle("Sepal Length Histogram") +
  tema

b5<-ggplot(data = data, mapping = aes(x=Species,y = Sepal.Width,fill=Species)) +
  geom_violin(size = 1.5) +
  geom_boxplot(width=0.1,color="white")+
  theme_wsj() +
  xlab("Sepal Width") +
  ggtitle("Sepal Width Histogram") +
  tema

c5<-ggplot(data = data, mapping = aes(x=Species,y = Petal.Length,fill=Species)) +
  geom_violin(size = 1.3) +
  geom_boxplot(width=0.1,color="white")+
  theme_wsj() +
  xlab("Petal Length") +
  ggtitle("Petal Length Histogram") +
  tema
d5<-ggplot(data = data, mapping = aes(x=Species,y = Petal.Width,fill=Species)) +
  geom_violin(size = 1.3) +
  geom_boxplot(width=0.1,color="white")+
  theme_wsj() +
  xlab("Petal Width") +
  ggtitle("Petal Width Histogram") +
  tema

plot_grid(a5,b5,d5,d5, nrow=2, ncol=2)


a6 <- ggplot(data = data, aes(x=Sepal.Length, y=Species, fill=Species)) +
  geom_boxplot(size = 1.3) +
  stat_boxplot(geom="errorbar")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_fivethirtyeight() +
  ggtitle("Sepal Length") +
  tema

b6<-sepallength <- ggplot(data = data, aes(x=Sepal.Width, y=Species, fill=Species)) +
  geom_boxplot(size = 1.3) +
  stat_boxplot(geom="errorbar")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_fivethirtyeight() +
  ggtitle("Sepal width") +
  tema

c6<- ggplot(data = data, aes(x=Petal.Length, y=Species, fill=Species)) +
  geom_boxplot(size = 1.3) +
  stat_boxplot(geom="errorbar")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_fivethirtyeight() +
  ggtitle("Petal Length") +
  tema

d6 <- ggplot(data = data, aes(x=Petal.Width, y=Species, fill=Species)) +
  geom_boxplot(size = 1.3) +
  stat_boxplot(geom="errorbar")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_fivethirtyeight() +
  ggtitle("Petal width") +
  tema
plot_grid(a6,b6,c6,d6,nrow=2,ncol=2)


a7<-ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(mapping = aes(color = Species, shape = Species), size = 5) +
  theme_economist() +
  tema

b7<-ggplot(data = data, mapping = aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(mapping = aes(color = Species, shape = Species), size = 5) +
  theme_economist() +
  tema

plot_grid(a7, b7, ncol=2, nrow=1)



ggplot(data = data) +
  geom_point(mapping = aes(x = Sepal.Length, y = Sepal.Width, color=Species, shape=Species), size = 4.5) +
  facet_wrap(~ Species, ncol=3) +
  theme_economist() +
  tema



df <- select(data, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, Species)
ggcorr(df)


ggpairs(df, columns = 2:4, ggplot2::aes(colour=Species)) +
  theme_economist() +
  tema
