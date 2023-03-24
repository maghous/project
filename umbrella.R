library(tidyverse)
library(skimr)
library(janitor)
data<-read_csv("umbrellas.csv")
attach(data)
view(data)
names(data)
tail(data)
str(data)
glimpse(data)
skim_without_charts(data)

ggplot(data=data)+geom_bar(mapping = aes(x=DIY),fill="red")+theme_classic()
ggplot(data=data)+geom_bar(mapping = aes(x=DIY),fill="red")+theme_bw()

ggplot(data = data) + geom_bar(mapping = aes(x = DIY, fill=Buy))
ggplot(data = data) + geom_bar(mapping = aes(x = DIY, fill=Buy))+theme_classic()

ggplot(data = data) + geom_bar(mapping = aes(x = DIY, fill=Catalog))
ggplot(data = data) + geom_bar(mapping = aes(x = DIY, fill=Catalog))+theme_classic()
ggplot(data = data) + geom_bar(mapping = aes(x = DIY, fill=Catalog))+theme_bw()


ggplot(data = data) + geom_bar(mapping = aes(x = DIY), fill="orange")+facet_wrap(~Buy)
ggplot(data = data) + geom_bar(mapping = aes(x = DIY), fill="orange")+facet_wrap(~Buy)+theme_classic()
ggplot(data = data) + geom_bar(mapping = aes(x = DIY), fill="orange")+facet_wrap(~Buy)+theme_bw()


ggplot(data=data)+geom_bar(mapping = aes(x=DIY),fill="green")+facet_grid(~Catalog)
ggplot(data=data)+geom_bar(mapping = aes(x=DIY),fill="green")+facet_grid(~Catalog)+theme_classic()
ggplot(data=data)+geom_bar(mapping = aes(x=DIY),fill="green")+facet_grid(~Catalog)+theme_bw()

ggplot(data=data)+geom_bar(mapping = aes(x=DIY),fill="red")+facet_grid(~Buy)
ggplot(data=data)+geom_bar(mapping = aes(x=DIY),fill="red")+facet_grid(~Buy)+theme_classic()
ggplot(data=data)+geom_bar(mapping = aes(x=DIY),fill="red")+facet_grid(~Buy)+theme_bw()

ggplot(data=data)+geom_bar(mapping = aes(x=DIY),fill="black")+facet_grid(~Sell)
ggplot(data=data)+geom_bar(mapping = aes(x=DIY),fill="black")+facet_grid(~Sell)+theme_bw()
ggplot(data=data)+geom_bar(mapping = aes(x=DIY),fill="black")+facet_grid(~Sell)+theme_classic()

ggplot(data = data) + geom_bar(mapping = aes(x = DIY),fill='blue') + facet_wrap(~Buy~Catalog)
ggplot(data = data) + geom_bar(mapping = aes(x = DIY),fill='blue') + facet_wrap(~Buy~Catalog)+theme_bw()
ggplot(data = data) + geom_bar(mapping = aes(x = DIY),fill='blue') + facet_wrap(~Buy~Catalog)+theme_classic()

ggplot(data = data) + geom_bar(mapping = aes(x = Size),fill="pink") + facet_grid(~Buy)
ggplot(data = data) + geom_bar(mapping = aes(x = Size),fill="pink") + facet_grid(~Buy)+theme_bw()
ggplot(data = data) + geom_bar(mapping = aes(x = Size),fill="pink") + facet_grid(~Buy)+theme_classic()








