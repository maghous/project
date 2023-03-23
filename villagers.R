library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(dplyr)

data1<-read_csv("villagers.csv")

str(data1)
glimpse(data1)
names(data1)
colSums(is.na(data1))
data1$`Style 1` <- NULL
data1$`Style 2` <- NULL
data1$`Color 1` <- NULL
data1$`Color 2` <- NULL
data1$Personality <- NULL
data1$Birthday <- NULL
data1$Wallpaper <- NULL
data1$Flooring <- NULL
data1$`Furniture List` <- NULL
data1$`Unique Entry ID` <- NULL
data1$Filename <- NULL
data1$Catchphrase <- NULL
glimpse(data1)
Gender<-data1$Gender
freq<-table(Gender)
freq
ggplot(data1,aes(x=Gender))+geom_bar(fill="orange")+theme_light()
table(data1$Species)

ggplot(data1,aes(x=Species))+geom_bar(aes(fill=Gender),position = "dodge")+theme_light()
