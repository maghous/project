library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(heatmaply)
library(Hmisc)
library(corrplot)
library(GGally)
library(repr)
library(ggrepel)

options(repr.plot.width = 12,repr.plot.height = 10)

df_2019<-read.csv("2019.csv") %>% as.data.frame()
df_2018<-read.csv("2018.csv") %>% as.data.frame()
attach(df_2018)
view(df_2018)
View(df_2019)
names(df_2018)
names(df_2019)

str(df_2018)
str(df_2019)
summary(df_2018)
summary(df_2019)
df_2018$Perceptions.of.corruption <- as.numeric(df_2018$Perceptions.of.corruption)
df_2019$Perceptions.of.corruption <- as.numeric(df_2019$Perceptions.of.corruption)

df_2019$Overall.rank <- NULL

cor_2019 <- cor(df_2019[,-1])
options(repr.plot.width = 12,repr.plot.height = 10)

corrplot(cor_2019, method = "circle")
corrplot(cor_2019, method = "number")

#PCA

fit <- PCA(df_2019[,-(1:2)], scale.unit = TRUE, ncp = 7, graph = FALSE)
(eig <- get_eigenvalue(fit))
fviz_eig(fit, addlabels = TRUE)
fviz_pca_var(fit2, axes = c(1,2) ,col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(fit2, axes = c(2,3) ,col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

      
         




