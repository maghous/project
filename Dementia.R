library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)
library(cowplot)
library(caret)
library(rpart)
df<-read.csv('oasis_cross-sectional.csv')
attach(df)
describe(df)
summary(df)
#on supprime la colonne hand
df<-select(df,-Hand)
df<-select(df,-Delay)
df$SES[is.na(SES)] <- median(df$SES,na.rm=T)
df$MMSE[is.na(MMSE)] <- median(df$MMSE,na.rm=T)
df$CDR[is.na(CDR)] <- median(df$CDR,na.rm=T)
df$Educ[is.na(Educ)] <- median(df$Educ,na.rm=T)


df %>%
  select(ID, Age, CDR, M.F) %>%
  group_by(ID, CDR, M.F) %>%
  summarise_all(funs(min)) %>%
  as.data.frame() %>%
  mutate(CDR = as.factor(CDR)) %>%
  ggplot(aes(x = CDR, y = Age, fill = M.F)) + 
  geom_violin() +
  labs(title = "1. Distribution of Age by CDR rate",
       fill = "Sex") +
  theme_light()



X<-df %>% select(Educ,CDR,M.F)%>% 
  mutate(CDR=as.factor(CDR))%>% 
  ggplot(aes(CDR,Educ))+
  geom_jitter(aes(col=CDR),alpha=0.5)+theme_classic()


Y<-df %>% select(SES,CDR,M.F)%>% 
  mutate(CDR=as.factor(CDR))%>% 
  ggplot(aes(CDR,SES))+
  geom_jitter(aes(col=CDR),alpha=0.5)+theme_classic()

plot_grid(X,Y)

X1<-df %>% select(eTIV,CDR,M.F)%>% 
  mutate(CDR=as.factor(CDR))%>% 
  ggplot(aes(CDR,eTIV))+
  geom_jitter(aes(col=CDR),alpha=0.5)+theme_classic()

Y1<-df %>% select(ASF,CDR,M.F)%>% 
  mutate(CDR=as.factor(CDR))%>% 
  ggplot(aes(CDR,ASF))+
  geom_jitter(aes(col=CDR),alpha=0.5)+theme_classic()

plot_grid(X1,Y1)


