library(ggplot2)
library(MASS)
library(dplyr)
library(Hmisc)
library(cowplot)
library(WVPlots)
df<-read.csv("insurance (1).csv")
attach(df)
head(df)
summary(df)
#graphe 1
x <- ggplot(df, aes(sex, charges)) +
  geom_jitter(aes(color=sex), alpha = 0.5) +
  theme_light()

y <- ggplot(df, aes(children, charges)) +
  geom_jitter(aes(color=children), alpha = 0.5) +
  theme_light()

plot_grid(x,y)

#graphe 2

x <- ggplot(df, aes(age, charges)) +
  geom_jitter(color = "red", alpha = 0.5) +
  theme_light()

y <- ggplot(df, aes(bmi, charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()
plot_grid(x,y)


#graphe 3

x <- ggplot(df, aes(smoker, charges)) +
  geom_jitter(aes(color=smoker), alpha = 0.5) +
  theme_light()

y <- ggplot(df, aes(region, charges)) +
  geom_jitter(aes(color=region), alpha = 0.5) +
  theme_light()

plot_grid(x,y)

#regression
df_train<-df[sample(1:nrow(df),round(0.8*nrow(df))),]
df_test<-df[-sample(1:nrow(df),round(0.8*nrow(df))),]

model<-lm(charges~.,data=df_train)
z=summary(model)
print(z)
#r carré
rcarre<-z$r.squared
#prediction au test
predict(model,newdata = df_test)
#qsm
res<-df_test$charges - predict(model,newdata = df_test)
print(res)
sqrt(mean(res^2))

#new model
model1<-lm(charges~age+bmi+children+smoker+region,data=df_train)
zz<-summary(model1)
print(zz)

#r carré
rcarre1<-zz$r.squared
#prediction au test
predict(model1,newdata = df_test)
#qsm
res<-df_test$charges - predict(model1,newdata = df_test)
print(res)
sqrt(mean(res^2))

#comparaison entre les modeles
print(paste0("R^2 pour le premier modèle:",round(rcarre,7)))
print(paste0("R^2 pour le deuxième modèle:",round(rcarre1,7)))




