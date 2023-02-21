library(ggplot2)
library(kernlab)
library(arules)
library(readxl)
library(readr)
df<-read.csv("StudentsPerformance.csv")
attach(df)
summary(df)
names(df)
str(df)
colnew<-c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")
colnames(df)<-colnew
table(df$Gender)
table(df$Race)
table(df$Parent_Education)
table(df$Lunch)
table(df$Test_Prep)

hist(df$Math_Score,col="orange",main="histogramme de math score")
hist(df$Reading_Score,col=2,main="histogramme de reading score")
hist(df$Writing_Score,col=4,main="histogramme de writing score")




#Math score
ggplot(df,aes(Math_Score))+geom_histogram(bins=50,color="black",aes(fill=Gender))+
  theme_excel()+ggtitle("Math scores by gender")
#reding score 
ggplot(df,aes(Reading_Score))+geom_histogram(bins=50,color="black",aes(fill=Gender))+
  theme_excel()+ggtitle("readin score by gender")
#writing score 
ggplot(df,aes(Writing_Score))+geom_histogram(bins=50,color="black",aes(fill=Gender))+
  theme_excel()+ggtitle("Math scores by gender")

#boxplot 
ggplot(df, aes(Gender, Writing_Score, fill = Test_Prep)) + geom_boxplot() + ggtitle("Writing scores by Gender Boxplot") + xlab("Gender") + ylab("Writing Scores")


ggplot(df, aes(Gender, Math_Score, fill = Test_Prep)) + geom_boxplot() + ggtitle("math scores by Gender Boxplot") + xlab("Gender") + ylab("math Scores")


ggplot(df, aes(Gender, Reading_Score, fill = Test_Prep)) + geom_boxplot() + ggtitle("reading scores by Gender Boxplot") + xlab("Gender") + ylab("reading Scores")


tema = theme(plot.background = element_rect(fill="#FFDAB9"),
             plot.title = element_text(size=25, hjust=.5),
             axis.title.x = element_text(size=10, color = "black"),
             axis.text.x = element_text(size=7),
             axis.text.y = element_text(size=20))

options(repr.plot.width=14, repr.plot.height=10)

a<-ggplot(data = df, mapping = aes(Parent_Education,Math_Score)) +
  geom_boxplot(fill = "red", size = 1.3) +
  theme_wsj() +
  ggtitle("parent education vs math score") +  facet_grid(.~Gender)+
  tema

b<-ggplot(data = df, mapping = aes(Parent_Education,Reading_Score)) +
  geom_boxplot(fill = "blue", size = 1.3) +
  theme_wsj() +
  ggtitle("parent education vs readinf score") + facet_grid(.~Gender)+
  tema

c<-ggplot(data = df, mapping = aes(Parent_Education,Writing_Score)) +
  geom_boxplot(fill = "cyan", size = 1.3) +
  theme_wsj() +
  ggtitle("Parent education vs writing score ") +  facet_grid(.~Gender)+
  tema
  

plot_grid(a,b,c, nrow=3, ncol=1)



