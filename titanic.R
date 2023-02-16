library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("mice")
train<-read.csv("train.csv",stringsAsFactors = F)
test<-read.csv('test.csv',stringsAsFactors = F)
full<-bind_rows(train,test)
str(full)
#grab title from passenger names
full$Title<-gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex,full$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title == "Mlle"] <-"Miss"
full$Title[full$Title == "Ms"] <-"Miss"
full$Title[full$Title == "Mme"] <-"MrS"
full$Title[full$Title %in% rare_title] <-"Miss"
table(full$Sex,full$Title)
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
full$Fsize<-full$SibSp + full$Parch + 1
full$Family <-paste(full$Surname,full$Fsize,sep="_")
#Visualize relation between family size vs survival
ggplot(full[1:nrow(full),],aes(x=Fsize,fill=factor(Survived)))+geom_bar(stat="count",position="dodge")+scale_x_continuous(breaks=c(1:11))+labs(x="Family Size")+theme_few()

fare<-full %>% filter(PassengerId != 62 & PassengerId != 830)







