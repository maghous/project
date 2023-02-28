library(tidyverse)
library(ggplot2)

df <- read.csv('MFG10YearTerminationData.csv')
attach(df)
names(df)
summary(df)
str(df)
view(df)
table(df$city_name)
table(df$department_name)
table(df$gender_full)
table(df$job_title)
table(df$STATUS)

ggplot(data = df[df$STATUS=='TERMINATED',],aes(x=city_name,fill=gender_short)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Figure 1 : Distribution of terminated employees across cities") +
  ylab("Count of employees") +
  xlab("City Name") + theme_light()


ggplot(data = df,aes(x=city_name)) +
  geom_bar(alpha = 0.4, color = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Figure 2 : Distribution of all employees across cities") +
  ylab("Count of employees") +
  xlab("City Name") + theme_light()



ggplot(data = df[df$STATUS=='TERMINATED',],aes(x=department_name,fill=gender_short)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Figure 1 : Distribution of terminated employees across departments") +
  ylab("Count of employees") +
  xlab("Department Name") +theme_light()



#df$city_name = as.numeric(df$city_name)
#df$department_name = as.numeric(df$department_name)
#df$job_title = as.numeric(df$job_title)
#df$gender_full = as.numeric(df$gender_full)
#df$BUSINESS_UNIT = as.numeric(df$BUSINESS_UNIT)




sub_col = c('age'
            ,'length_of_service'
            ,'city_name'
            ,'department_name'
            ,'job_title'
            ,'store_name'
            ,'gender_full'
            ,'STATUS_YEAR'
            ,'BUSINESS_UNIT'
            ,'STATUS')

attr_data_sub = df[,sub_col]


set.seed(123)
df$STATUS01 = ifelse(df$STATUS=='ACTIVE',1,0)
partitionIndex = createDataPartition(df$STATUS01,p=0.8,list=FALSE)

attr_data_sub.train = attr_data_sub[partitionIndex,]
attr_data_sub.test = attr_data_sub[-partitionIndex,]
prop.table(table(attr_data_sub.train$STATUS01))


prop.table(table(attr_data_sub.test$STATUS01))

prop.table(table(attr_data_sub.train$STATUS01))

