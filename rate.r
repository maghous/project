library(ggplot2)
library(dplyr)
library(maps)
library(scales)
library(ggthemes)
rate2015<-subset(Rate,BusinessYear == "2015")
IndividualOption <- subset(rate2015, (rate2015$Age != "Family Option" & rate2015$IndividualRate < "9000"),
                           select = c(BusinessYear:IndividualTobaccoRate))

benefit <- subset(PlanAttributes, select = names(PlanAttributes)[c(115,104,162,166,170)])
benefit$PlanId <- substr(benefit$PlanId, 1, 14)
IndividualOption$PlanId <- as.character(IndividualOption$PlanId)

benefit$TEHBInnTier1IndividualMOOP <- gsub(",", "", benefit$TEHBInnTier1IndividualMOOP)
benefit$TEHBInnTier1IndividualMOOP <- gsub("\\$", "", benefit$TEHBInnTier1IndividualMOOP)
benefit$TEHBInnTier1IndividualMOOP <- as.numeric(benefit$TEHBInnTier1IndividualMOOP)
benefit$TEHBInnTier2IndividualMOOP <- gsub(",", "", benefit$TEHBInnTier2IndividualMOOP)
benefit$TEHBInnTier2IndividualMOOP <- gsub("\\$", "", benefit$TEHBInnTier2IndividualMOOP)
benefit$TEHBInnTier2IndividualMOOP <- as.numeric(benefit$TEHBInnTier2IndividualMOOP)
benefit$TEHBOutOfNetIndividualMOOP <- gsub(",", "", benefit$TEHBOutOfNetIndividualMOOP)
benefit$TEHBOutOfNetIndividualMOOP <- gsub("\\$", "", benefit$TEHBOutOfNetIndividualMOOP)
benefit$TEHBOutOfNetIndividualMOOP <- as.numeric(benefit$TEHBOutOfNetIndividualMOOP)

benefittouse <- group_by(benefit, PlanId, MetalLevel) %>%
  summarise(innettier1moop=mean(TEHBInnTier1IndividualMOOP),
            innettier2moop=mean(TEHBInnTier2IndividualMOOP),
            outnetmoop=mean(TEHBOutOfNetIndividualMOOP))
planrates <- inner_join(IndividualOption, benefittouse, by="PlanId")

bystatecoverage <- group_by(planrates, StateCode, MetalLevel) %>%
  summarize(PlanOffered = length(unique(PlanId)),
            MeanIndRate= mean(IndividualRate),
            MedianIndRate = median(IndividualRate)) %>%
  arrange(desc(PlanOffered))

#now exclude the dental coverage
medicalonly <- subset(bystatecoverage, bystatecoverage$MetalLevel != "High" & bystatecoverage$MetalLevel != "Low")

graph5 <- ggplot(data=medicalonly, aes(x=reorder(StateCode, PlanOffered), y=PlanOffered))+
  geom_bar(aes(fill=MetalLevel), stat="identity")+
  ggtitle("Number of Medical Plans Offered By Coverage Levels")+
  labs(x="Coverage Metal Level", y="Number of Plans Offered")
graph5

#premium distribution by coverage levels in 2015. graph 6 box plot
graph6 <- ggplot(data=planrates, aes(x=MetalLevel, y=IndividualRate, fill=MetalLevel))+
  geom_boxplot()+
  stat_summary(fun.y="mean",na.rm =T, geom="point",colour="darkred", size=1.5)+
  ggtitle("Ind Rate Distribution of Coverage")
graph6

#now looking at this by age dimension
#by age group
byagecoverage <- select(planrates, Age, MetalLevel, IndividualRate) %>%
  group_by(Age, MetalLevel) %>%
  summarize(MeanIndRate= mean(IndividualRate),
            MedianIndRate = median(IndividualRate)) %>%
  arrange(desc(MeanIndRate))

graph7 <- ggplot(data=byagecoverage, aes(x=Age, y=MeanIndRate))+
  geom_point(stat='identity', size = 1.5, aes(color = factor(MetalLevel)))+
  ggtitle("Average Monthly Premium by Age")+
  labs(x="Age", y="Average Premium by Coverage")
graph7
