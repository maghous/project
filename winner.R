library(tidyverse)
library(ggcorrplot)
library(grid)
library(gridExtra)
library(caret)
df<-read.csv("MatchTimeLinesFirst15.csv")
attach(df)
names(df)
str(df)
summary(df)

df<-df %>% select(-X,-matchId,-blueDragonKills,-redDragonKills)
df<-df %>% mutate(victory=ifelse(blue_win == 1,"BleuWin","RedWin")) %>% select(-blue_win)

options(repr.plot.width=15,repr.plot.height=10)
colors_fill = scale_fill_manual(values = c('orange','#e41a1c'))
colors_color = scale_color_manual(values = c('orange','#e41a1c'))

tham<-theme_minimal()+ theme(axis.text.x = element_text(size = 15, face = "bold"),
                              axis.text.y = element_text(size = 15, face = "bold"),
                              axis.title.x = element_text(size = 15),
                              axis.title.y = element_text(size = 15))

ggplot(df,aes(y=abs(blueGold - redGold),fill=victory)) +
  geom_boxplot(notch = TRUE,varwidth = TRUE) +
  colors_fill + tham


df %>% select(victory,blueAvgLevel,redAvgLevel) %>% 
  mutate(level_difference = abs(blueAvgLevel - redAvgLevel)) %>%
  ggplot(aes(x=level_difference,fill=victory)) +
  geom_histogram(position="dodge",bins=35) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  labs(x='Level Difference',y='Count')+
  colors_fill + tham

g1 = df %>% select(blueHeraldKills,redTowersDestroyed,victory) %>%
  ggplot(aes(x=blueHeraldKills,y=redTowersDestroyed,color=victory)) + geom_point(alpha=0.5) + 
  geom_jitter(width = 0.4,height = 0.4) + colors_color + tham

g2 = df %>% select(redHeraldKills,blueTowersDestroyed,victory) %>%
  ggplot(aes(x=redHeraldKills,y=blueTowersDestroyed,color=victory)) + geom_point(alpha=0.8) + 
  geom_jitter(width = 0.4,height = 0.4) + colors_color + tham

grid.arrange(g1,g2,ncol=2,nrow=1)
