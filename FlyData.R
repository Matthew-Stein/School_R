
Egg_Data <- read.csv("C:/Users/matth/OneDrive/ZOO 4070 - Animal Behaviour/Egg_Data.csv")
View(Egg_Data)

E_Counts <- Egg_Data$T_E_counts
L_Counts <- Egg_Data$T.L_counts
Cb_Counts <- Egg_Data$C.b_counts
Ca_Counts <- Egg_Data$C.a_counts
Ca_test <- Egg_Data$Ca_test
Cb_test <- Egg_Data$Cb_test

install.packages("cowplot")
install.packages("car")
install.packages("sciplot")
install.packages("lme4")
install.packages("scales")
library(car)
library(sciplot)
library(tidyverse)
library(lme4)
library(cowplot)
library(scales)


t.test(E_Counts, L_Counts, paired = TRUE, alternative = "two.sided")
t.test(Ca_Counts, Cb_Counts, paired = TRUE, alternative = "two.sided")
t.test(Cb_test, Ca_test, paired = TRUE, alternative = "two.sided")

delta_eggs <- (E_Counts - L_Counts)
delta_controls <- abs(Ca_test - Cb_test)
delta_eggs
delta_controls

t.test(delta_eggs, delta_controls, paired = FALSE, alternative = "greater")

1-(mean(L_Counts)/mean(E_Counts))




?barplot()
mean(E_Counts)
mean(L_Counts)
mean(Ca_Counts)
mean(Cb_Counts)

?ggplot


ggplot(Egg_Data %>% 
         group_by(SPP,TREAT,WATER,COMM) %>%
         summarise(MEAN=mean(SHRUB,na.rm=T),SE=sd(SHRUB,na.rm=T/sqrt(n()))),
       aes(x=COMM,y=MEAN,fill=as.factor(WATER)))+
  geom_bar(stat='identity',position='dodge',col='black')+
  geom_errorbar(aes(ymin=MEAN-SE,ymax=MEAN+SE),width=.2,position=position_dodge(width=1))+
  scale_fill_manual(values=c('#deebf7','#3182bd'),labels=c('Low','High'))+ #colors here! http://colorbrewer2.org
  labs(x='Competition treatment',y='Shrub biomass (g/pot)',fill='Water\nTreatment')+
  facet_wrap(~SPP,scales='free_y')





