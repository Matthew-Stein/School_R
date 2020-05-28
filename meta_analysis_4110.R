
#### -- Actual Meta analysis ---- 

library(tidyverse)
library(metafor)

full_dat %>% group_by(paper_id) %>% summarise(N=n()) %>% data.frame()


# The number of papers used in each analysis.
N_dat %>% group_by(paper_id) %>% summarise(N=n()) %>% data.frame()
NO3_dat %>% group_by(paper_id) %>% summarise(N=n()) %>% data.frame()
P_dat %>% group_by(paper_id) %>% summarise(N=n()) %>% data.frame()


#Data input
full_dat <- read.csv("C:/Users/matth/OneDrive/BIOL 4110 - Ecological Methods/full_dat.csv")
View(full_dat)
NO3_dat <- read.csv("C:/Users/matth/OneDrive/BIOL 4110 - Ecological Methods/NO3_dat.csv")
View(full_dat)
N_dat <- read.csv("C:/Users/matth/OneDrive/BIOL 4110 - Ecological Methods/N_dat.csv")
View(full_dat)
P_dat <- read.csv("C:/Users/matth/OneDrive/BIOL 4110 - Ecological Methods/P_dat.csv")
View(full_dat)

full_lnRR <- escalc(measure="ROM", n2i= C_N_n , n1i= E_N_n , m2i= C_N_mean , m1i= E_N_mean, sd2i= C_N_sd, sd1i= E_N_sd ,data=full_dat)
N_lnRR <- escalc(measure="ROM", n2i= C_N_n , n1i= E_N_n , m2i= C_N_mean , m1i= E_N_mean, sd2i= C_N_sd, sd1i= E_N_sd ,data=N_dat)
NO3_lnRR <- escalc(measure="ROM", n2i= C_NO3_n , n1i= E_NO3_n , m2i= C_NO3_mean , m1i= E_NO3_mean, sd2i= C_NO3_sd, sd1i= E_NO3_sd ,data= NO3_dat)
P_lnRR <- escalc(measure="ROM", n2i= C_P_n , n1i= E_P_n , m2i= C_p_mean , m1i= E_P_mean, sd2i= C_p_sd, sd1i= E_P_sd ,data= P_dat)
#gives the standardized lnRR for variance and effect sizes


summary(FEfull_lnRR <-rma(yi=yi,vi=vi,method="FE",data=full_lnRR))

summary(FE_N_lnRR <-rma(yi=yi,vi=vi,method="FE",data=N_lnRR))
summary(FE_NO3_lnRR <-rma(yi=yi,vi=vi,method="FE",data=NO3_lnRR))
summary(FE_P_lnRR <-rma(yi=yi,vi=vi,method="FE",data=P_lnRR))

forest(FEfull_lnRR,order='obs')

forest(FE_N_lnRR,order='obs')
forest(FE_NO3_lnRR,order='obs')
forest(FE_P_lnRR,order='obs')

summary(RAND_N_lrr<-rma(yi=yi,vi=vi,method="REML",data=N_lnRR))
summary(RAND_NO3_lrr<-rma(yi=yi,vi=vi,method="REML",data=NO3_lnRR))
summary(RAND_P_lrr<-rma(yi=yi,vi=vi,method="REML",data=P_lnRR))
#Modelling - prob useless for full_data

forest(FEfull_lnRR,order='obs', annotate = FALSE)

forest(RAND_N_lrr,order='obs')
forest(RAND_NO3_lrr,order='obs')
forest(RAND_P_lrr,order='obs')

par(mfrow=c(1,2))
funnel(RAND_N_lrr, addtau2 = FALSE, xlab = "Log Ratio of Means (NH3)")
funnel(RAND_N_lrr, addtau2=TRUE, xlab = "Log Ratio of Means (N & I^2)")

par(mfrow=c(1,2))
funnel(RAND_NO3_lrr, addtau2 = FALSE, xlab = "Log Ratio of Means (N03)")
funnel(RAND_NO3_lrr, addtau2 = TRUE, xlab = "Log Ratio of Means (NO3 & I^2)")

par(mfrow=c(1,2))
funnel(RAND_P_lrr, addtau2 = FALSE, xlab = "Log Ratio of Means (P)") 
funnel(RAND_P_lrr, addtau2 = TRUE, xlab = "Log Ratio of Means (P & I^2)")

?funnel
regtest(RAND_N_lrr)
regtest(RAND_NO3_lrr)
regtest(RAND_P_lrr)

?regtest


RAND_N_lrr$beta
exp(RAND_N_lrr$se)
RAND_NO3_lrr$beta 
RAND_NO3_lrr$se 
RAND_P_lrr$beta 
RAND_P_lrr$se 
FE_P_lnRR$beta
FE_P_lnRR$se


#OVERALL

par(mfrow=c(1,1))
forest(RAND_N_lrr,annotate=FALSE,order="obs",
       psize=1,xlab='Relative Change in Ammonium Concentration',
       mlab='Average Fixed Effect Model',atransf=exp)

forest(RAND_NO3_lrr,annotate=FALSE,order="obs",
       psize=1,xlab='Relative Change in Nitrate Concentration',
       mlab='Average Random Effect Model',atransf=exp)

forest(RAND_P_lrr,annotate=FALSE,order="obs",
       psize=1,xlab='Relative Change of Phosphate Concentration',
       mlab='Average Random Effect Model',atransf=exp)
forest(FE_P_lnRR,annotate=FALSE,order="obs",
       psize=1,xlab='Relative Change of Phosphate Concentration',
       mlab='Average Fixed Effect Model',atransf=exp)
?forest

exp(RAND_N_lrr$beta)
exp(RAND_NO3_lrr$se)
exp(RAND_P_lrr$beta)







#None of this worked. Will just keep for posterity.
modelmean<-data.frame(RAND_N_lrr$beta, RAND_N_lrr$se, 'Average Random Effect Model','Average','Average'); names(modelmean)<-c('yi','vi') # extracts the model means and turns it into a data frame; this was discussed earlier. you also do need to add labesl for whatever groups you might want to plot by shape/color...or else it will say it can't graph the point

N_lnRR2 <- N_lnRR %>% bind_rows(modelmean) #gotta add in the model mean
N_lnRR2$modelmean <- fct_reorder(N_lnRR2$paper_id,-exp(N_lnRR2$yi)) #and order your papers if you like

ggplot(N_lnRR2,aes(x=exp(yi),y=paper_id,col=precip)) + #the "col" says which group will get the differnt colors; the "shape" says which group will get the differnt shapes
  geom_errorbarh(height=.1,aes(xmin=exp(yi-(1.96*vi)),xmax=exp(yi+(1.96*vi)))) + #this adds error bars to the graph
  geom_point(size=2)+ #this adds points to the graph; size makes them bigger or smaller
  theme_bw() + #this just gets rid of the grey background
  labs(x='Relative Change in NH3 Concentration',y='Study',col='Precipitation Category') + #this let's you change the axis labels AND the color/shape labels
  geom_vline(xintercept= RAND_N_lrr$beta  ,lty=2)+ #this gives the vertical line where you want it
  scale_color_manual(values=c('cornflowerblue','orange2','green3'))
N_lnRR2

