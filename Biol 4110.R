### Ecological Methods - Meta Analysis

### Lesson #1 October 25th 

install.packages('tidyverse')
#also containes ggplot
install.packages('metafor')

#metanalysis uses effect sizes -Standardised Mean Differences; Similar to Cohen's d
library(tidyverse)
library(metafor)

#example;
dat <- get(data("dat.curtis1998"))
head(dat)
names(dat)
nrow(dat)

#piping w/ tidyverse  %>% = piping

dat %>% group_by(paper) %>% summarise(N=n()) %>% data.frame()
# Shows 29 unique papers referenced N # of times (N# of trees)

#stdzd mean difference
#n1i from coloumn names
SMD <- escalc(measure="SMD", n1i=n1i, n2i=n2i, m1i=m1i, m2i=m2i, sd1i=sd1i, sd2i=sd2i, data=dat)
head(SMD)
#yi = effect size, vi = variance

#LRR = log response ratio
lnRR <- escalc(measure="ROM", n1i=n1i, n2i=n2i, m1i=m1i, m2i=m2i, sd1i=sd1i, sd2i=sd2i, data=dat)
head(lnRR)

#compare two outputs

meta<- SMD %>% 
  rename(SMDy=yi, SMDv=vi) %>%
  merge(lnRR)
head(meta %>% rename(LRRy=yi, LRRv=vi))
  
nrow(meta)
#nrow SMD & nrow meta are equal (102)

#graph it - forest() in metafor package
forest(meta$SMDy, meta$SMDv)
#Shows that things are kind of increasing biomass (>0)

forest(meta$SMDy[1:12], meta$SMDv[1:12])
#just first 12 rows, less ugly
?escalc

#challenge, turn this whole graph into a much better graph. more readable - by tuesday 

### END -- Oct 25th lec

checking <- meta$n1i-meta$n2i
checking


par(mfrow=c(1,2))
plot(meta$n1i, meta$yi,  xlab = "Sample Size (n)", ylab = "Log Response Ratio (yi)")
abline(h=mean(meta$yi), col="blue")
plot(meta$n1i, meta$SMDy, xlab = "Sample Size (n)", ylab ="Standardized Mean Differences (SMDy)")
abline(h=mean(meta$SMDy), col="purple")

# Class 2 - Oct 30
#SMD or LRR ?
#LRR useful for comparing between studies of large variation in scope, more often used & preferred

common_m_smd <- rma(yi=SMDy, vi=SMDv, method = "FE", data = meta)
#FE = fixed effects
summary(common_m_smd)
# k= the number of studies, i.e. ~15 for our assignment
#zval w/ p<0.5, therefore CO2 IS associated with increased biomass. (report z value to 2 decimal points, p to 3)
common_m_lrr <- rma(yi=yi, vi=vi, method = "FE", data = meta)
summary(common_m_lrr)
#same result as SMD, good!
#estimate -> e^estimate = effect size
exp(.2088)
## = 1.232199, positive result with that effect size: Ex "On average CO2 increased biomass production by 23%"

#new model: w random effects
random_m_lrr <- rma(yi=yi, vi=vi, method = "REML", data = meta)
summary(random_m_lrr)
#I^2 = heterogeniety  --- 25% is low, 50% moderate, 75% high.

#what is explaining the heterogeniety??
random_m_lrr_fg <- rma(yi=yi, vi=vi, method = "REML", data = meta, mod=~fungrp)
summary(random_m_lrr_fg)
#still high, didnt resolve

random_m_lrr_fg_meth_time <- rma(yi=yi, vi=vi, method = "REML", data = meta, mod=~fungrp+method+time)
summary(random_m_lrr_fg_meth_time)
#still high, didnt resolve
#this is an awful model, nothing helped to explain any variability with this model.


#new thing: publication bias
funnel(random_m_lrr)
#if all in the white = no evidence of publication bias
#formal test:
regtest(random_m_lrr)
#p<0.5, therefore evidence of publication bias. 

#models did not help. 

forest(common_m_lrr, order = 'obs') #helps w a prettier graph

modelmean <- data.frame(common_m_lrr$b, common_m_lrr$se, 'Summary');
names(modelmean) <- c('LRRy', 'LRRv', 'auth_ord');
modelmean$


