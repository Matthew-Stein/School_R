install.packages('metafor')

library(tidyverse)
library(metafor)


####
#R is a calculator!
####
1+1 #yup!
test<-1 #the "carrot" assigns meaning to variables
test+5 #here, test is 1, so this should =6
Test+5 #and R is case sensitive
test<-5 #and it's easy to write over things
test+5 #why isn't it =6 like before??

#####
# read in data
#####
dat <- get(data(dat.curtis1998))
# it will probs look more like this for you:
#dat<-read_csv('..........csv')

#look at the data
head(dat)
names(dat)
summary(dat)
nrow(dat)
dat %>% group_by(paper) %>% summarise(N=n()) %>% data.frame()

#compute stdized mean difference
SMD <- escalc(measure="SMD", n1i=n1i, n2i=n2i, m1i=m1i, m2i=m2i, sd1i=sd1i, sd2i=sd2i,data=dat)
head(SMD)
#yay! look,  yi (effect size) and vi (varriance) got added to the data frame :)

#log response ratio
lnRR <- escalc(measure="ROM", n1i=n1i, n2i=n2i, m1i=m1i, m2i=m2i, sd1i=sd1i, sd2i=sd2i,data=dat)
head(lnRR)
#yay! the same thing happened


#compare the two outputs
meta<-SMD %>% 
	rename(SMDy=yi,SMDv=vi) %>% #turn the names into something more useful
		merge(lnRR %>% #combine it with the other data too
			rename(LRRy=yi,LRRv=vi)) #those names need to be made useful
head(meta)
#double check if we lost information
nrow(meta) #all good! sometimes merging can be tricky... do ?merge or ?bind_rows or ?bind_cols

#graph it!
forest(meta$SMDy,meta$SMDv) #sorry, this "forest" call is not that smart ->must attach the data with the names using the money sign
	#effect sizes, and then the 95% conf interval
	
forest(meta$SMDy[1:12],meta$SMDv[1:12]) 	#can subset the data frame so you can actually see things

#this is still not a very beautiful graph though. Work on compiling your data, and we will talk more about this issue on Tuesday :)
#we will also resolve errors and such :)


######
#Tuesday
######
#SMD vs lnRR
#both show the difference in means between 2 groups (control, experimental) --> 'effect size'
#both are unitless so comprable between studies without needing to consider units of measurement
#but, lnRR is often preferred given reasons discussed in class; we will be working with lnRR from here on out

#####
#Moving on to quantitative ANALYSIS (graphing=good, but no 'real' info)
#####
common_m_smd<-rma(yi=SMDy,vi=SMDv,method="FE",data=meta)#recall yi=effect size; vi=varriance
summary(common_m_smd) #seems like the treatment generally does something to the response
	#please report as df = 101, z = 18.24, p < 0.001 --> z = X.XX p = X.XXX
	#report the k (number of studies)
	#report the mean effect size (1.02)
common_m_lrr<-rma(yi=LRRy,vi=LRRv,method="FE",data=meta)
summary(common_m_lrr)
common_m_lrr$beta

	#df = 101, z = 38.34, p < 0.001 -->same significance result as SMD = good
	#give this is the LOG response ratio, the effect size is weird right now
	exp(.2088) #means that plant growth (reponse) was 23% bigger (1-effect) in the experimental group

#as an aside "FE" in method stands for fixed effects. this is NOT the default, so beware of this :) 	

#given reasons discussed in class, we will continue with LRR from here on out

####
#Of course it's more complicated :) (like testing for normality; this is also hard to do with n=15, but important to consider!!!)
####	
#first, how different is our data? (heterogeneity)
random_m_lrr<-rma(yi=LRRy,vi=LRRv,method="REML",data=meta)
summary(random_m_lrr)
	#here, REML stands for restricted maximum-likelihood estimator, this is default
	#I2 = 88.9% ; test for heterogeneity Q = 769.02, p < 0.001 --> yes, lots of heterogenity (I2=25%=low heterogeneity; I2=50%=moderate, I2=75=high)
forest(meta$LRRy,meta$LRRv) #but this makes sense looking at the (bad) figure --> lots of scatter
	#the method="FE" bit assumes I2=0; eek!
	
#determine what is moderating our results (and proabably explaining the heterogeniety)	
summary(rma(yi=LRRy,vi=LRRv,method="REML",data=meta, mod=~fungrp))#we get a new variable; r2. But this is TERRIBLE. The other new variable (test of moderators) also says that the moderator doesn't improve model fit (p>0.05)
summary(rma(yi=LRRy,vi=LRRv,method="REML",data=meta, mod=~fungrp+method+time))#equally terrible. 

random_m_lrr_modif<-rma(yi=LRRy,vi=LRRv,method="REML",data=meta, mod=~time+method+fungrp)
summary(random_m_lrr_modif)
	#we get a new variable; r2. But this is TERRIBLE. Don't know what else to do. move on for this analysis, refernce the meta analysis mega book if you want to go more indepth

####
#What about publication bias (aka, significant results more likely to be published)
#effect size vs std err --> low precision studies (HIGH stderr) are probably less likely to be published (so shouldn't see much with high stderr and large effect sizes(+/-)). If no bias, we have an inverted funnel
#however, high stderr and large effect sizes might also be due to the high heterogenity we couldn't explain
funnel(random_m_lrr)
regtest(random_m_lrr) #well, probably some pub bias. Not that intersting of a finding for our purposes.
	
####
#Conclusion; just introduced A LOT of things to consider. Where did we start/finish?
#The random model didn't help at all, so we're back to the base model
forest(meta$LRRy,meta$LRRv)
forest(common_m_lrr) #you can also do this with the stats!!

####
#Pretty plots
####
forest(common_m_lrr,order='obs') #this helps

?rma #look at the value section
common_m_lrr$beta #coficient of overall model
common_m_lrr$se #se of overall model

modelmean<-data.frame(common_m_lrr$b, common_m_lrr$se, 'Summary'); names(modelmean)<-c('LRRy','LRRv','auth_ord'); modelmean$author<-'Model Average'
prettymeta <- meta %>%
	mutate(Study=row.names(.)) %>%
	mutate(author = paste(genus,paper,sep="_")) %>%
	mutate(auth_ord=fct_reorder(author,LRRy)) %>%
	merge(modelmean,all=T)

prettymeta$auth_ord2<- fct_reorder(prettymeta$author,exp(prettymeta$LRRy)) 

ggplot(filter(prettymeta),aes(x=(exp(LRRy)-1)*100,y=auth_ord2,col=method)) +
	geom_point() +
	geom_errorbarh(height=.1,aes(xmin=(exp(LRRy-(1.96*LRRv))-1)*100,xmax=(exp(LRRy+(1.96*LRRv))-1)*100)) +
	theme_bw() +
	labs(x='Percent Change (%)',y='Reference') +
	geom_vline(xintercept=0,lty=2)+
	facet_grid(fungrp~.,scales='free_y',space='free_y')
	
forest(meta$LRRy[meta$genus=='PICEA'],meta$LRRv[meta$genus=='PICEA'], showweights =T)

#http://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups
#https://ecologyforacrowdedplanet.wordpress.com/2013/05/10/using-metafor-and-ggplot-togetherpart-1/



#############
###The super simple way to graph data! :) 
#############
#Log response ratio
ee<-read_csv('/Users/ellen/Desktop/Ellen/Guelph/Ecology Methods Teaching/example data.csv') #read in data

RR_ee <- escalc(measure="ROM", n1i=cont_n, n2i=exp_n, m1i=cont_mean, m2i=exp_mean, sd1i=cont_sd, sd2i=exp_sd,data=ee) #see how I've specificed what "n1i" is in my data ("=cont_n"), for instance (this happends for the means, sd, n for both control ("1i") and experimental (2i)) #Also note that you can do log response ratio "ROM" or std mean difference "SMD" ...it's whatever you think is best for your data

fixed_ee<-rma(yi=yi,vi=vi,method="FE",data=RR_ee) #but do note there are different models, as previously discussed :)

summary(fixed_ee) #this is where you get the model output
exp(.1367) #-->experimental treatment increases response by 14.6% on average

forest(fixed_ee,slab=RR_ee$paper,annotate=FALSE,order="obs",psize=1,xlab='Relative Change With Nutrient Addition',mlab='Average Fixed Effect Model',atransf=exp)	
#What follows is a bunch of annotations; feel free to use what you like, and to don't use what you don't like. I've tried to really add a bunch of things here which you might think is interesting, obviously update and use/don't use the ones you like/don't like :)
#slab --> gives labels to the studies (default is "study 1", "study 2", etc)
#annotate --> includes or removes the range values for each study shown on the right of the figure if you don't specifiy anything (default is annotate=TRUE) --> this typically shows the "mean [95% CI]"
#order --> tells how to organize the studies ('obs' orders them by the observed effect size; there are other ways to order, just call up the "?forest" help file and read a bit :) )
#psize --> tells how big the point indicating the mean effect size for each study should be. default is to have the points sized by their precision 
#xlab --> allows you to give a legend title; whatever you think is good here...of course, make sure it describes what you are actually showing on the x axis :)
#mlab --> lets you change the name of your overall model 
#atransf --> this takes e^(effect size), in effect converting a log response into a fraction; it also changes where the dashed line is, <1 means a decrease >1 means an increase


#########
#The less easy way to graph data 
#(if you need a complicated color scheme) and/or differnt panels and/or different shapes
#delete the color/shape/facet_grid sections if you don't need them all :)
#########
modelmean<-data.frame(fixed_ee$beta, fixed_ee$se, 'Average Fixed Effect Model','Average','Average'); names(modelmean)<-c('yi','vi','paper','fungrp',"time") # extracts the model means and turns it into a data frame; this was discussed earlier. you also do need to add labesl for whatever groups you might want to plot by shape/color...or else it will say it can't graph the point

RR_ee_2 <- RR_ee %>% bind_rows(modelmean) #gotta add in the model mean
RR_ee_2$auth_ord<- fct_reorder(RR_ee_2$paper,-exp(RR_ee_2$yi)) #and order your papers if you like


ggplot(RR_ee_2,aes(x=exp(yi),y=auth_ord,col=fungrp,shape=time)) + #the "col" says which group will get the differnt colors; the "shape" says which group will get the differnt shapes
  geom_errorbarh(height=.1,aes(xmin=exp(yi-(1.96*vi)),xmax=exp(yi+(1.96*vi)))) + #this adds error bars to the graph
  geom_point(size=2)+ #this adds points to the graph; size makes them bigger or smaller
  theme_bw() + #this just gets rid of the grey background
  labs(x='Relative Change',y='Study',col='Functional Group',shape='Study length') + #this let's you change the axis labels AND the color/shape labels
  geom_vline(xintercept=1,lty=2)+ #this gives the vertical line where you want it
  facet_grid(method~.,scales='free_y',space='free_y') + #this gives differnt panels; here "method" makes the differnt panels
  scale_color_manual(values=c('cornflowerblue','black','orange2','green3'))






