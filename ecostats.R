#########
#Get set up
#########
#load any packages you might need
#don't forget you might need to install them first! Remember, it's like this install.packages('car') 
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



########
#Read in data
########
#For the purposes of this class, I'd reccomend getting your excel sheet in it's final form BEFORE moving into R. 
#More generally, if you end up doing data analyses or research as a career, my reccomendataion would 100% be the opposite. Of course, get your final spreadsheet but then do any/all data "cleaning" or maniupulations WITHIN R. It will make your life so much easier :) You'll be able to track errors/updates in your data, easily communicate with colleagues about the data, etc. 
data<-read.csv("/Users/ellen/Desktop/Ellen/2018_Esch et al_Competition across gradients/Esch_et_al_2018_data_knb.csv",header=T) %>% 
	mutate(TREAT=paste(SPP,COMM)) %>% #Needed to create a new variable
	mutate(BIOMASS = SHRUB+AVENA) %>% #can also calculate new variables
	mutate(BLOCK=as.factor(BLOCK)) #might need to make things categorical/continious, be careful here!

###What follows is metadata. You don't need it, but again, if you're going into any data analysis/management career, having meta data and annotations in your code will make life much better :)
	#SPP = native shrub species grown
	#COMM = community is with or without invasive Avena (+ is with, - is without)
	#WATER = amount of water added, 8 levels
	#BLOCK = block, 1-3 possible
	#SHRUB = aboveground native shrub biomass
	#AVENA = aboveground Avena biomass
	#ROOT = total root biomass
	#NITRATE = resin nitrate (ppm) extracted
	#MOISTURE = average soil moisture over the entirety of the experiment
	#TREAT = native shrub spp, community


########
#Need to check data normality and such first
########
#we will do a shapiro test on BIOMASS (total pot biomass)
shapiro.test(data$BIOMASS) 
#the data fails assumptions...not surprising since we haven't parsed the data yet --> CLT of statistics!
ggplot((data)) + 
	geom_density(aes(x = BIOMASS), alpha = 0.2,fill='black')+
	theme_cowplot()

with(filter(data,TREAT=='Encelia -'), tapply(BIOMASS, WATER, shapiro.test)) #(with(data,tapply(RESPONESE, TREATMENT,shapiro.test)))
with(filter(data,TREAT=='Encelia +'), tapply(BIOMASS, WATER, shapiro.test))
with(filter(data,TREAT=='Eriogonum -'), tapply(BIOMASS, WATER, shapiro.test)) 
with(filter(data,TREAT=='Eriogonum +'), tapply(BIOMASS, WATER, shapiro.test)) 
#overall, some assumptions are violated, but on average the data looks good to go! 
ggplot((data)) + 
	geom_density(aes(x = BIOMASS, fill = as.factor(WATER)), alpha = 0.2) +
	facet_wrap(~TREAT,scales='free')+
	labs(fill='Water\nTreatment',x='Total biomass production (g/pot)')

#does a log transformation improve the fit?	(let's do this graphically)
ggplot((data)) + 
	geom_density(aes(x = log(BIOMASS), fill = as.factor(WATER)), alpha = 0.2) +
	facet_wrap(~TREAT,scales='free')+
	labs(fill='Water\nTreatment',x='Log total biomass production (g/pot)')
#looks like the data gets becomes leptokurtic (more peaked) and left skewed (tails on left)

##personal golden rule of normality...
#0) Before all else, think about your experimental design, and what you already know to be true about statistics (best practices taught in other courses)
#1) consider overall fit of data
#2) what you do to one, you must do to all (within a response, across treatments)
#### A log transformation improved normality of biomass response, and was applied throughout instances.
#3) log (base e) transformations most acceptable. zeros require negative binomial distributions (MASS package, https://cran.r-project.org/web/packages/MASS/MASS.pdf)
#4) True outliers may be removed (data error), but it is preferrable to keep all measured values in analysis
#5) Turn to R tutorials for non-parametric tests: http://www.r-tutor.com/elementary-statistics/non-parametric-methods
#6) evaluate model fits

#######
#biomass 2
#######

#subset the data for easy analysis...example
data2 <- data %>% 
	filter(WATER==200|WATER==550) %>% 
	mutate(WATER=ifelse(WATER==550,'b.high','a.low')) %>% 
	mutate(COMM=ifelse(COMM=='-','Alone','With Competition'))

#3-way ANOVA
Anova(lmer(SHRUB~SPP*COMM*WATER+(1|BLOCK),data=data2),type=2)
	#sig 3-way so break up by species
	Anova(lmer(SHRUB~COMM*WATER+(1|BLOCK),data=filter(data2,SPP=='Encelia')),type=2)
		#sig 2-way so do post-hoc for each water trt		
		TukeyHSD(aov(SHRUB ~(WATER),data=filter(data2,SPP=='Encelia',COMM=='Alone')))
		TukeyHSD(aov(SHRUB ~(WATER),data=filter(data2,SPP=='Encelia',COMM=='With Competition')))
	#sig 3-way so break up by species
	Anova(lmer(SHRUB~COMM*WATER+(1|BLOCK),data=filter(data2,SPP=='Eriogonum')),type=2)
		#sig 2-way so do post-hoc for each water trt		
		TukeyHSD(aov(SHRUB ~(WATER),data=filter(data2,SPP=='Eriogonum',COMM=='Alone')))
		TukeyHSD(aov(SHRUB ~(WATER),data=filter(data2,SPP=='Eriogonum',COMM=='With Competition')))#ns

#plot biomass data
ggplot(data2 %>% 
		group_by(SPP,TREAT,WATER,COMM) %>%
		summarise(MEAN=mean(SHRUB,na.rm=T),SE=sd(SHRUB,na.rm=T/sqrt(n()))),
	aes(x=COMM,y=MEAN,fill=as.factor(WATER)))+
	geom_bar(stat='identity',position='dodge',col='black')+
	geom_errorbar(aes(ymin=MEAN-SE,ymax=MEAN+SE),width=.2,position=position_dodge(width=1))+
	scale_fill_manual(values=c('#deebf7','#3182bd'),labels=c('Low','High'))+ #colors here! http://colorbrewer2.org
	labs(x='Competition treatment',y='Shrub biomass (g/pot)',fill='Water\nTreatment')+
	facet_wrap(~SPP,scales='free_y')
	#note, this figure is pretty good! But I added the "*" in powerpoint :) (it's easier)

#check mechanism; more soil moisture = more biomass?
	cor.test(data2$MOISTURE[data2$TREAT=='Encelia -'],data2$BIOMASS[data2$TREAT=='Encelia -'],method="pearson") #sig
	cor.test(data2$MOISTURE[data2$TREAT=='Encelia +'],data2$BIOMASS[data2$TREAT=='Encelia +'],method="pearson") #ns
	cor.test(data2$MOISTURE[data2$TREAT=='Eriogonum -'],data2$BIOMASS[data2$TREAT=='Eriogonum -'],method="pearson") #sig
	cor.test(data2$MOISTURE[data2$TREAT=='Eriogonum +'],data2$BIOMASS[data2$TREAT=='Eriogonum +'],method="pearson") #sig

#plot moisture/biomass data....this is a bit tricky since we CANT have the Encelia + relationship
	ggplot(data2, aes(x=MOISTURE,y=BIOMASS,col=COMM))+
		geom_point()+
		scale_color_manual(values=c('#000000','#969696'))+
		facet_wrap(~SPP,scales='free')+
		geom_smooth(method='lm',fill=NA,inherit.aes=F,data=(data2 %>% filter(TREAT=='Encelia -')),aes(x=MOISTURE,y=BIOMASS),col='#000000') +
		geom_smooth(method='lm',fill=NA,inherit.aes=F,data=(data2 %>% filter(TREAT=='Eriogonum -')),aes(x=MOISTURE,y=BIOMASS),col='#000000') +
		geom_smooth(method='lm',fill=NA,inherit.aes=F,data=(data2 %>% filter(TREAT=='Eriogonum +')),aes(x=MOISTURE,y=BIOMASS),col="#969696")+
		labs(x='Soil moisture (VWC%)',y='Biomass (g/pot)',col='Competition\nTreatment')


#a simple, but not very pretty way to graph
bargraph.CI(x.factor=COMM,response=SHRUB,group=WATER,col=,legend=T,xlab='Competition Treatment',ylab='Shrub biomass (g/pot)',main='Encelia',data=filter(data2,SPP=='Encelia'))
bargraph.CI(x.factor=COMM,response=SHRUB,group=WATER,col=,legend=T,xlab='Competition Treatment',ylab='Shrub biomass (g/pot)',main='Eriogonum',data=filter(data2,SPP=='Eriogonum'))

########
#Biomass complex
#######
#encelia shrub above ground biomass
encaSh2<-lmer(SHRUB~COMM+WATER+I(WATER^2/1000)+COMM:WATER+COMM:I(WATER^2/1000) +(1|BLOCK),data=subset(data,SPP=='Encelia'), REML=FALSE)
encaSh3<-lmer(SHRUB~COMM*WATER+(1|BLOCK),data=subset(data,SPP=='Encelia'), REML=FALSE)
AIC(encaSh2,encaSh3)#the model with the squared term is better
Anova(encaSh2)

#eriogonum shrub aboveground bioimass
eriSh2<-lmer(SHRUB~COMM+WATER+I(WATER^2/1000)+COMM:WATER+COMM:I(WATER^2/1000)+(1|BLOCK),data=subset(data,SPP=='Eriogonum'),REML=FALSE)
eriSh3<-lmer(SHRUB~WATER*COMM+(1|BLOCK),data=subset(data,SPP=='Eriogonum'),REML=FALSE)
AIC(eriSh2,eriSh3)#the model with the squared term is better
Anova(eriSh2)

ggplot(data %>% #first, need to aggregate the data so it's good to plot (means/se)
		group_by(TREAT,WATER,COMM,SPP) %>%
		summarise(MEAN=mean(SHRUB,na.rm=T),SE=sd(SHRUB,na.rm=T/sqrt(n()))),
	aes(x=WATER,y=MEAN,col=as.factor(COMM)))+ #x/y values, plus any colors etc
	geom_point(size=2)+ #add points, and adjust size (if needed)
	geom_errorbar(aes(ymin=MEAN-SE,ymax=MEAN+SE),width=10)+
	scale_color_manual(values=c('orange2','darkgreen'),breaks=c('-','+'),labels=c('Alone','With Competition'))+
	facet_wrap(~SPP)+
	labs(x='Water treatment (ml 2x weekly)',y='Focal shrub biomass (g/pot)',col='Competition')+ #make good figure labels
	stat_smooth(inherit.aes=F,data=(data %>% filter(COMM=='-')),aes(x= WATER,y=SHRUB),method = "lm", formula = y ~ x + I(x^2), size = .5, color = "orange2",fill=NA,lty=2) + #add a trend line
	stat_smooth(inherit.aes=F,data=(data %>% filter(COMM=='+')),aes(x= WATER,y=SHRUB),method = "lm", formula = y ~ x + I(x^2), size = .5, color = "darkgreen",fill=NA,lty=2) #add a trend line

#a simple, but not very pretty way to graph
lineplot.CI(x.factor=WATER,response=SHRUB,group=COMM,ylab='Shrub biomass (g/pot)',xlab='Water Treatment (ml 2x weekly)',main='Encelia',data=filter(data,SPP=='Encelia'))
lineplot.CI(x.factor=WATER,response=SHRUB,group=COMM,ylab='Shrub biomass (g/pot)',xlab='Water Treatment (ml 2x weekly)',main='Eriogonum',data=filter(data,SPP=='Eriogonum'))
