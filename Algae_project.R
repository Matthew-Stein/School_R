
#Algal Project - Data Analysis

library(dplyr)
library(tidyverse)
library(car)

Eco_Dat <- read.csv("C:/Users/matth/OneDrive/BIOL 4110 - Ecological Methods/Eco_Dat.csv", header=TRUE) 
Eco_Dat_down <- read.csv("C:/Users/matth/OneDrive/BIOL 4110 - Ecological Methods/Eco_Dat_down.csv", header=TRUE) 
Eco_Dat_up <- read.csv("C:/Users/matth/OneDrive/BIOL 4110 - Ecological Methods/Eco_Dat_up.csv", header=TRUE)
Eco_Dat_red <- read.csv("C:/Users/matth/OneDrive/BIOL 4110 - Ecological Methods/Eco_Dat_red.csv", header=TRUE)


View(Eco_Dat)
View(Eco_Dat_down)
View(Eco_Dat_up)

Courses <- substr(Eco_Dat_down$Sample , start = 1, stop = 2)
Eco_Dat_down <- cbind(Eco_Dat_down, Courses)

Courses <- substr(Eco_Dat_up$Sample , start = 1, stop = 2)
Eco_Dat_up <- cbind(Eco_Dat_up, Courses)


Courses_Fixed <- c("Dr_d", "Dr_d", "Dr_d", "Dr_d", "Dr_d", "Dr_u", "Dr_u", "Dr_u", "Dr_u", "Dr_u", "MH_d", "MH_d", "MH_d",
             "MH_d", "MH_d", "MH_u", "MH_u", "MH_u", "MH_u", "MH_u", "GCC_d", "GCC_d", "GCC_d", "GCC_d", "GCC_d", "GCC_u",
             "GCC_u", "GCC_u", "GCC_u", "GCC_u", "Cu_d", "Cu_d", "Cu_d", "Cu_d", "Cu_d", "Cu_u", "Cu_u", "Cu_u", "Cu_u",
             "Cu_u", "DV_d", "DV_d", "DV_d", "DV_d", "DV_d", "DV_u", "DV_u", "DV_u", "DV_u", "DV_u", "H20", "H20",
             "H20", "H20", "H20", "NP", "NP", "NP", "NP", "NP")
Courses_alone <- c("Dr", "Dr", "Dr", "Dr", "Dr", "Dr", "Dr", "Dr", "Dr", "Dr", "MH", "MH", "MH",
                   "MH", "MH", "MH", "MH", "MH", "MH", "MH", "GCC", "GCC", "GCC", "GCC", "GCC", "GCC",
                   "GCC", "GCC", "GCC", "GCC", "Cu", "Cu", "Cu", "Cu", "Cu", "Cu", "Cu", "Cu", "Cu",
                   "Cu", "DV", "DV", "DV", "DV", "DV", "DV", "DV", "DV", "DV", "DV", "H20", "H20",
                   "H20", "H20", "H20", "H20", "H20", "H20", "H20", "H20")
Eco_Dat <- cbind(Eco_Dat, Courses_Fixed)
Eco_Dat <- cbind(Eco_Dat, Courses_alone)
Eco_Dat$Courses <- NULL

Course_codes <- Eco_Dat %>% group_by(Courses_Fixed) %>% summarise(N=n()) %>% data.frame()
Course_codes

Course_codes <- Eco_Dat %>% group_by(Courses_Fixed) %>% summarise(N=n()) %>% data.frame()
Course_codes <- Eco_Dat %>% group_by(Courses_Fixed) %>% summarise(N=n()) %>% data.frame()


Dr <- Eco_Dat %>%
  filter(Courses_alone == "Dr")
GCC <- Eco_Dat %>%
  filter(Courses_alone == "GCC")
MH <- Eco_Dat %>%
  filter(Courses_alone == "MH")
Cu <- Eco_Dat %>%
  filter(Courses_alone == "Cu")
DV <- Eco_Dat %>%
  filter(Courses_alone == "DV")
H20 <- Eco_Dat %>%
  filter(Courses_alone == "H20")


# Each course becomes their own dataframe
Cu_d <- Eco_Dat %>%
  filter(Courses_Fixed == "Cu_d")
Cu_u <- Eco_Dat %>%
  filter(Courses_Fixed == "Cu_u")
Dr_d <- Eco_Dat %>%
  filter(Courses_Fixed == "Dr_d")
Dr_u <- Eco_Dat %>%
  filter(Courses_Fixed == "Dr_u")
DV_d <- Eco_Dat %>%
  filter(Courses_Fixed == "DV_d")
DV_u <- Eco_Dat %>%
  filter(Courses_Fixed == "DV_u")
GCC_d <- Eco_Dat %>%
  filter(Courses_Fixed == "GCC_d")
GCC_u <- Eco_Dat %>%
  filter(Courses_Fixed == "GCC_u")
MH_d <- Eco_Dat %>%
  filter(Courses_Fixed == "MH_d")
MH_u <- Eco_Dat %>%
  filter(Courses_Fixed == "MH_u")
H2O <- Eco_Dat %>%
  filter(Courses_Fixed == "H20")
NP <- Eco_Dat %>%
  filter(Courses_Fixed == "NP")

# each vector: (cutten, DoonV, GCC, MH)
Up_NO2 <-c(0.5, 0.5, 0.5, 0.5)
Up_NO3 <-c(20, 40, 20, 20)
Up_ph <-c(8.5, 8, 8, 8.5)
Up_PO4 <-c(0.5, 0.25, 0, 0.5)
Up_Kh <-c(180, 120, 120, 240)
Up_Gh <-c(180, 180, )

Down_NO2 <-c(0.5, 0.5, 0.5, 1)
Down_NO3 <-c(20, 40, 20, 20)
Down_ph <-c(8.0, 8.5, 8, 8.5)
Down_PO4 <-c(0.25, 0.25, 0.25, 0.5)
Down_Kh <-c(80, 180, 120, 240)
Down_Gh <-c(180, 180, )

Soil_NO2 <-c()
Soil_NO3 <-c()
Soil_ph <-c()
Soil_PO4 <-c()
Soil_Kh <-c()
Soil_Gh <-c()





head(Eco_Dat)
ggplot(Eco_Dat,aes(x=S_NO2,y=W_NO2,col=updown))+
  geom_point()+geom_smooth(method='lm',fill=NA)

#example stripped from online
model = aov(dry_mass ~ updown, data = Eco_Dat)
summary(model)


#Within course ANOVA
t.test(DV_d$dry_mass,DV_u$dry_mass, paired = FALSE, conf.level = 0.95, alternative = "two.sided")
#sig
t.test(GCC_d$dry_mass,GCC_u$dry_mass, paired = FALSE, conf.level = 0.95, alternative = "two.sided")
t.test(DV_d$dry_mass,DV_u$dry_mass, paired = FALSE, conf.level = 0.95, alternative = "two.sided")
t.test(DV_d$dry_mass,DV_u$dry_mass, paired = FALSE, conf.level = 0.95, alternative = "two.sided")
t.test(DV_d$dry_mass,DV_u$dry_mass, paired = FALSE, conf.level = 0.95, alternative = "two.sided")
t.test(DV_d$dry_mass,DV_u$dry_mass, paired = FALSE, conf.level = 0.95, alternative = "two.sided")


MH_aov = lm(dry_mass ~ updown,data = MH)
Anova(MH_aov, type = "II")
GCC_aov = lm(dry_mass ~ updown,data = GCC)
Anova(GCC_aov, type = "II")
Cu_aov = lm(dry_mass ~ updown,data = Cu)
Anova(Cu_aov, type = "II")
Dr_aov = lm(dry_mass ~ updown,data = Dr)
Anova(Dr_aov, type = "II")
DV_aov <- lm(dry_mass ~ updown,data = DV)
Anova(DV_aov, type = "II")
#SIG
H20_aov = lm(dry_mass ~ updown,data = H20)
Anova(H20_aov, type = "II")

MH_aov = lm(dry_mass ~ W_PO4,data = MH)
Anova(MH_aov, type = "II")
GCC_aov = lm(dry_mass ~ W_PO4,data = GCC)
Anova(GCC_aov, type = "II")
Cu_aov = lm(dry_mass ~ W_PO4,data = Cu)
Anova(Cu_aov, type = "II")
Dr_aov = lm(dry_mass ~ W_PO4,data = Dr)
Anova(Dr_aov, type = "II")
DV_aov = lm(dry_mass ~ W_PO4,data = DV)
Anova(DV_aov, type = "II")
#SIG
H20_aov = lm(dry_mass ~ W_PO4,data = H20)
Anova(H20_aov, type = "II")

DV_aov = lm(dry_mass ~ updown + W_PO4,data = DV)
Anova(DV_aov, type = "II")
DV_aov = lm(dry_mass ~ updown + W_PO4 + W_KH,data = DV)
Anova(DV_aov, type = "II")


cor(DV$W_PO4, DV$W_KH)

boxplot(dry_mass ~ W_PO4, data = DV) 


test = lm(W_KH ~ updown, data = Eco_Dat)


#Normailty Testing --- ((need to add the other one))

shapiro.test(Eco_Dat$dry_mass)

qqnorm(Eco_Dat$dry_mass);qqline(Eco_Dat$dry_mass, col = 2)

#between course ANOVA

model = aov(dry_mass ~ Courses, data = Eco_Dat_down)
summary(model)
mass_downAOV # SIG

?aov
tuk <- TukeyHSD(model, "Courses", ordered = FALSE, conf.level = 0.95)
tuk # dry_mass MH and DV > dry_mass Dr.  Dr is upriver from DV, therefore supports our hypothesis. 

Groups <- c("Deer Ridge Down", "Deer Ridge Up", "Merryhill Down", "Merryhill Up", "Guelph CC Down", 
            "Guelph CC Up", "Cutten Down", "Cutten Up", "Doon Valley Down", "Doon Valley Up", 
            "Deionized H2O", "Enriched H2O")


boxplot(dry_mass~Courses_Fixed, names = (Groups), data = Eco_Dat)
?boxplot

boxplot(dry_mass~updown, names = c("Doon Valley Downstream", "Doon Valley Upstream"), ylab="Algal Dry Mass", data = DV)

boxplot(dry_mass~Courses, names = c("Deer Ridge", "Doon Valley", "Merryhill"), xlab = "Courses", ylab="Algal Dry Mass", data = Eco_Dat_red)


mass_up <- lm(dry_mass ~ Courses, data = Eco_Dat_up)
mass_upAOV <- Anova(mass_up, type = "II")
mass_upAOV #UNSIG



#water chemistry

Eco_Dat_aov <- lm(dry_mass ~ W_PO4, data = Eco_Dat)
check <- Anova(Eco_Dat_aov, type = "II")
check





