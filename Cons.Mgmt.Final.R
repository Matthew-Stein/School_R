# Final Project

install.packages("auk")
library(auk)
library(tidyverse)
library(metafor)
library(dplyr)
library(reshape2)

ebd_CA_bkcchi_relAug.2018 <- read.delim("C:/Users/matth/OneDrive/BIOL 4150 - Wildlife Conservation & Management/Final Project/ebd/ebd_CA_bkcchi_relAug-2018.txt")

Blk_Chi_bc <- ebd_CA_bkcchi_relAug.2018 %>%
  filter(STATE == "British Columbia")
Blk_Chi_bc[, 9] <- as.numeric(as.character( Blk_Chi_bc[, 9] ))
years <- substr(Blk_Chi_bc$OBSERVATION.DATE, start = 1, stop = 4)
Blk_Chi_bc <- cbind(Blk_Chi_bc, years)
Blk_Chi_bc <- Blk_Chi_bc[, -c(1:8, 10:16, 20:33)]
Blk_Chi_bc <- Blk_Chi_bc[, -c(4:18)]
Blk_Chi_bc <- Blk_Chi_bc[complete.cases(Blk_Chi_bc), ]

write.table(Blk_Chi_bc,"BlkCChi_Reduced.txt",sep="\t",row.names=FALSE)

ebd_CA_mouchi_relAug.2018 <- read.delim("C:/Users/matth/OneDrive/BIOL 4150 - Wildlife Conservation & Management/Final Project/ebd/ebd_CA_mouchi_relAug-2018.txt")

M_Chi_bc <- ebd_CA_mouchi_relAug.2018 %>%
  filter(STATE == "British Columbia")
M_Chi_bc[, 9] <- as.numeric(as.character( M_Chi_bc[, 9] ))
years <- substr(M_Chi_bc$OBSERVATION.DATE, start = 1, stop = 4)
M_Chi_bc <- cbind(M_Chi_bc, years)
M_Chi_bc <- M_Chi_bc[, -c(1:8, 10:16, 20:33)]
M_Chi_bc <- M_Chi_bc[, -c(4:18)]
M_Chi_bc <- M_Chi_bc[complete.cases(M_Chi_bc), ]

write.table(M_Chi_bc,"MCHi_reduced.txt",sep="\t",row.names=FALSE)

#simplified Data

Blk_Chi_bc <- read.delim("C:/Users/matth/OneDrive/BIOL 4150 - Wildlife Conservation & Management/Final Project/BlkCChi_Reduced.txt")
M_Chi_bc <- read.delim("C:/Users/matth/OneDrive/BIOL 4150 - Wildlife Conservation & Management/Final Project/MCHi_reduced.txt")
View(Blk_Chi_bc)
View(M_Chi_bc)

Blk_counties <- Blk_Chi_bc %>% group_by(COUNTY) %>% summarise(N=n()) %>% data.frame()
Blk_counties_codes <- Blk_Chi_bc %>% group_by(COUNTY.CODE) %>% summarise(N=n()) %>% data.frame()
Blk_counties
#Found in 17 Counties (Some with V little Obs)

#Dataset too large to manipulate as a whole: 

#Breaks up the BC Dataset into county-specific datasets
Blk_c1 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-BN")
Blk_c2 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-CC")
Blk_c3 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-CK")
Blk_c4 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-EK")
Blk_c5 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-FV")
Blk_c6 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-GV")
Blk_c7 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-KB")
Blk_c8 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-KS")
Blk_c9 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-NO")
Blk_c10 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-OS")
Blk_c11 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-PC")
Blk_c12 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-SC")
Blk_c13 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-SL")
Blk_c14 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-SQ")
Blk_c15 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-ST")
Blk_c16 <- Blk_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-TN")


M_County <- M_Chi_bc %>% group_by(COUNTY) %>% summarise(N=n()) %>% data.frame()
M_County
M_County_Code <- M_Chi_bc %>% group_by(COUNTY.CODE) %>% summarise(N=n()) %>% data.frame()
M_County_Code
#Found in 12 Counties

M_Chi_bc %>% group_by(OBSERVATION.DATE) %>% summarise(N=n()) %>% data.frame()

M_c1 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-BN")
M_c2 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-CC")
M_c3 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-CK")
M_c4 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-EK")
M_c5 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-FF")
M_c6 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-FV")
M_c7 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-GV")
M_c8 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-KB")
M_c9 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-KS")
M_c10 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-NO")
M_c11 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-NR")
M_c12 <- M_Chi_bc %>%
  filter(COUNTY.CODE == "CA-BC-OS")

#maybe ill get this to work one day
for (i in 1:12){
  eval(parse(paste("M_c",i,sep=""))){
    year <- as.integer(as.vector(unique(M_c$years)))
    freq <- as.vector(table(M_c$years))
    tot <- length(year)
    tau <- numeric(length = tot)
    for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
    cnt <- aggregate(OBSERVATION.COUNT~years, M_c, sum)
    raw_counts <- cnt[,2]
    pop <- raw_counts/freq
    r <- numeric(length = tot)
    for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
    plot(r~year, type = "l")
    abline(lm(r ~ year))
    M_c_dat <- lm(r~year)
    }}

#Manual Data analysis
#**************BLACK CAP CHICKADEES**************** ALL YEARS

#Blk_c1
year <- as.integer(as.vector(unique(Blk_c1$years)))
freq <- as.vector(table(Blk_c1$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c1, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c1_dat <- lm(r~year)
Blk_c1_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c2
year <- as.integer(as.vector(unique(Blk_c2$years)))
freq <- as.vector(table(Blk_c2$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c2, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c2_dat <- lm(r~year)
Blk_c2_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c3
year <- as.integer(as.vector(unique(Blk_c3$years)))
freq <- as.vector(table(Blk_c3$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c3, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c3_dat <- lm(r~year)
Blk_c3_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c4
year <- as.integer(as.vector(unique(Blk_c4$years)))
freq <- as.vector(table(Blk_c4$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c4, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c4_dat <- lm(r~year)
Blk_c4_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c5
year <- as.integer(as.vector(unique(Blk_c5$years)))
freq <- as.vector(table(Blk_c5$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c5, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c5_dat <- lm(r~year)
Blk_c5_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c6
year <- as.integer(as.vector(unique(Blk_c6$years)))
freq <- as.vector(table(Blk_c6$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c6, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c6_dat <- lm(r~year)
Blk_c6_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c7
year <- as.integer(as.vector(unique(Blk_c7$years)))
freq <- as.vector(table(Blk_c7$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c7, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c7_dat <- lm(r~year)
Blk_c7_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c8
year <- as.integer(as.vector(unique(Blk_c8$years)))
freq <- as.vector(table(Blk_c8$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c8, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c8_dat <- lm(r~year)
Blk_c8_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c9
year <- as.integer(as.vector(unique(Blk_c9$years)))
freq <- as.vector(table(Blk_c9$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c9, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c9_dat <- lm(r~year)
Blk_c9_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c10
year <- as.integer(as.vector(unique(Blk_c10$years)))
freq <- as.vector(table(Blk_c10$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c10, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c10_dat <- lm(r~year)
Blk_c10_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c11
year <- as.integer(as.vector(unique(Blk_c11$years)))
freq <- as.vector(table(Blk_c11$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c11, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c11_dat <- lm(r~year)
Blk_c11_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c12
year <- as.integer(as.vector(unique(Blk_c12$years)))
freq <- as.vector(table(Blk_c12$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c12, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c12_dat <- lm(r~year)
Blk_c12_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c13
year <- as.integer(as.vector(unique(Blk_c13$years)))
freq <- as.vector(table(Blk_c13$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c13, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c13_dat <- lm(r~year)
Blk_c13_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c14
year <- as.integer(as.vector(unique(Blk_c14$years)))
freq <- as.vector(table(Blk_c14$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c14, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c14_dat <- lm(r~year)
Blk_c14_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c15
year <- as.integer(as.vector(unique(Blk_c15$years)))
freq <- as.vector(table(Blk_c15$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c15, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c15_dat <- lm(r~year)
Blk_c15_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#Blk_c16
year <- as.integer(as.vector(unique(Blk_c16$years)))
freq <- as.vector(table(Blk_c16$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c16, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
Blk_c16_dat <- lm(r~year)
Blk_c16_dat_10 <- lm(tail(r,10) ~ tail(year,10))



#**************MOUNTAIN CHICKADEES********** ALL YEARS

#M_C1
year <- as.integer(as.vector(unique(M_c1$years)))
freq <- as.vector(table(M_c1$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c1, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
M_c1_dat <- lm(r~year)
M_c1_dat_10 <- lm(tail(r,10) ~ tail(year,10))


#M_c2
year <- as.integer(as.vector(unique(M_c2$years)))
freq <- as.vector(table(M_c2$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c2, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
M_c2_dat <- lm(r~year)
M_c2_dat_10 <- lm(tail(r,10) ~ tail(year,10))


#M_c3
year <- as.integer(as.vector(unique(M_c3$years)))
freq <- as.vector(table(M_c3$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c3, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
M_c3_dat <- lm(r~year)
M_c3_dat_10 <- lm(tail(r,10) ~ tail(year,10))


#M_c4
year <- as.integer(as.vector(unique(M_c4$years)))
freq <- as.vector(table(M_c4$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c4, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
M_c4_dat <- lm(r~year)
M_c4_dat_10 <- lm(tail(r,10) ~ tail(year,10))


#M_c5
year <- as.integer(as.vector(unique(M_c5$years)))
freq <- as.vector(table(M_c5$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c5, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
M_c5_dat <- lm(r~year)
M_c5_dat_10 <- lm(tail(r,10) ~ tail(year,10))


#M_c6
year <- as.integer(as.vector(unique(M_c6$years)))
freq <- as.vector(table(M_c6$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c6, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
M_c6_dat <- lm(r~year)
M_c6_dat_10 <- lm(tail(r,10) ~ tail(year,10))


#M_c7
year <- as.integer(as.vector(unique(M_c7$years)))
freq <- as.vector(table(M_c7$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c7, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
M_c7_dat <- lm(r~year)
M_c7_dat_10 <- lm(tail(r,10) ~ tail(year,10))


#M_c8
year <- as.integer(as.vector(unique(M_c8$years)))
freq <- as.vector(table(M_c8$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c8, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(pop~year, type = "l")
plot(tail(r,10)~tail(year,10), type = "l")
abline(lm(tail(r,10)~tail(year,10)))
lm(tail(r,10)~tail(year,10))
M_c8_dat <- lm(r~year)
M_c8_dat_10 <- lm(tail(r,10) ~ tail(year,10))


#M_c9
year <- as.integer(as.vector(unique(M_c9$years)))
freq <- as.vector(table(M_c9$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c9, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
M_c9_dat <- lm(r~year)
M_c9_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#M_c10
year <- as.integer(as.vector(unique(M_c10$years)))
freq <- as.vector(table(M_c10$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c10, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
M_c10_dat <- lm(r~year)
M_c10_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#M_c11
year <- as.integer(as.vector(unique(M_c11$years)))
freq <- as.vector(table(M_c11$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c11, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
M_c11_dat <- lm(r~year)
M_c11_dat_10 <- lm(tail(r,10) ~ tail(year,10))

#M_c12
year
year <- as.integer(as.vector(unique(M_c12$years)))
freq <- as.vector(table(M_c12$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, M_c12, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
M_c12_dat <- lm(r~year)
M_c12_dat_10 <- lm(tail(r,10) ~ tail(year,10))


#End of Mountain Chickadees

table(M_Chi_bc$years)
table(Blk_Chi_bc$years)
table(Blk_c1$years)


year <- as.integer(as.vector(unique(Blk_c1$years)))
freq <- as.vector(table(Blk_c1$years))
tot <- length(year)
tau <- numeric(length = tot)
for (t in 1:tot-1){tau[t] <- year[t+1] - year[t]}
cnt <- aggregate(OBSERVATION.COUNT~years, Blk_c1, sum)
raw_counts <- cnt[,2]
pop <- raw_counts/freq
r <- numeric(length = tot)
for(t in 1:tot-1){r[t] <- (log(pop[t+tau[t+1]]/pop[t])/tau[t+1])}
plot(r~year, type = "l")
abline(lm(r ~ year))
plot(tail(r,10)~tail(year,10), type = "l")
abline(lm(tail(r,15) ~ tail(year,15)))

abline(lm(tail(r,10) ~ tail(year,10)))


par(mfrow=c(1,1))
#multiple plots or 1
plot(pop~year)
plot(r~year, type = "l")
abline(lm(r ~ year))
lm(r~year)
sum(r)
?plot
mean(r)
abline(r~year)
#population counts - total obs in year
Pop 

#RESULTS


Blk_c1_dat
Blk_c2_dat
Blk_c3_dat
Blk_c4_dat
Blk_c5_dat
Blk_c6_dat
Blk_c7_dat
Blk_c8_dat
Blk_c9_dat
Blk_c10_dat
Blk_c11_dat
Blk_c12_dat
Blk_c13_dat
Blk_c14_dat
Blk_c15_dat
Blk_c16_dat

Blk_c1_dat_10
Blk_c2_dat_10
Blk_c3_dat_10
Blk_c4_dat_10
Blk_c5_dat_10
Blk_c6_dat_10
Blk_c7_dat_10
Blk_c8_dat_10
Blk_c9_dat_10
Blk_c10_dat_10
Blk_c11_dat_10
Blk_c12_dat_10
Blk_c13_dat_10
Blk_c14_dat_10
Blk_c15_dat_10
Blk_c16_dat_10

M_c1_dat
M_c2_dat
M_c3_dat
M_c4_dat
M_c5_dat
M_c6_dat
M_c7_dat
M_c8_dat
M_c9_dat
M_c10_dat
M_c11_dat
M_c12_dat

M_c1_dat_10
M_c2_dat_10
M_c3_dat_10
M_c4_dat_10
M_c5_dat_10
M_c6_dat_10
M_c7_dat_10
M_c8_dat_10
M_c9_dat_10
M_c10_dat_10
M_c11_dat_10
M_c12_dat_10

Blk_cR <- c(
as.numeric(Blk_c1_dat$coefficients[2]),
as.numeric(Blk_c2_dat$coefficients[2]),
as.numeric(Blk_c3_dat$coefficients[2]),
as.numeric(Blk_c4_dat$coefficients[2]),
as.numeric(Blk_c5_dat$coefficients[2]),
as.numeric(Blk_c6_dat$coefficients[2]),
as.numeric(Blk_c7_dat$coefficients[2]),
as.numeric(Blk_c8_dat$coefficients[2]),
as.numeric(Blk_c9_dat$coefficients[2]),
as.numeric(Blk_c10_dat$coefficients[2]),
as.numeric(Blk_c11_dat$coefficients[2]),
as.numeric(Blk_c12_dat$coefficients[2]),
as.numeric(Blk_c13_dat$coefficients[2]),
as.numeric(Blk_c14_dat$coefficients[2]),
as.numeric(Blk_c15_dat$coefficients[2]),
as.numeric(Blk_c16_dat$coefficients[2]))
Blk_cR

Blk_cR10 <- c(
  as.numeric(Blk_c1_dat_10$coefficients[2]),
  as.numeric(Blk_c2_dat_10$coefficients[2]),
  as.numeric(Blk_c3_dat_10$coefficients[2]),
  as.numeric(Blk_c4_dat_10$coefficients[2]),
  as.numeric(Blk_c5_dat_10$coefficients[2]),
  as.numeric(Blk_c6_dat_10$coefficients[2]),
  as.numeric(Blk_c7_dat_10$coefficients[2]),
  as.numeric(Blk_c8_dat_10$coefficients[2]),
  as.numeric(Blk_c9_dat_10$coefficients[2]),
  as.numeric(Blk_c10_dat_10$coefficients[2]),
  as.numeric(Blk_c11_dat_10$coefficients[2]),
  as.numeric(Blk_c12_dat_10$coefficients[2]),
  as.numeric(Blk_c13_dat_10$coefficients[2]),
  as.numeric(Blk_c14_dat_10$coefficients[2]),
  as.numeric(Blk_c15_dat_10$coefficients[2]),
  as.numeric(Blk_c16_dat_10$coefficients[2]))

M_cR <- c(
as.numeric(M_c1_dat$coefficients[2]),
as.numeric(M_c2_dat$coefficients[2]),
as.numeric(M_c3_dat$coefficients[2]),
as.numeric(M_c4_dat$coefficients[2]),
as.numeric(M_c5_dat$coefficients[2]),
as.numeric(M_c6_dat$coefficients[2]),
as.numeric(M_c7_dat$coefficients[2]),
as.numeric(M_c8_dat$coefficients[2]),
as.numeric(M_c9_dat$coefficients[2]),
as.numeric(M_c10_dat$coefficients[2]),
as.numeric(M_c11_dat$coefficients[2]),
as.numeric(M_c12_dat$coefficients[2]))

M_cR10 <- c(
  as.numeric(M_c1_dat_10$coefficients[2]),
  as.numeric(M_c2_dat_10$coefficients[2]),
  as.numeric(M_c3_dat_10$coefficients[2]),
  as.numeric(M_c4_dat_10$coefficients[2]),
  as.numeric(M_c5_dat_10$coefficients[2]),
  as.numeric(M_c6_dat_10$coefficients[2]),
  as.numeric(M_c7_dat_10$coefficients[2]),
  as.numeric(M_c8_dat_10$coefficients[2]),
  as.numeric(M_c9_dat_10$coefficients[2]),
  as.numeric(M_c10_dat_10$coefficients[2]),
  as.numeric(M_c11_dat_10$coefficients[2]),
  as.numeric(M_c12_dat_10$coefficients[2]))

Blk_cR
Blk_cR10
M_cR
M_cR10

M_County <- M_Chi_bc %>% group_by(COUNTY) %>% summarise(N=n()) %>% data.frame()
M_County
Blk_counties <- Blk_Chi_bc %>% group_by(COUNTY) %>% summarise(N=n()) %>% data.frame()
Blk_counties

table(M_c8$years)
table(Blk_Chi_bc$years)
numb <- table(Blk_Chi_bc$years)
as.vector(numb)
barplot(numb, xlab = "Years", ylab = "Total observations")

cor.test(test_corr$M_r, test_corr$Blk_r, data = test_corr, paired = TRUE, conf.level = 0.95, alternative = "two.sided")
?cor.test
plot(test_corr$M_r, test_corr$Blk_r)

