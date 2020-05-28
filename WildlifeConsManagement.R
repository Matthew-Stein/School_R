#Wildlife Conservation and Management
#Thurs sept 13 2018

#"c" means "concatenate"  - to put together
rowdata<-c(142,155,145,149,149,156,134,127,128,168,126,156)
coldata<-c(12,29,58,82,105,135,150,203,222,222,250,267)
sum(rowdata)
sum(coldata)
y<-sample(rowdata,4)
#random selection : sample(data, # selected, replace = T/F)

y1<-y[1]
y2<-y[2]
y3<-y[3]
y4<-y[4]

sumy<-y1+y2+y3+y4

sumysq<-(y1*y1)+(y2*y2)+(y3*y3)+(y4*y4)
sumysq<-(y1^2)+(y2^2)+(y3^2)+(y4^2)
#Equivalent expressions

a<-10
n<-2
totaln<-12
samplearea<-a*n
totarea<-100
D<-sumy/samplearea
#all just example values

SED<-(1/a)*sqrt((sumysq-(sumy^2)/n)/(n*(n-1)))
#Without the area correction factor

SEDcf <- (sqrt(1-(samplearea/totarea))) (1/a)*sqrt((sumysq-(sumy^2)/n)/(n*(n-1)))
#With correction factor, when Sample area > ~5% of Total Area

rm(list=ls())
#wipes everything

N <- numeric(10)
Year <- numeric(10)
N[1] <- 7.89
lambda <- 1.39
Year <- 1:10
for(t in 2:10){N[t] <- N[t-1]*lambda}
#Geometric growth loop

plot(Year, N, 'b')
#Points w/ connecting lines
plot(Year, N, 'p')
#just points
plot(Year, N, 'l')
#just lines

### Assignment #1: 

counts <- c(39, 36, 30, 36, 27, 32, 22, 39, 31, 35)

n<-9
#change each repetition of calc.

totalarea<-10
a<-1
totaln<-10
samplearea <- a*n

y<-sample(counts, n, replace = F)

y1<-y[1]
y2<-y[2]
y3<-y[3]
y4<-y[4]
y5<-y[5]
y6<-y[6]
y7<-y[7]
y8<-y[8]
y9<-y[9]
#y10<-y[10]

sumy<-y1+y2+y3+y4+y5+y6+y7+y8+y9#+y10

D <- sumy/samplearea

sumysq<-(y1*y1)+(y2*y2)+(y3*y3)+(y4*y4)+(y5*y5)+(y6*y6)+(y7*y7)+(y8*y8)+(y9*y9)#+(y10*y10)

SEDcf <- (sqrt(1-(samplearea/totalarea)))*(1/a)*sqrt((sumysq-(sumy^2)/n)/(n*(n-1)))

Y<-totalarea*D
SEy <- totalarea*SEDcf

D
SEDcf
Y
SEy


#Model Eval stuff

#Model 1 : Ricker Logistic Model

#sigma
rmax <- 1
slope <- -0.0008
K <- -rmax/slope
rpredict <- rmax*(1-n/K)
#Havent defined n right now also n or N?
resid1 <- r-rpredict
sqresid1 <- resid1*resid1
sumsq1 <- sum(sqresid1)
sigma1 <- sqrt(sumsq1/n)
#is this n or N????

#Likelihood
L1 <- n*(log(sigma1)+0.5*log(2*pi))+sum(sqresid1/(2*sigma1^2))
p1 <- 3
AIC1 <- 2*L1+2*p1*n/(n-p1-1)

#Model 2: Geometric Growth Model - Constant Rate
resid2 <- r-mean(r)
sqresid2 <- resid2*resid2
sumsq2 <- sum(sqresid2)
sigma2 <- sqrt(sumsq2/n)
#Likelihood
L2 <- n*(log(sigma2)+0.5*log(2*pi))+sum(sqresid2/(2*sigma2^2))
p2 <- 2
AIC2 <- 2*L2+2*p2*n/(n-p2-1)

#Model 3: Theta-Logistic Model   ----- Not included in assignment 2
den <- numeric(17)
den <- seq(0,1600,100)
model3 <- nls(r~rmax*(1-(n/K)^theta),start = list(rmax=0.2, K=1150, theta=5))
summary(model3)
rmax <- 0.105
K <- 1241.039
theta <- 5.94
rpredict3 <- rmax*(1-(n/K)^theta)

resid3 <- r-rpredict3
sqresid3 <- resid3*resid3
sumsq3 <- sum(sqresid3)
sigma3 <- sqrt(sumsq3/n)
#Likelihood
L3 <- n*(log(sigma3)+0.5*log(2*pi))+sum(sqresid3/(2*sigma3^2))
p3 <- 4
AIC3 <- 2*L3+2*p3*n/(n-p3-1)

rm(list=ls())

### Assignment 2 ---

N1add <- c(1202, 1252, 1272, 1300, 1280, 1269, 1261, 1288, 1241, 1252, 1289, 1238, 1298, 1282, 1377, 1322)
#N1add has 1 additional year of data (n=16) to calc final year's r for N (n=15).
N <- c(1202, 1252, 1272, 1300, 1280, 1269, 1261, 1288, 1241, 1252, 1289, 1238, 1298, 1282, 1377)
#numbers in 1000's
years <- c(1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007)
plot(N~years)
tau <- 1
n <- 15
r <- numeric(15)
for(t in 1:15){r[t] <- (log(N1add[t+tau]/N1add[t])/tau)}
plot(r~N, xlab = "Population Abundance (Thousands)", ylab = "Exponential Rate of Increase (r)")

#Model 1 : Ricker Logistic Model

#sigma
fit <- lm(r~N)
summary(fit)
rmax <-  0.7092921
slope <- -0.0005520
K <- -rmax/slope
rpredict <- rmax*(1-N/K)
resid1 <- r-rpredict
sqresid1 <- resid1*resid1
sumsq1 <- sum(sqresid1)
sigma1 <- sqrt(sumsq1/n)

#Likelihood
L1 <- n*(log(sigma1)+0.5*log(2*pi))+sum(sqresid1/(2*sigma1^2))
p1 <- 3
AIC1 <- 2*L1+2*p1*n/(n-p1-1)

#Model 2: Geometric Growth Model - Constant Rate
resid2 <- r-mean(r)
sqresid2 <- resid2*resid2
sumsq2 <- sum(sqresid2)
sigma2 <- sqrt(sumsq2/n)
#Likelihood
L2 <- n*(log(sigma2)+0.5*log(2*pi))+sum(sqresid2/(2*sigma2^2))
p2 <- 2
AIC2 <- 2*L2+2*p2*n/(n-p2-1)
AIC1
AIC2
#Akaike Weights
deltaAIC2 <- abs(AIC1 - AIC2)
deltaAIC1 <- 0
RL1 <- exp(-0.5*deltaAIC1)
RL2 <- exp(-0.5*deltaAIC2)
w1 <- RL1 / (RL1 + RL2)
w2 <- RL2 /(RL1 + RL2)
w1 
w2

#done


### Assignment 3 ---

m <- c(0.0, 1.5, 2.2, 3.4, 0.8)
#reproduction
p <- c(0.40, 0.71, 0.86, 0.49, 0.14)
#Survival

A <- matrix(0, nr=5, ncol =5)

A[1,2] <- p[2]*m[2]
A[1,3] <- p[3]*m[3]
A[1,4] <- p[4]*m[4]
A[1,5] <- p[5]*m[5]
A[2,1] <- p[1]
A[3,2] <- p[2]
A[4,3] <- p[3]
A[5,4] <- p[4]
A[5,5] <- p[5]
A

#[,1]  [,2]  [,3]  [,4]  [,5]
#[1,]  0.0 1.065 1.892 1.666 0.112
#[2,]  0.4 0.000 0.000 0.000 0.000
#[3,]  0.0 0.710 0.000 0.000 0.000
#[4,]  0.0 0.000 0.860 0.000 0.000
#[5,]  0.0 0.000 0.000 0.490 0.140

#lit

#Total Population
tmax <- 30
t <- 1:tmax
n <- matrix(0, nr = 5, ncol = tmax)
n[,1] <- c(0, 0, 10, 0, 0) #variable
#test the optimal comp. of female ages
for (i in 1:(tmax-1)){n[,i+1] <- A%*%n[,i]}
N <- numeric(tmax)
for (i in 1:tmax){N[i] <- sum (n[,i])}
plot(t-1, N, type = 'l', xlab = "Years", ylab = "Population Density (N)")

#with individual age categories
plot(t-1, n[1,], type = 'l', cex = 1.5, xlab = "t", ylab = 'n')
lines(t-1, n[2,], lty = 2)
lines(t-1, n[3,], lty = 3)
lines(t-1, n[4,], lty = 4)
lines(t-1, n[5,], lty = 5)
legend("topleft", bg = "transparent", legend=c("Age 1", "Age 2", "Age 3", "Age 4", "Age 5" ),
       lty=1:5, cex=0.7,
       title="Age Classes", text.font=3)

# % composition of each age category 
w <- matrix(0, nr = 5, ncol = tmax)
for (i in 1:tmax){w[,i] <- n[,i]/N[i]}
# ^ original code is written as "1:tmax-1", causes values to crash to 0 at tmax, this was better.
plot(t-1, w[1,], type = "l", ylab = "Proportion of Total Population", xlab = "Years")
lines(t-1, w[2,], lty = 2)
lines(t-1, w[3,], lty = 3)
lines(t-1, w[4,], lty = 4)
lines(t-1, w[5,], lty = 5)
legend("topright", bg = "transparent", legend=c("Age 1", "Age 2", "Age 3", "Age 4", "Age 5" ),
       lty=1:5, cex=0.7,
       title="Age Classes", text.font=3)


#QUESTION 1 
w[1,30]
# 0.5403927
w[2,30]
# 0.1937024
w[3,30]
# 0.1232404
w[4,30]
# 0.09497685
w[5,30]
# 0.04768754

#Checking:
sum(w[1:5,30])
#1

#QUESTION 2 

eigen(A)

#eigen() decomposition
#$values
#[1]  1.1159263+0.0000000i   <<< Largest: 1.12 = 12% growth/year.
#[2] -0.2003235+0.6849137i
#[3] -0.2003235-0.6849137i
#[4] -0.6868826+0.0000000i
#[5]  0.1116033+0.0000000i

#QUESTION 3

tmax <- 10
t <- 1:tmax
n <- matrix(0, nr = 5, ncol = tmax)
n[,1] <- c(0, 0, 0, 0, 10) #variable
#test the optimal comp. of female ages
for (i in 1:(tmax-1)){n[,i+1] <- A%*%n[,i]}
N <- numeric(tmax)
for (i in 1:tmax){N[i] <- sum (n[,i])}
N[10]



### Assignment #4 --- Habitat Use

hab <- c("RCF", "ROF", "RSL", "RGL", "UCF", "UOF", "USL", "UGL", "UDL")
habN <- 1:9
habA <- c(12882.457, 8721.529, 1717.702, 79.245, 868.845, 578.31, 173.52, 299.623, 416.609)
Atot <- 25737.841
sum(habA)
habProp_percent <- c(50.05, 33.89, 6.67, 0.31, 3.38, 2.25, 0.67, 1.16, 1.62)
habProp <- habProp_percent/100
sum(habP)
habUse_percent <- c(13.84, 10.09, 7.31, 1.51, 12.86, 6.58, 8.04, 23.98, 15.79)
habUse <- habUse_percent/100
sum(habUse)
#chi2 critical values for a=0.05, Std_chi2[1] for 1df, Std_chi2[2] for 2df, etc. 
Std_chi2 <- c(3.841, 5.991, 7.815, 9.488, 11.070, 12.592, 14.067, 15.507, 16.919) 
nElk <- 35
nObs <- 1207
ExpFreq <- (nObs * habProp)
ObsFreq <- (habUse * nObs)

#Double Checking
sum(ExpFreq)
sum(ObsFreq)
#Both = 1207
ExpFreq
#604.1035 409.0523  80.5069   3.7417  40.7966  27.1575   8.0869  14.0012  19.5534
ObsFreq
#167.0488 121.7863  88.2317  18.2257 155.2202  79.4206  97.0428 289.4386 190.5853

#Chi2 - Calculations
x <- habN
O <- ObsFreq
E <- ExpFreq
diff <- O - E
diff_squared <- diff^2 
chi2_components <- diff_squared / E 
chi2 <- sum(diff_squared / E )
chi2
# = 8889.286
Std_chi2[8] #8 df
# = 15.05


# ************************ ASSIGNMENT 5 **********************







#ricker model stuff

#Assignment 5
N <- c(1:1750)
Fn <- N*exp(0.39*(1-(N/1619)))
R <- Fn-N
plot(R~N, type = "l", ylab = "Net Recruitment", xlab = "Population Size")
abline( h=0 , col = "gray")
#Max Sus. yield
msy <- max(R) # = 174.74

abline(h=174.74)




 




#from previous
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



















