
#Daphnia Project

# Import "Raw_dat.csv" and "offspring.csv" - stored in Desktop/Invert/Lab Report.



## Adult Survival
days <- 1:15


cntrl <- c(100, 95.78947368, 83.15789474, 78.94736842, 76.84210526, 73.68421053, 
            57.89473684, 51.57894737, 49.47368421, 46.31578947, 46.31578947, 43.15789474, 
            40, 37.89473684, 36.84210526)
expmt <- c(100, 97.89473684, 84.21052632, 82.10526316, 77.89473684, 72.63157895, 65.26315789,
           57.89473684, 57.89473684, 56.84210526, 55.78947368, 55.78947368, 55.78947368, 55.78947368,
           55.78947368)
t1 <- t.test(cntrl, expmt,
            alternative = "two.sided", paired = TRUE, mu = 0, conf.level = 0.95)
t1


plot(cntrl~days, type = 'l', xlab = "Days", ylab = "Survival (%)")
lines(expmt~days, type = 'l', lty=2)
legend("topright", bg = "transparent", legend=c("Control", "Treatment"),
       lty=1:2, cex=0.7,
       title="Legend", text.font=3)


#import Raw_dat.csv
summary(Raw_dat)

C_liv_days <- Raw_dat$ï..C_liv_days
C_molt_obs <- Raw_dat$C_molt_obs
C_moltldays <- Raw_dat$C_obs_ldays
T_liv_days <- Raw_dat$T_liv_days
T_molt_obs <- Raw_dat$T_molt_obs
T_moltldays <- Raw_dat$T_obs_ldays

#Molts/Living days calc:
t2 <- t.test(T_moltldays, C_moltldays,
       alternative = "two.sided", paired = FALSE, mu = 0, conf.level = 0.95)
t2

C_moltldays
T_moltldays
stderr(C_moltldays)
mean(C_moltldays)
mean(T_moltldays)
sd(C_moltldays)
sd(T_moltldays)
std <- function(x) sd(x)/sqrt(length(x))
std(C_moltldays)
std(T_moltldays)


#result: Welch's t test Treatment molts/living days sig larger than control (t=2.2861, df=183.51, p=0.02)

#Average mortality
#import offspring.csv
C_Alive <- offspring$C_Alive
C_Dead <-  offspring$C_Dead
T_Alive <-  offspring$T_Alive
T_Dead <-  offspring$T_Dead

C_Alive
C_Dead
T_Alive
T_Dead


G = (sum((C_Alive[1:46] + C_Dead[1:46])))/46
C_Av = numeric(47)
for(i in 1:47){ C_Av[i] <- G}

F = (sum((T_Alive[1:47] + T_Dead[1:47])))/47
T_Av = numeric(47)
for(i in 1:47){ T_Av[i] <- F}

C_Mortish <- C_Dead/(C_Av)
T_Mort <- T_Dead/(T_Av)
C_Mort <- C_Mortish[1:46]
C_Mort
T_Mort

C_Mort_av <- 0.034883721
T_Mort_av <- 0.014270033
mean(C_Mort)
mean(T_Mort)
#(HEY LOOK THEY MATCH NEOW)

t3 <- t.test(C_Mort, T_Mort,
             alternative = "two.sided", paired = FALSE, mu = 0, conf.level = 0.95)
t3

C_offspring <- C_Alive+C_Dead
T_offspring <- T_Alive+T_Dead
C_offspring
T_offspring

t4 <- t.test(C_offspring, T_offspring,
             alternative = "two.sided", paired = FALSE, mu = 0, conf.level = 0.95)

t4


t1
t2
t3
t4



