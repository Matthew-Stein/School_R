
algal_r_dat <- read.csv("C:/Users/matth/Desktop/algal_r_dat.csv")
View(algal_r_dat)

fp_mass <- algal_r_dat$fp_mass[1:10]
fp_mass_av <- mean(fp_mass)

algal_dw_raw <- algal_r_dat$dry_mass
algal_dw_true <- algal_dw_raw - fp_mass_av
algal_dw_true
dr_d <- algal_dw_true[1:5]
dr_u <- algal_dw_true[6:10]
mh_d <- algal_dw_true[11:15]
mh_u <- algal_dw_true[16:20]
gcc_d <- algal_dw_true[21:25]
gcc_u <- algal_dw_true[26:30]
cu_d <- algal_dw_true[31:35]
cu_u <- algal_dw_true[36:40]
dv_d <- algal_dw_true[41:45]
dv_u <- algal_dw_true[46:50]
h2o <- algal_dw_true[51:55]
np <- algal_dw_true[56:60]

dr <- t.test(dr_u, dr_d, alternative = "two.sided", paired = FALSE, conf.level = 0.95)
dr
mh <- t.test(mh_u, mh_d, alternative = "two.sided", paired = FALSE, conf.level = 0.95)
mh
gcc <- t.test(gcc_u, gcc_d, alternative = "two.sided", paired = FALSE, conf.level = 0.95)
gcc
cu <- t.test(cu_u, cu_d, alternative = "two.sided", paired = FALSE, conf.level = 0.95)
cu
iw <- t.test(h2o, np, alternative = "two.sided", paired = FALSE, conf.level = 0.95)
iw

aov