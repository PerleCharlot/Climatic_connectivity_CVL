library(ggplot2)
library(pscl)
library(dplyr)
library(data.table)

load(file="C:/Users/perle/OneDrive/Bureau/RDATA/Metrics/gain.RDATA")
patches.dt <- fread("C:/Users/perle/OneDrive/Bureau/RDATA/Metrics/Inputs/core_PA.csv")

# ALL <- data.frame()
# setwd("C:/Users/perle/OneDrive/Bureau/RDATA/propagation/data_all_Austria/mean_500m/Output")
# for(scenario in c(26,45,85)){
#   for(year in c(2030,2040,2050,2060,2070,2080)){
#     path <- paste("AllData_rcp",scenario,"_",year,"_COR.csv", sep="")
#     name <- paste("Cor_",scenario,"_",year,sep="") # "Cor_26_2030
#     tab <- assign(name,fread(path))
#     tab <- tab[,c(2,16)]
#     dta <- data.frame(tab[,c(1)],tab[,c(2)],
#                       c(rep(year,length(tab$Cores))), c(rep(scenario,length(tab$Cores))))
#     names(dta) <- c("core_ID","Success","year","scenario")
#     ALL <- rbind(ALL, dta)
#   }}
# save(ALL,file="C:/Users/perle/OneDrive/Bureau/RDATA/Metrics/success.RDATA" )

load(file="C:/Users/perle/OneDrive/Bureau/RDATA/Metrics/success.RDATA")
ALL$successful[which(ALL$Success<0)] <- 0
ALL$successful[which(ALL$Success>=0)] <- 1
mergeons <-merge(ALL, patches.dt, by="core_ID")

sucperall <- mergeons  %>%
                group_by(scenario, year, PA_ID) %>%
                summarise(sum(successful))#combien de patchs sont successful au sein d'un PA 
#regroupe les patches par scénario, par année et par PA
totperPA <- patches.dt %>%
                group_by(PA_ID) %>%
                summarise(n_distinct(core_ID))
final <- merge(sucperall, totperPA, by="PA_ID")
names(final) <- c("PA_ID","scenario","year","nb_suc_patch","nb_tot_patch")
final$year <- factor(final$year, levels=c("2030","2040","2050","2060","2070","2080"))
final$scenario <- factor(final$scenario, levels=c("26","45","85"),
                         labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))
final2 <- merge(final, ALL_Gain, by=c("PA_ID","year","scenario"))
final2 <- final2[,c(1:5,10)]

#zero inflated poisson regression
m3 <- zeroinfl(Gain ~  nb_suc_patch + scenario + year, data = final2)
summary(m3)

m4 <- zeroinfl(Gain ~  nb_tot_patch + scenario + year, data = final2)
summary(m4)

subfin = subset(final2, final2$year==2080)
subfin = subset(subfin, final2$scenario=="RCP 2.6")
subfin = na.omit(subfin)
subfin$proportion = subfin$nb_suc_patch/ subfin$nb_tot_patch

m1 <- zeroinfl(Gain ~  nb_tot_patch, data = subfin)
summary(m1)
m2 <- zeroinfl(Gain ~  nb_tot_patch + nb_suc_patch, data = subfin)
summary(m2)
m3 <- zeroinfl(Gain ~  nb_suc_patch, data = subfin)
summary(m3)
m4 <- zeroinfl(Gain ~  proportion, data = subfin)
summary(m4)

m5 <- zeroinfl(Gain ~  nb_tot_patch*nb_suc_patch, data = subfin)
summary(m5)

vuong(m4, m3)#m4 best

mnull <- update(m3, . ~ 1)
pchisq(2 * (logLik(m3) - logLik(mnull)), df = 3, lower.tail = FALSE)
#normal poisson regression
p3 <- glm(Gain ~ nb_suc_patch + nb_tot_patch + scenario + year, data = final2, family = poisson)
vuong(p3, m3)
#we can see that our test statistic is significant, 
#indicating that the zero-inflated model is superior to the standard Poisson model.

newdata1 <- expand.grid(1:max(final2$nb_tot_patch),
                        factor(c("RCP 2.6","RCP 4.5","RCP 8.5")), 
                        factor(c("2030","2040","2050","2060","2070","2080")))
colnames(newdata1) <- c("nb_suc_patch","scenario", "year")
newdata1 <- subset(newdata1, subset=(year==2080))
newdata1 <- subset(newdata1, subset=(scenario =="RCP 2.6"))
newdata1$phat <- predict(m3, newdata1)

ggplot(newdata1, aes(x = nb_suc_patch, y = phat)) +
  geom_point() +
  geom_line() +
  labs(x = "Number of successful patches", y = "Predicted Gain")+facet_wrap(~scenario) 


newdata1 <- expand.grid(1:max(final2$nb_tot_patch),
                        factor(c("RCP 2.6","RCP 4.5","RCP 8.5")), 
                        factor(c("2030","2040","2050","2060","2070","2080")))
colnames(newdata1) <- c("nb_tot_patch","scenario", "year")
newdata1 <- subset(newdata1, subset=(year==2080))
newdata1 <- subset(newdata1, subset=(scenario =="RCP 2.6"))
newdata1$phat <- predict(m4, newdata1)

ggplot(newdata1, aes(x = nb_tot_patch, y = phat)) +
  geom_point() +
  geom_line() +
  labs(x = "Number of patches", y = "Predicted Gain")
facet_wrap(~scenario) 



newdata1 <- expand.grid(seq(0,1,0.01))
colnames(newdata1) <- c("proportion")
newdata1$phat <- predict(m4, newdata1)
ggplot(newdata1, aes(x = proportion, y = phat)) +
  geom_point() +
  geom_line() +
  labs(x = "Proportion of successful patches per PA", y = "Predicted Gain")

ggplot(subfin, aes(x = proportion, y = Gain)) +
  geom_point() +
  labs(x = "Proportion of successful patches per PA", y = "Gain")

ggplot(subfin, aes(x = nb_suc_patch, y = Gain)) +
  geom_point() +
  labs(x = "Number of successful patches per PA", y = "Gain")

ggplot(subfin, aes(x = nb_tot_patch, y = Gain)) +
  geom_point() +
  labs(x = "Number of patches per PA", y = "Gain")
#if there is a gain, it happens in a PA with fewer patches
ggplot(data =subset(subfin, subfin$Gain>0) , aes(x = nb_tot_patch, y = Gain)) +
  geom_point() +
  labs(x = "Number of patches per PA", y = "Gain")

ggplot(data =subset(subfin, subfin$Gain>0) , aes(x = nb_suc_patch, y = Gain)) +
  geom_point() +
  labs(x = "Number successful of patches per PA", y = "Gain")


m6 <- glm(Gain ~  nb_tot_patch+nb_suc_patch, data = subset(subfin, subfin$Gain>0),
          family = "poisson")
summary(m6)#AIC = 4242.8
m7 <- glm(Gain ~  nb_tot_patch, data = subset(subfin, subfin$Gain>0),
          family = "poisson")
summary(m7)#AIC = 4245.5
m8 <- glm(Gain ~  nb_suc_patch, data = subset(subfin, subfin$Gain>0),
          family = "poisson")
summary(m8)#AIC = 4250.6
m9 <- glm(Gain ~  nb_tot_patch*nb_suc_patch, data = subset(subfin, subfin$Gain>0),
          family = "poisson")
summary(m9)#AIC = 4235.2
#all models are nested
summary(subset(subfin, subfin$Gain>0))
with(subset(subfin, subfin$Gain>0), tapply(Gain, nb_tot_patch, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

anova(m8,m7,m6,m9,test="Chisq")#m9 best
#best model = provides less residual sum of squares
#here: Gain ~ nb_tot_patch * nb_suc_patch

summary(m9)
#This means that the expected log count for a one-unit increase in nb_suc is 0.07 
#This means that the expected log count for a one-unit decrease in nb_tot is 0.03 
cov.m9 <- vcovHC(m9, type="HC0")
std.err <- sqrt(diag(cov.m9))
r.est <- cbind(Estimate= coef(m9), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m9)/std.err), lower.tail=FALSE),
               LL = coef(m9) - 1.96 * std.err,
               UL = coef(m9) + 1.96 * std.err)

r.est
library(msm)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                 coef(m9), cov.m9)

## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s

rexp.est
#The exponentiated nb_suc_patch coefficient (1.07711) is the multiplicative term 
#to use to calculate the estimated Gain when nb_suc_patch increases by 1 unit.

#The exponentiated nb_tot_patch coefficient (0.9697731) is the multiplicative term 
#to use to calculate the estimated Gain when nb_tot_patch increases by 1 unit.

#When a PA (with at least one patch succesful) has one more patch, 
#estimated Gain are multiplied by 0,97 (thus decreasing by 3%)
#Whereas
#When a PA (with at least one patch succesful) has one more successful patch, 
#estimated Gain are multiplied by 1,07 (thus increasing by 7%)

