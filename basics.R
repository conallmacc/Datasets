
dema<-read.csv("c:\\Users\\conal\\Downloads\\Dema Data.csv", header = TRUE)

dema$ExpGroup<-factor(dema$ExpGroup, labels = c("Group1","Group2"),levels = c(1,2))

dema$DayCycle<-factor(dema$DayCycle)
dema$Phase3split<-factor(dema$Phase3split, levels=c(1,2,3), labels = c("early_folic", "ovulatory", "luteal"))

which(dema==999)
dema[dema==999]<-NA
install.packages("Rcpp")
library(Rcpp)
remove.packages("psycho")
library(ggplot2)
library(tidyverse)
library(psycho)
install.packages("psycho")

dema1<-dema %>%
  mutate(zE2 = psycho::standardize(E2_level)) %>%
  mutate(zTestos=psycho::standardize((T_level))) %>%
  mutate(zP=psycho::standardize(P_level)) %>%
  mutate(zE_zP= zE2/zP)

library(lme4)
lm()

hormones$Hormone<-factor(hormones$Hormone, labels = c("E2", "Progesterone", "Testosterone"))

hormones$ID[1]


cor.test(dema1$zE2, dema1$zTestos)

hormones<-filter(hormones,!is.na(Hormonelevel))

library(modelr)

hormones<-hormones %>%
  add_predictions(lm_model)%>%
  add_residuals(lm_model)

ggplot(hormones, aes(Hormonelevel, resid, colour = Hormone)) + 
  geom_point()+
  facet_grid(~Hormone)


ggplot(data = hormones)+geom_point(data=hormones,aes(x=Hormonelevel, y=VMTotalTime))+
  geom_line( aes(y=pred, x=Hormonelevel))+
  facet_grid(~Hormone)

lm_model<-lm(VMTotalTime~1+Hormonelevel:Hormone, data = hormones)



hormones<-gather(dema1, value=Hormonelevel, key=Hormone, c(zE2,zP,zTestos))


lm_model$fitted.values

