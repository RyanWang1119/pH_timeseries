#
library(readr)
library(here)
library(tidyverse)
library(car)
library(ggplot2)
library(multcomp) # new package alert! Make sure to install.packages("")
library(dplyr)


ph_<-read.csv('C:/Users/Ryan_/Documents/R/time series/files/ph_across_sites.csv')
ph1<-subset(ph_, select = -c(temp_durafet_c, temp_hobo_c)) 
ph_total<-subset(ph1,  ph1$tide == 'high')
ph_

boxplot(ph$p_h~ph$site, xlab = 'Sites', ylab = 'pH') # visualize data

ph_aov<-aov(ph$p_h~ph$site, data = ph)
par(mfrow = c(2,2))
plot(ph_aov)
leveneTest(ph$p_h~ph$site, data = ph) # test equality of variance
res_ph_aov <- ph_aov$residuals
qqPlot(res_ph_aov)
shapiro.test(res_ph_aov) # test normality

summary(ph_aov)
# The p-vlaue is 2e-16, which is less than 0.05. 
# So, I reject the null that the mean pH of site 1 = the mean pH of site 2 = the mean pH of site 3.
TukeyHSD(ph_aov)
