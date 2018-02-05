################################################
# PLSC 503 -- Spring 2018
#
# Code for Day Six ("Stupid Regression Tricks")
################################################

library(RCurl)

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-Spring-2018-git/master/Data/africa2001.csv")
africa<-read.csv(text=temp, header=TRUE)
summary(africa)

fit<-with(africa, lm(adrate~muslperc))
summary(fit)

# First figure

SEs<-predict(fit,interval="confidence")
Sort<-order(africa$muslperc)

pdf("Notes/SRTFig1.pdf",6,5)
plot(africa$muslperc, africa$adrate, 
     xlab="Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate",pch=16) 
abline(fit,lwd=3)
lines(sort(africa$muslperc),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslperc),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Add 10:

africa$muslplusten<-africa$muslperc+10
fit2<-with(africa, lm(adrate~muslplusten,data=africa))
summary(fit2)

SEs<-predict(fit2,interval="confidence")

pdf("Notes/SRTFig2.pdf",6,5)
plot(africa$muslplusten, africa$adrate, 
     xlab="Muslim Percentage of the Population + 10", 
     ylab="HIV Prevalence Rate",pch=16)
abline(fit2,lwd=3)
lines(sort(africa$muslplusten),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslplusten),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Multiply Y times -314:

africa$screwyrate<-africa$adrate*(-314)
fit3<-with(africa, lm(screwyrate~muslperc))
summary(fit3)

SEs<-predict(fit3,interval="confidence")

pdf("Notes/SRTFig3.pdf",6,5)
plot(africa$muslperc, africa$screwyrate, 
     xlab="Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate times -314",pch=16,
     ylim=c(-13000,1000))
abline(fit3,lwd=3)
lines(sort(africa$muslperc),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslperc),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Reversing the scales:

africa$nonmuslimpct <- 100 - africa$muslperc
africa$noninfected <- 100 - africa$adrate
fit4<-lm(noninfected~nonmuslimpct,data=africa)
summary(fit4)

SEs<-predict(fit4,interval="confidence")
Sort2 <- order(africa$nonmuslimpct)

pdf("Notes/SRTFig4.pdf",6,5)
plot(africa$nonmuslimpct, africa$noninfected, 
     xlab="Non-Muslim Percentage of the Population", 
     ylab="HIV Non-Prevalence Rate",pch=16,
     ylim=c(60,111))
abline(fit4,lwd=3)
lines(sort(africa$nonmuslimpct),SEs[Sort2,2],col="red",lwd=2,lty=2)
lines(sort(africa$nonmuslimpct),SEs[Sort2,3],col="red",lwd=2,lty=2)
dev.off()

# Centering X:

africa$muslcenter<-africa$muslperc - mean(africa$muslperc, na.rm=TRUE)
fit5<-lm(adrate~muslcenter,data=africa)
summary(fit5)

SEs<-predict(fit5,interval="confidence")

pdf("Notes/SRTFig5.pdf",6,5)
plot(africa$muslcenter, africa$adrate, 
     xlab="Centered Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate",pch=16,
     ylim=c(-10,40))
abline(fit5,lwd=3)
lines(sort(africa$muslcenter),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslcenter),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Rescaling X for interpretability:

fit6<-lm(adrate~population,data=africa)
summary(fit6)

africa$popmil<-africa$population / 1000000
fit7<-lm(adrate~popmil,data=africa)
summary(fit7)

# Dichotomous X and t-tests:

fit8<-lm(adrate~subsaharan,data=africa)
summary(fit8)

with(africa,
     t.test(adrate~subsaharan, var.equal=TRUE))

# Reporting:

fit<-lm(adrate~muslperc, data=africa)
summary.lm(fit)


