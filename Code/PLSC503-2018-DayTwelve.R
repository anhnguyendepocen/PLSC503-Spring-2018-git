################################################
# PLSC 503 -- Spring 2017: Code for Day Twelve.
################################################

# ANES 2016 pilot study aggregation example...

library(plyr)
library(car)
library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-Spring-2018-git/master/Data/ANES-pilot-2016.csv")
ANES<-read.csv(text=temp, header=TRUE)
rm(temp)

ANES$ftgay<-ifelse(ANES$ftgay==998,NA,ANES$ftgay)

# Average feeling thermometers about gays and lesbians:

summary(ANES$ftgay)

# States:

ANES$State<-recode(ANES$state,
"1='AL';2='AK';4='AZ';5='AR';6='CA';8='CO';9='CT';
10='DE';11='DC';12='FL';13='GA';15='HI';16='ID';17='IL';
18='IN';19='IA';20='KS';21='KY';22='LA';23='ME';24='MD';25='MA';
26='MI';27='MN';28='MS';29='MO';30='MT';31='NE';32='NV';
33='NH';34='NJ';35='NM';36='NY';37='NC';38='ND';
39='OH';40='OK';41='OR';42='PA';44='RI';45='SC';46='SD';
47='TN';48='TX';49='UT';50='VT';51='VA';53='WA';54='WV';
55='WI';56='WY'")

# Aggregate by state:

ANES$one<-1
StateFT<-ddply(ANES,.(State),summarise,
               Nresp=sum(one),
               meantherm=mean(ftgay,na.rm=TRUE))
summary(StateFT)

respfit<-with(StateFT, lm(meantherm~log(Nresp)))

pdf("StateThermPlot.pdf",6,5)
par(mar=c(4,4,2,2)) 
with(StateFT, plot(Nresp,meantherm,pch=".",col="white",log="x",
                   xlab="ln(N of Respondents)",xlim=c(0.5,200),
                   ylab="Statewide Mean Score"))
with(StateFT, text(Nresp,meantherm,log="x",labels=StateFT$State,
                   cex=0.3*log(StateFT$Nresp+1)))
abline(h=mean(ANES$ftgay,na.rm=TRUE),lwd=2)
abline(h=mean(StateFT$meantherm),lwd=2,lty=2,col="red")
abline(respfit,lwd=3,col="darkgreen")
dev.off()

#Justices data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-Spring-2018-git/master/Data/Justices.csv")
Justices<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(Justices)

OLSfit<-with(Justices, lm(civrts~score))
summary(OLSfit)

WLSfit<-with(Justices, lm(civrts~score,weights=lnNedit))
summary(WLSfit)

pdf("WLSBubblePlotR.pdf",6,6)
par(mar=c(4,4,2,2))
with(Justices, symbols(score, civrts,circles=Neditorials,
        ylab="Civil Rights Voting",xlab="Segal-Cover Score",
        ylim=c(0,100)))
abline(reg=OLSfit,lwd=2)
abline(reg=WLSfit,lwd=2,lty=2)
with(Justices, points(score,civrts,pch=20))
legend("topleft",bty="n",lty=c(1,2),lwd=2,
       legend=c("OLS","WLS"))
dev.off()

# "Robust"

library(car)
hccm(OLSfit, type="hc1")

library(rms)
OLSfit2<-ols(civrts~score, x=TRUE, y=TRUE)
RobSEs<-robcov(OLSfit2)
RobSEs
