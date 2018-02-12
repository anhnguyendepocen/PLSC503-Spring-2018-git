################################################
# PLSC 503 -- Spring 2018
#
# Code for Day Eight ("Multivariate Regression")
################################################

# setwd() here...
#
# setwd(~/Whatever)
#
################################
# Added variable plot:

library(RCurl)

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-Spring-2018-git/master/Data/CountryData2000.csv")
Data <- read.csv(text = url) # read the "votes" data
rm(url)

Data<-na.omit(Data[c("infantmortalityperK","DPTpct","healthexpGDP")])

fit<-lm(infantmortalityperK~DPTpct,data=Data)
aux<-lm(healthexpGDP~DPTpct,data=Data)
plot(aux$residuals,fit$residuals,xlab="Health Expenditures | DPT Rates", 
     ylab="Infant Mortality | DPT Rates")
abline(lm(fit$residuals~aux$residuals),lwd=3)

# Using avPlots from car:

library(car)

fit2<-lm(infantmortalityperK~DPTpct+healthexpGDP,data=Data)
avPlots(fit2,~healthexpGDP)

#####################
# Toy example (N=4):

Y<-c(4,-2,9,-5)
X1<-c(200,120,430,110)
X2<-c(-17,32,-29,25)
data<-cbind(Y,X1,X2)
scatterplotMatrix(data)

cor(data)

fit<-lm(Y~X1+X2)
summary(fit)

#####################
# Computation bit...

options(digits=16)
options(scipen=99)

z<-c(-1000000000000,0.000000000000001,1000000000000)
x<-c(-50000,0.000007,5000000)
fit<-lm(z~x)
fit

X<-as.matrix(x)
Z<-as.matrix(z)
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% Z
beta.hat

(fit$coefficients[2] / beta.hat) * 100 # percent difference
