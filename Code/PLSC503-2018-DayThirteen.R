##################################################
# PLSC 503 -- Spring 2018: Code for Day Thirteen.
##################################################

# Africa examples:

library(RCurl)
# install.packages("stargazer")
library(stargazer)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-Spring-2018-git/master/Data/africa2001.csv")
Africa<-read.csv(text=temp, header=TRUE)
rm(temp)

# Perfect multicollinearity:

Africa$newgdp<-(Africa$gdppppd-mean(Africa$gdppppd))*1000

fit<-with(Africa, lm(adrate~gdppppd+newgdp+healthexp+subsaharan+
                       muslperc+literacy))
summary(fit)

# N = K

smallAfrica<-subset(Africa,subsaharan=="Not Sub-Saharan")
fit2<-with(smallAfrica,lm(adrate~gdppppd+healthexp+muslperc+
                            literacy+war))
summary(fit2)

# Multicollinearity examples:

with(Africa, table(internalwar,intensity))

HIV1<-with(Africa, lm(adrate~internalwar))
HIV2<-with(Africa, lm(adrate~intensity))
HIV3<-with(Africa, lm(adrate~internalwar+intensity))

stargazer(HIV1,HIV2,HIV3)
