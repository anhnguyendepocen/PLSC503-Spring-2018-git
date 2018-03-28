#######################################################
# PLSC 503 - Spring 2017
#
# MLE, Day One code
#######################################################

options(scipen=999)
options(digits=4)
par(mar=c(4,4,2,2))

# Toy example:

data<-c(64,63,59,71,68) # Data
Lis<-dnorm(data,mean=68,sd=4) # Likelihoods for m=68,s=4
L68.4<-prod(Lis) # The likelihood (joint product) for m=68,s=4

Mus<-seq(62,68,by=0.1) # possible values of mu [62,68]
L<-numeric(length(Mus)) # a place to put the likelihoods

# Calculate likelihoods for different values of mu:

for (i in 1:length(Mus)) {
  L[i]<-prod(dnorm(data,mean=Mus[i],sd=4))
}

# Plot:

pdf("SalaryLR.pdf",5,4)
plot(Mus,L,t="l",lwd=2,xlab=expression(hat(mu)),
     ylab="Likelihood")
dev.off()

# Log-L Plot:

lnL<-numeric(length(Mus)) # a place to put the lnLs

for (i in 1:length(Mus)) {
  lnL[i]<-sum(log(dnorm(data,mean=Mus[i],sd=4)))
}

pdf("SalarylnLR.pdf",5,4)
plot(Mus,lnL,t="l",lwd=2,xlab=expression(hat(mu)),
     ylab="Log-Likelihood")
dev.off()
