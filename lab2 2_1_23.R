
## Sim for sample correlation
library(MASS)
rho = 0
Sigma = matrix (c(1,rho,rho,1),nrow = 2, ncol= 2)
Sigma
set.seed(1)
xs = mvrnorm(n= 20, mu = c(0,0), Sigma)
x = xs[,1]
y = xs[,2]
r = cor(xs[,1],xs[,2])
r
plot(xs)
abline(h = mean(y))
abline(v = mean(x))
abline(lm(xs[,2]~xs[,-1]))

######
par(mfrow = c(2,3) ,mai = c(0,0,0,0), mar =c(0,0,1.5,0))
#####################################################################
############################################################################
rho = .5
Sigma = matrix (c(1,rho,rho,1),nrow = 2, ncol= 2)
Sigma
xs = mvrnorm(n= 20, mu = c(0,0), Sigma)
r = cor(xs[,1],xs[,2])
r
plot(xs)
abline(lm(xs[,2]~xs[,-1]))
mtext(paste("true cor is" , round(rho,2), ", sample cor", round(r,2)), cex = .8)
#########
rho = -0.5
Sigma = matrix (c(1,rho,rho,1),nrow = 2, ncol= 2)
Sigma
xs = mvrnorm(n= 20, mu = c(0,0), Sigma)
r = cor(xs[,1],xs[,2])
r
plot(xs)
abline(lm(xs[,2]~xs[,-1]))
mtext(paste("true cor is" , round(rho,2), ", sample cor", round(r,2)), cex = .8)
##################################################################################
#################################################################################
rho = .9
Sigma = matrix (c(1,rho,rho,1),nrow = 2, ncol= 2)
Sigma
xs = mvrnorm(n= 20, mu = c(0,0), Sigma)
r = cor(xs[,1],xs[,2])
r
plot(xs)
abline(lm(xs[,2]~xs[,-1]))
mtext(paste("true cor is" , round(rho,2), ", sample cor", round(r,2)), cex = .8)
#################################################################################
rho = -0.9
Sigma = matrix (c(1,rho,rho,1),nrow = 2, ncol= 2)
Sigma
xs = mvrnorm(n= 20, mu = c(0,0), Sigma)
r = cor(xs[,1],xs[,2])
r
plot(xs)
abline(lm(xs[,2]~xs[,-1]))
mtext(paste("true cor is" , round(rho,2), ", sample cor", round(r,2)), cex = .8)
#######################################################################
rho = 1
Sigma = matrix (c(1,rho,rho,1),nrow = 2, ncol= 2)
Sigma
xs = mvrnorm(n= 20, mu = c(0,0), Sigma)
r = cor(xs[,1],xs[,2])
r
plot(xs)
abline(lm(xs[,2]~xs[,-1]))
mtext(paste("true cor is" , round(rho,2), ", sample cor", round(r,2)), cex = .8)
#####################################################################
rho = -1
Sigma = matrix (c(1,rho,rho,1),nrow = 2, ncol= 2)
Sigma
xs = mvrnorm(n= 20, mu = c(0,0), Sigma)
r = cor(xs[,1],xs[,2])
r
plot(xs)
abline(lm(xs[,2]~xs[,-1]))
mtext(paste("true cor is" , round(rho,2), ", sample cor", round(r,2)), cex = .8)
############################################################################
############################################################################
advt = read.csv("Advertising.csv")
lm1 = lm(Sales~TV, data = advt)
attach(advt)
r = cor(Sales,TV)
r
Sx = sd(TV)
Sy = sd(Sales)
r * Sy/Sx
lm1$coefficients[2]
summary(lm1)
beta1_hat * Sx / Sy = r ### another way to find r
#################