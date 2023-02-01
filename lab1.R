# lesson2 

advt = read.csv("Advertising.csv")[,-1]
head(advt)
dim(advt)
n = nrow(advt)
n
###################different ways of calling a column and row

advt[10,"Sales"]
advt[20,c(2,5)]
advt[20,c("Tv", "Sales")]
pairs(advt)
attach(advt) #attach the data set to the current R 

Sales[1:10]
TV[1:10]
plot(Sales~TV, data=advt)
plot(TV,Sales)


################################
cov_TV = cov(TV, Sales)
var_TV = var(TV)
Var_sales = var(Sales)
Sxx = (n - 1) *Var_TV

####################
lm1=lm(Sales~TV) # Short cut Gives me beta 0 hat and beta 1 hat, in order (7,03259, 0.04754)
############################# 
abline(lm1,col=2, lwd=2)#Least square line 
summary(lm1)
############# LSE by "hand"  ##### Q1A
cov_TV=cov(TV,Sales) #coveriant
Var_TV=var(TV) 
b1hat = cov_TV / Var_TV #beta 1 hat is coveriant over variant
b1hat
b0hat=mean(Sales) - b1hat * mean(TV)#beta 0 hat
b0hat
coef(lm1)

################33
yhats = b0hat + b1hat * TV #y hat formula for each expected y value
########################################################### Manual way to plot the points and line
plot(TV, Sales)
lines(TV, yhats)
######################################### Calculating the residuals 
resds = Sales - yhats # difference between the predicted vs actual
resds[1:10]
#####################################Calculating the SSE
SSE = sum(resds^2)
SSE
######################################### calculating sigma and sigma2hat
sigma2hat = SSE/(n-2)
sigma2hat
Sigmahat = sqrt(sigma2hat)
Sigmahat
summary(lm1)$sigma

##############################

################## Case study: Advertising, cont. from last code

###########t-test
X=model.matrix(lm1)
SX=solve(t(X)%*%X)
SX
#1/(Var_TV*(n-1))
#s_b1=sigmahat*sqrt(SX[2,2])
s_b1=Sigmahat/sqrt(Sxx)
t1=b1hat/s_b1
t1

pvalue1=2*(1-pt(abs(t1),df=n-2))
pvalue1
s_b0=Sigmahat*sqrt(SX[1,1])
s_b0
sqrt(1/n+mean(TV)^2/Sxx)*Sigmahat
t0=b0hat/s_b0
t0

pvalue0=2*(1-pt(abs(t0),df=n-2))
pvalue0
summary(lm1)$coefficients
#########CI for coefs
confint(lm1,level=0.95)
#construct the CIs by yourself
b0hat+c(-1,1)*qt(0.975,n-2)*s_b0
b1hat+c(-1,1)*qt(0.975,n-2)*s_b1
#########estimation
predict(lm1,newdata=data.frame(TV=150))
predict(lm1,newdata=data.frame(TV=150),interval = "confidence",level=0.9)
#construct the CI by yourself
#newx=matrix(c(1,150),2,1)
#sum(newx*c(b0hat,b1hat))
yhat=b0hat+b1hat*150
yhat
#Smuhat=sigmahat*sqrt(t(newx)%*%SX%*%newx)[1,1]
Smuhat=Sigmahat*sqrt(1/n+(150-mean(TV))^2/Sxx)
Smuhat
yhat+c(-1,1)*qt(0.95,n-2)*Smuhat
yhat+1*qt(0.95,n-2)*Smuhat
yhat+(-1)*qt(0.95,n-2)*Smuhat
#Exercise test H_0: mu_150<=13.5, find the p-value by yourselves

###############Prediction
predict(lm1,newdata=data.frame(TV=150),interval = "prediction",level=0.9)
#Syhat=sigmahat*sqrt(t(newx)%*%SX%*%newx+1)[1,1]
Syhat=Sigmahat*sqrt(1/n+(150-mean(TV))^2/Sxx+1)
Syhat
yhat+c(-1,1)*qt(0.95,n-2)*Syhat






#####################################################
#####1D####Estimate the expected Sales for TV=150#####
yhat = (b0hat + b1hat * 150) # Y hat

########1E###################
qt(.95, n-2) ##### T stat
xbar= mean(TV)
Smuhat = Sigmahat*sqrt((1/n)+((xbar-150)^2)/Sxx)##### Syhat150###### for confinde ce interval
Smuhat1 = Sigmahat*sqrt(1+(1/n)+((xbar-150)^2)/Sxx)###### for prediction interval 
yhat+c(-1,1) * qt (.95,n-2) * Smuhat #####Confidence interval
yhat+c(-1,1) * qt (.95,n-2) * Smuhat1 ###### prediction interval









#####################################################################
#####################################################################
#################Page 75: Q1#########################################
#####################################################################


q1a = read.csv("CH05Q01.csv")
head(q1a)
dim(q1a)
n1 = nrow(q1a)
n1
####################################
lmq1=lm(DRYWGT~AGE, data = q1a) ### b0 and b1 for dry weight regressed on Age (-1.88453, 0.23507)
plot(DRYWGT~AGE, data = q1a)
abline(lmq1,col=2, lwd=2) #least square line
####################################
lmq1_2 = lm(LOGDRYWG ~ AGE, data = q1a) ### b0 and b1 for log10 dry weight regressed on Age (-2.689, 0.196)
plot(LOGDRYWG~AGE, data = q1a)
abline(lmq1_2, col=2, lwd = 2 ) #least square line
####################################

confint(lmq1_2,  level= 0.95) # confidence interval for
                              #b1 and for b0

###############################LSE by hand for log10 dry

attach(q1a)
cov_AGE=cov(AGE,LOGDRYWG) #coveriant
Var_AGE=var(AGE) ### varient
#####################
b1hat_AGE = cov_AGE / Var_AGE #beta 1 hat is coveriant over variant
b1hat_AGE
b0hat_AGE=mean(LOGDRYWG) - b1hat_AGE * mean(AGE)#beta 0 hat
############################################
yhats_AGE = b0hat_AGE + b1hat_AGE * AGE   ### Y hats
resds_AGE = LOGDRYWG  - yhats_AGE  #### residuals 
SSE_AGE = sum(resds_AGE^2) ### SSE
#########################################calculating sigma and sigma2hat
sigma2hat_AGE = SSE_AGE/(n1-2)
Sigmahat_AGE = sqrt(sigma2hat_AGE)
summary(lmq1_2)$sigma
############################################

###################################################
###################################################
###################################################
#######################q2##########################

q2a = read.csv("CH05Q02.csv")
#################################
plot(SBP~QUET, data = q2a)
lmq2_1=lm(SBP~QUET, data = q2a)
abline(lmq2_1,col=2, lwd=2)
QUET.Sort = sort(QUET)
YHAT = 70.567 + 21.492*QUET.Sort
plot(SBP~QUET, data = q2a)
lines(QUET.Sort, YHAT, col=2)
resds_QUET = SBP  - YHAT
SSE_QUET = sum(resds_QUET^2) ### SSE
n2 = nrow(q2a)
sigma2hat_QUET = SSE_QUET/(n2-2)
Sigmahat_QUET= sqrt(sigma2hat_QUET)
var_QUET = var(QUET)

Sxx_QUET = (n2 - 1) *var_QUET

smuhat0 = Sigmahat_QUET * sqrt(1/n2+ (QUET.Sort - mean(QUET))^2/Sxx_QUET)
syhat0 = Sigmahat_QUET * sqrt(1/n2+ (QUET.Sort - mean(QUET))^2/Sxx_QUET + 1)
CB.L = YHAT-c(1) * qt(0.95, n2-2)*smuhat0
CB.U = YHAT+c(1) * qt(0.95, n2-2)*smuhat0
PB.L = YHAT-c(1) * qt(0.95, n2-2)*syhat0
PB.U = YHAT+c(1) * qt(0.95, n2-2)*syhat0

lines(QUET.Sort, CB.L, col = 3)
lines(QUET.Sort, CB.U, col = 3)

lines(QUET.Sort, PB.L, col = 4)
lines(QUET.Sort, PB.U, col = 4)


YHAT_1 = 70.567 + 21.492*3.4


xbar_QUET= mean(QUET)
Smuhat_QUET = Sigmahat_QUET*sqrt((1/n2)+((xbar_QUET-3.4)^2)/Sxx_QUET)##### Syhat150###### for confinde ce interval
Smuhat1_QUET = Sigmahat_QUET*sqrt(1+(1/n2)+((xbar_QUET-3.4)^2)/Sxx_QUET)###### for prediction interval 
YHAT_1+c(-1,1) * qt (.95,n2-2) * Smuhat #####Confidence interval
YHAT_1+c(-1,1) * qt (.95,n2-2) * Smuhat1_QUET
###########################################
plot(QUET~AGE, data = q2a)
lmq2_2=lm(QUET~AGE, data = q2a)
abline(lmq2_2,col=2, lwd=2)

########################################
plot(SBP~AGE, data = q2a)
lmq2_3=lm(SBP~AGE, data = q2a)
abline(lmq2_3,col=2, lwd=2)
############################
plot(SBP~SMK, data = q2a)
lmq2_4 = lm(SBP~SMK, data = q2a)
summary(lmq2_4)
attach(q2a)
nd = q2a[order(SMK),]
 nd1 = q2a[c(1,2,3,4,13,14,19,20,22,23,24,27,29,31,32),]
 mean(nd1$SBP)
nd2 = q2a[c(5,6,7,8,9,10,11,12,15,16,17,18,21,25,26,28,30),]
mean(nd2$SBP)
