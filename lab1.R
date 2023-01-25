# lesson2 

advt = read.csv("Advertising.csv")[,-1]
head(advt)
dim(advt)
n = nrow(advt)
n
##################

pairs(advt)
attach(advt) #attach the dataset to the current R 

Sales[1:10]
TV[1:10]
plot(TV,Sales)


#different ways of calliong a column and row
advt[10,"Sales"]
advt[20,c(2,5)]
advt[20,c("Tv", "Sales")]

######################3

plot(Sales~TV, data=advt)

################################
lm1=lm(Sales~TV) # Short cut Gives me beta 0 hat and beta 1 hat, in order (7,03259, 0.04754)
############################# 
abline(lm1,col=2, lwd=2)#Least square line 
summary(lm1)
############# LSE by "hand"
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
resds = Sales - yhats # differnece between the predicted vs actual
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
