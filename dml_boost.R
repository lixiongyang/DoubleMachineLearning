


dml_boost <- function(data,y,x,d,sed=123) {

#split the sample into two parts
library(caret)
set.seed(sed)
trainindex=createDataPartition(d,p=0.5,list=F)
data1=data[trainindex,]
data2=data[-trainindex,]
y1=y[trainindex]
y2=y[-trainindex]
d1=d[trainindex]
d2=d[-trainindex]

formula.1 <- as.formula(paste("y1~",x))
formula.2 <- as.formula(paste("d1~",x))

formula.3 <- as.formula(paste("y2~",x))
formula.4 <- as.formula(paste("d2~",x))

#########################################################
# Method 3: boosting
library(gbm)
set.seed(sed)
#step 1
boost.aq=gbm(formula.1,data=data1,distribution="gaussian",n.trees=100,interaction.depth=4,shrinkage=0.1)
summary(boost.aq)
yhat.aq3=predict(boost.aq,newdata=data2,n.trees=100)
ylhat31=y2-yhat.aq3
#step 2
boost.d=gbm(formula.2,data=data1,distribution="gaussian",n.trees=100,interaction.depth=4,shrinkage=0.1)
summary(boost.d)
yhat.d3=predict(boost.d,newdata=data2,n.trees=100)
vhat31=d2-yhat.d3
#step 3
boost.aq=gbm(formula.3,data=data2,distribution="gaussian",n.trees=100,interaction.depth=4,shrinkage=0.1)
summary(boost.aq)
yhat.aq3=predict(boost.aq,newdata=data1,n.trees=100)
ylhat32=y1-yhat.aq3
#step 4
boost.d=gbm(formula.4,data=data2,distribution="gaussian",n.trees=100,interaction.depth=4,shrinkage=0.1)
summary(boost.d)
yhat.d3=predict(boost.d,newdata=data1,n.trees=100)
vhat32=d1-yhat.d3
#step5: reg ylhat vhat
lm.fit1=lm(ylhat31~vhat31)
summary(lm.fit1)

lm.fit2=lm(ylhat32~vhat32)
summary(lm.fit2)


# compute robust standard error

#install.packages("lmtest")
#install.packages("sandwich")
library(lmtest)
library(sandwich)
est1 = coeftest(lm.fit1, vcov = vcovHC, type = "HC0")
est2 = coeftest(lm.fit2, vcov = vcovHC, type = "HC0")

b1 = est1[2,1]
b2 = est2[2,1]
be = (b1+b2)/2

se1 = est1[2,2]
se2 = est2[2,2]
sig2 =(se1^2+se2^2 +(b1-be)^2+(b2-be)^2)/2
se =sqrt(sig2)
t =be/se


# ouput the estimation results
cat("----------------------------------------------","\n")
cat("Double machine learning(boosting):","\n")
cat("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
print(cbind(theta=be,se.robust=se,t.value=t,pvalue=round(2*(1-pnorm(abs(be/se))),5),
            be-1.96*se,be+1.96*se),digits=4)

cat("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
cat("----------------------------------------------","\n")

}