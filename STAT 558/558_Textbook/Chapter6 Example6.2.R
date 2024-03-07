rm(list = ls())
#install.packages("conf.design")
require("conf.design")
A=rep(c(-1,1),8)
B=rep(c(rep(-1,2),rep(1,2)),4)
C=rep(c(rep(-1,4),rep(1,4)),2)
D=c(rep(-1,8),rep(1,8))
X=matrix(c(rep(1,16),A,B,C,D),nrow=16,ncol=5)
#Beta_hat1=solve(t(X)%*%X)%*%t(X)%*%FiltrationRate
FiltrationRate=c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)
FiltrationRate_Data <- data.frame(FiltrationRate, A,B,C,D)

factor(A)
#res.lm<-lm(FiltrationRate~A+B, data=FiltrationRate_Data)
res.lm<-lm(FiltrationRate~A*B*C*D, data=FiltrationRate_Data)
summary(res.lm)
A1=rep(c(0,1),8)
B1=rep(c(rep(0,2),rep(1,2)),4)
X=matrix(c(rep(1,16),A1,B1),nrow=16,ncol=3)
Beta_hat2=solve(t(X)%*%X)%*%t(X)%*%FiltrationRate

res.aov<-aov(FiltrationRate~A*B*C*D,data=FiltrationRate_Data)
summary(res.aov)
summary(res.lm)

#res.aov3<-aov(FiltrationRate~factor(A)*factor(B)*factor(C)*factor(D),data=FiltrationRate_Data)
#summary(res.aov3)
#Which factors are important? 
#We only have one obs so we need the normal prob. plot

#install.packages("daewr")
library(daewr)
fullnormal(coef(res.lm)[-1],alpha=.025)

#Projected model
res.aov<-aov(FiltrationRate~A*C*D,data=FiltrationRate_Data)
summary(res.aov)
res.lm<-lm(FiltrationRate~A*C*D, data=FiltrationRate_Data)
summary(res.lm)

#Final model - remove non-significant terms
res.aov<-aov(FiltrationRate~A*C*D-A:C:D-C:D,data=FiltrationRate_Data)
summary(res.aov)
#Residual Analysis
#Normality
filtration_residuals=res.aov$residuals
qqnorm(filtration_residuals, ylim=c(min(filtration_residuals),max(filtration_residuals)), main = "Normal Q-Q Plot for Residuals",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles- Modified",
       plot.it = TRUE, datax = FALSE)

qqline(filtration_residuals, datax = FALSE, distribution = qnorm)
#Test normality using Shapiro Wilks
shapiro.test(filtration_residuals)


#Check Variance
Fitted_values=res.aov$fitted.values
plot(Fitted_values,filtration_residuals,ylab="Residuals",xlab="Fitted Values")
abline(h=0)

#Introducing Block effect- Chapter 7
#Modification 1- confound ABCD with blocks
Block1=c(1,2,2,1,2,1,1,2,2,1,1,2,1,2,2,1)
res.aov2<-aov(FiltrationRate~A*B*C*D-A:B:C:D+Block1,data=FiltrationRate_Data)
summary(res.aov2)

  #Modified response variable
FiltrationRate_Mod=FiltrationRate;
FiltrationRate_Mod[Block1==1]=FiltrationRate_Mod[Block1==1]-20
FiltrationRate_Data_Mod <- data.frame(FiltrationRate_Mod, A,B,C,D)
res.aov2<-aov(FiltrationRate_Mod~A*B*C*D-A:B:C:D+Block1,data=FiltrationRate_Data_Mod)
summary(res.aov2)


res.aov3<-aov(FiltrationRate_Mod~A*B*C*D,data=FiltrationRate_Data_Mod)
#summary(res.aov3)

res.lm2<-lm(FiltrationRate~A*B*C*D, data=FiltrationRate_Data)
#fullnormal(coef(res.lm2)[-1],alpha=.025)

res.lm2<-lm(FiltrationRate_Mod~A*B*C*D-A:B:C:D+Block1,data=FiltrationRate_Data_Mod)
summary(res.lm2)
fullnormal(coef(res.lm2)[-1],alpha=.025)
