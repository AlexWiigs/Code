
rm(list = ls())
#EtchRate <-c(-25,-58,-70,-61,-30,-35,-7,-10,-21,10,-0,51,10,37,29,125,100,115,85,110)
#ER=EtchRate+600
RFPower<-c(160,160,160,160,160,180,180,180,180,180,200,200,200,200,200,220,220,220,220,220)
ER<-c(575,542, 530, 539, 570, 565, 593, 590, 579, 610, 600, 651, 610,
      637, 629, 725, 700, 715, 685, 710)
ER_data <- data.frame(ER,RFPower)

library(dplyr)
group_by(ER_data, RFPower) %>%
  summarise(
    count = n(),
    mean = mean(ER, na.rm = TRUE),
    sd = sd(ER, na.rm = TRUE)
  )

res.aov <-aov(ER~factor(RFPower),data=ER_data)
summary(res.aov)

#Checking assumptions
#Normality
etch_residuals<-res.aov$residuals
qqnorm(etch_residuals, ylim=c(min(etch_residuals)-1,max(etch_residuals)+1), main = "Normal Q-Q Plot for Residuals",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles- Modified",
       plot.it = TRUE, datax = FALSE)

qqline(etch_residuals, datax = FALSE, distribution = qnorm)

#Check Independency
plot(1:1:20,etch_residuals,ylab="Residuals",xlab="Order")
abline(h=0)

#Check Variance
Fitted_values=res.aov$fitted.values
plot(Fitted_values,etch_residuals,ylab="Residuals",xlab="Fitted Values")
abline(h=0)

#Multiple comparisons
#Individual ttest for each  comparison
RF_160<-ER_data$ER[ER_data$RFPower==160]
RF_180<-ER_data$ER[ER_data$RFPower==180]
RF_200<-ER_data$ER[ER_data$RFPower==200]
RF_220<-ER_data$ER[ER_data$RFPower==220]
PValues_RF<-NULL
resttest<-t.test(RF_160,RF_180)
PValues_RF[1]<-resttest$p.value
resttest<-t.test(RF_160,RF_200)
PValues_RF[2]<-resttest$p.value
resttest<-t.test(RF_160,RF_220)
PValues_RF[3]<-resttest$p.value
resttest<-t.test(RF_180,RF_200)
PValues_RF[4]<-resttest$p.value
resttest<-t.test(RF_180,RF_220)
PValues_RF[5]<-resttest$p.value
resttest<-t.test(RF_200,RF_220)
PValues_RF[6]<-resttest$p.value
resttest<-t.test(RF_200,RF_220)
PValues_R_round<-round(PValues_RF, digits=6)

#Multiple comparisons
#Bonferroni
#NewP=6*PValues_RF
p.adjust(PValues_RF,'bonferroni')
#Benjamini_Hochberg pro0cedure FDR
p.adjust(PValues_RF,'fdr')
Ord_pvalues=sort(PValues_RF)
alpha=0.05
Cutoff=(c(1,2,3,4,5,6)/6)*alpha
Ord_pvalues<=Cutoff

#Another example
P_values=c(0.02850,0.02980,0.48815,0.03010,0.02500,1)
p.adjust (P_values, method="fdr")
p.adjust(P_values,method="bonferroni")
sort(P_values)<Cutoff

#Tukey
TUKEY <- TukeyHSD(x=res.aov, "factor(RFPower)", conf.level=0.95)
Quantile_0.05=qtukey(0.05,4,16,lower.tail = FALSE)
Quantile_0.05*sqrt(5339.20/(16*5)) #This is T_alpha

#Model
lmres<-lm(ER~factor(RFPower))
summary(lmres)
require(lme4)
remres<-lmer(ER~1|factor(RFPower))#package lme4
summary(remres)
