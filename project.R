# Import libraries
library("dplyr")

# Get data
dir<-"C:/Users/user1/Documents/neomod.dat"
# Data for GLM
neomod<-read.table(dir)
# Code categorical variables as factors
neomod$gest<-as.factor(neomod$gest)
neomod$edu<-as.factor(neomod$edu)

# Exploratory analysis
# Histograms
par(mfrow=c(2,2))
hist(neomod$re.ad)
hist(neomod$los)
hist(neomod$sex)
hist(neomod$emp.m)

# Scatterplots
par(mfrow=c(1,2))
plot(neomod$los,neomod$re.ad,main="neomod$los vs. neomod$re.ad")
plot(neomod$emp.f,neomod$re.ad,main="neomod$los vs. neomod$re.ad")

# Test all covariates individually
model1<-glm(re.ad~cns,family=binomial(link="logit"),data=neomod)
model2<-glm(re.ad~size,family=binomial(link="logit"),data=neomod)
model3<-glm(re.ad~gest,family=binomial(link="logit"),data=neomod)
model4<-glm(re.ad~bwt,family=binomial(link="logit"),data=neomod)
model5<-glm(re.ad~emp.f,family=binomial(link="logit"),data=neomod)
model6<-glm(re.ad~emp.m,family=binomial(link="logit"),data=neomod)
model7<-glm(re.ad~edu,family=binomial(link="logit"),data=neomod)
model8<-glm(re.ad~los,family=binomial(link="logit"),data=neomod)
model9<-glm(re.ad~sex,family=binomial(link="logit"),data=neomod)
model10<-glm(re.ad~accom,family=binomial(link="logit"),data=neomod)

summary(model1)
summary(model2) 
summary(model3) 
summary(model4) 
summary(model5) 
summary(model6) 
summary(model7) 
summary(model8) 
summary(model9) 
summary(model10) 

# Kendall's Tau
los_empf<-cor.test(neomod$los,neomod$emp.f,method="kendall")
los_empf

los_sex<-cor.test(neomod$los,neomod$sex,method="kendall")
los_sex

los_accom<-cor.test(neomod$los,neomod$accom,method="kendall")
los_accom

empf_accom<-cor.test(neomod$emp.f,neomod$accom,method="kendall")
empf_accom

empf_sex<-cor.test(neomod$emp.f,neomod$sex,method="kendall")
empf_sex

sex_accom<-cor.test(neomod$sex,neomod$accom,method="kendall")
sex_accom

empf_bwt<-cor.test(neomod$emp.f,neomod$bwt,method="kendall")
empf_bwt

empf_gest<-cor.test(neomod$emp.f,as.numeric(neomod$gest),method="kendall")
empf_gest

empf_empm<-cor.test(neomod$emp.f,neomod$emp.m,method="kendall")
empf_empm

empm_sex<-cor.test(neomod$emp.m,neomod$sex,method="kendall")
empm_sex

empm_los<-cor.test(neomod$emp.m,neomod$los,method="kendall")
empm_los

empm_accom<-cor.test(neomod$emp.m,neomod$accom,method="kendall")
empm_accom

gest_empf<-cor.test(neomod$emp.f,as.numeric(neomod$gest),method="kendall")
gest_empf

bwt_los<-cor.test(neomod$bwt,neomod$los,method="kendall")
bwt_los

gest_los<-cor.test(neomod$los,as.numeric(neomod$gest),method="kendall")
gest_los

gest_bwt<-cor.test(neomod$bwt,as.numeric(neomod$gest),method="kendall")
gest_bwt

# Simulate Forward Model Selection
# Empty model
empt_model<-glm(re.ad~1,data=neomod,family=binomial(link="logit"))
# Full model 
full_model<-glm(re.ad~.,data=neomod, family=binomial(link="logit"))

# Using LRT to add/drop variables
n=nrow(neomod)

# Forward Selection
add1(empt_model,data=neomod,family=binomial(link="logit"),scope=full_model,test="LRT",k=log(n))
add1(glm(re.ad~los,data=neomod,family=binomial(link="logit")),scope=full_model,test="LRT",k=log(n))
add1(glm(re.ad~los+emp.f,data=neomod,family=binomial(link="logit")),scope=full_model,test="LRT",k=log(n))
add1(glm(re.ad~los+emp.f+sex,data=neomod,family=binomial(link="logit")),scope=full_model,test="LRT",k=log(n))
add1(glm(re.ad~los+emp.f+sex+accom,data=neomod,family=binomial(link="logit")),scope=full_model,test="LRT",k=log(n))
# Result: re.ad~los+emp.f+sex+accom

# 6 possible combinations (4 choose 2)
int_1<-glm(re.ad~los+emp.f+sex+accom+emp.f:sex,data=neomod,family=binomial(link="logit"))
int_2<-glm(re.ad~los+emp.f+sex+accom+sex:los,data=neomod,family=binomial(link="logit"))
int_3<-glm(re.ad~los+emp.f+sex+accom+los:emp.f,data=neomod,family=binomial(link="logit"))
int_4<-glm(re.ad~los+emp.f+sex+accom+emp.f:accom,data=neomod,family=binomial(link="logit"))
int_5<-glm(re.ad~los+emp.f+sex+accom+sex:accom,data=neomod,family=binomial(link="logit"))
int_6<-glm(re.ad~los+emp.f+sex+accom+los:accom,data=neomod,family=binomial(link="logit"))

summary(int_1)
summary(int_2)
summary(int_3)
summary(int_4)
summary(int_5)
summary(int_6)

# Compare link functions
main_model<-glm(re.ad~los+emp.f+sex+accom,binomial(link="logit"),data=neomod)
summary(main_model)

main_model_2<-glm(re.ad~los+emp.f+sex+accom,binomial(link="probit"),data=neomod)
summary(main_model_2)

main_model_3<-glm(re.ad~los+emp.f+sex+accom,binomial(link="cloglog"),data=neomod)
summary(main_model_3)

main_model_4<-glm(re.ad~los+emp.f+sex+accom,binomial(link="cauchit"),data=neomod)
summary(main_model_4)

BIC(main_model,main_model_2,main_model_3,main_model_4)

# Choose final model as cloglog link
main_model<-main_model_3

# Residuals
dev_res<-residuals(main_model,type="deviance")
mean(dev_res)
var(dev_res)

pea_res<-residuals(main_model,type="pearson")
mean(pea_res)
var(pea_res)

# Examine for linear relationship between covariates and residuals
par(mfrow=c(2,2))
plot(neomod$los,pea_res,main="neomod$los vs.\npearson residuals",ylab="pearson residuals")
lines(smooth.spline(neomod$los,pea_res),col=2)
plot(neomod$accom,pea_res,main="neomod$accom vs.\npearson residuals",ylab="pearson residuals")
plot(neomod$emp.f,pea_res,main="neomod$emp.f vs. \npearson residuals",ylab="pearson residuals")
plot(neomod$sex,pea_res,main="neomod$sex vs. \npearson residuals",ylab="pearson residuals")

res_df<-data.frame(cbind(neomod$accom,neomod$emp.f,neomod$sex,pea_res))
colnames(res_df)<-c("accom","emp.f","sex","pea_res")

# Accom
df_acc0<-res_df %>% filter(accom==0)
mean(df_acc0$pea_res)
df_acc1<-res_df %>% filter(accom==1)
mean(df_acc1$pea_res)
# difference
abs(mean(df_acc0$pea_res)-mean(df_acc1$pea_res))

# Emp.f
df_empf0<-res_df %>% filter(emp.f==0)
mean(df_empf0$pea_res)
df_empf1<-res_df %>% filter(emp.f==1)
mean(df_empf1$pea_res)
# difference
abs(mean(df_empf0$pea_res)-mean(df_empf1$pea_res))

# Sex
df_sex0<-res_df %>% filter(sex==0)
mean(df_sex0$pea_res)
df_sex1<-res_df %>% filter(sex==1)
mean(df_sex1$pea_res)
# difference
abs(mean(df_sex0$pea_res)-mean(df_sex1$pea_res))

# Outlier detection
p<-4 # 4 parameters

influence.measures(main_model)

# Number of abnormally large standardized residuals
sum(as.numeric(abs(residuals(main_model))>3)) # None, consistent with the plot
# Number of points that are influential according to dffit residual
sum(as.numeric(dffits(main_model)>2*sqrt(p/n))) #7
# Number of influential points according to residual variance
sum(as.numeric(abs(covratio(main_model))>(1+((3*p)/(n-p))))) #120
# Number of influential points according to Cook's Distance
sum(as.numeric(cooks.distance(main_model)>4/(n-p))) #35
# Number of influential points according to high leverage
sum(as.numeric((hatvalues(main_model)/(p/n))>2)) #235

# Hosmer-Lemeshow with G=10
group<-cut(fitted.values(main_model),breaks=quantile(fitted.values(main_model),
                                     seq(0,1,length.out=11)),include.lowest=TRUE)
obser<-tapply(main_model$y,group,sum)
pr<-tapply(fitted.values(main_model),group,mean)
ng<-as.numeric(table(group))
expec<-pr*ng
HLcontr<-(obser-expec)^2/(ng*pr*(1-pr))
HL<-sum(HLcontr)
HL
1-pchisq(HL,df=8)
