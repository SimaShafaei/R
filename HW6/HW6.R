setwd("D:/PhD+/semester 4/Datamining with linear model/week 8")
library(moments)
d=read.csv("TitanicPassengers1.csv",header = TRUE)
head(d)
dt=na.omit(d)
head(dt)
# create your own variable for survive ifelse to convert y/n to 1/0
#step 2: select best subset of variables from  pclass, sex, age, fare,embarked that have highest R-Square in lm
#step3: run lm and predict survivability and calculate accuracy for those variables 
# do the same thing for logit and probit
survived=ifelse(dt$Survived=="Yes",1,0) #convert Survived to numerical variable
sex=ifelse(dt$Sex=="male",1,0)
#===================================================
m=lm(survived~dt$Pclass)
summary(m)  #R-squared:  0.1294 

m=lm(survived~sex)
summary(m)  #R-squared:  0.2903 

m=lm(survived~dt$Age)
summary(m)  #R-squared:  0.005963 

m=lm(survived~dt$Fare)
summary(m)  #R-squared:  0.07193 

m=lm(survived~dt$Embarked)
summary(m)  #R-squared:  0.03846 

m=lm(survived~dt$Pclass+sex)
summary(m)  #R-squared:0.3683  

m=lm(survived~dt$Pclass+dt$Age)
summary(m)  #R-squared:0.1804 

m=lm(survived~dt$Pclass+dt$Fare)
summary(m)  #R-squared:0.1362

m=lm(survived~dt$Pclass+dt$Embarked)
summary(m)  #R-squared: 0.139

m=lm(survived~sex+dt$Age)
summary(m)  #R-squared: 0.2911

m=lm(survived~sex+dt$Fare)
summary(m)  #R-squared:0.3197 

m=lm(survived~sex+dt$Embarked)
summary(m)  #R-squared:0.3124

m=lm(survived~dt$Age+dt$Fare)
summary(m)  #R-squared:0.08263

m=lm(survived~dt$Age+dt$Embarked)
summary(m)  #R-squared:0.04567 

m=lm(survived~dt$Fare+dt$Embarked)
summary(m)  #R-squared: 0.08655

m=lm(survived~dt$Pclass+sex+dt$Age)
summary(m)  #R-squared:0.3902  

m=lm(survived~dt$Pclass+sex+dt$Fare)
summary(m)  #R-squared:0.3689 

m=lm(survived~dt$Pclass+sex+dt$Embarked)
summary(m)  #R-squared:0.3734

m=lm(survived~dt$Pclass+dt$Age+dt$Fare)
summary(m)  #R-squared:0.1831

m=lm(survived~dt$Pclass+dt$Age+dt$Embarked)
summary(m)  #R-squared:0.1872

m=lm(survived~dt$Pclass+dt$Fare+dt$Embarked)
summary(m)  #R-squared:0.1433 

m=lm(survived~sex+dt$Age+dt$Fare)
summary(m)  #R-squared:0.322  

m=lm(survived~sex+dt$Age+dt$Embarked)
summary(m)  #R-squared:0.3135

m=lm(survived~sex+dt$Fare+dt$Embarked)
summary(m)  #R-squared:0.3306

m=lm(survived~dt$Age+dt$Fare+dt$Embarked)
summary(m)  #R-squared:0.09753

m=lm(survived~dt$Pclass+sex+dt$Age+dt$Fare)
summary(m)  #R-squared:0.3902

m=lm(survived~dt$Pclass+dt$Age+dt$Fare+dt$Embarked)
summary(m)  #R-squared:0.1886

m=lm(survived~dt$Pclass+sex+dt$Fare+dt$Embarked)
summary(m)  #R-squared:0.3736

m=lm(survived~dt$Pclass+sex+dt$Age+dt$Embarked)
summary(m)  #R-squared:0.3939

m=lm(survived~sex+dt$Age+dt$Fare+dt$Embarked)
summary(m)  #R-squared:0.333

m=lm(survived~dt$Pclass+sex+dt$Age+dt$Fare+dt$Embarked)
summary(m)  #R-squared:0.3939


#=============================================================================

EmbarkedQ=ifelse(dt$Embarked=="Q",1,0)
EmbarkedS=ifelse(dt$Embarked=="S",1,0)
m1=lm(survived~dt$Pclass+sex+dt$Age+dt$Embarked)
summary(m1)
c=coef(m1)
c
y1=c[1]+c[2]*dt$Pclass+c[3]*sex+c[4]*dt$Age+c[5]*EmbarkedQ+c[6]*EmbarkedS #prediction of linrea model

summary(y1)
z=ifelse(y1>0.5,1,0)
t=table(survived,z)
t
sum(diag(t))/sum(t)  #Accuracy of LM=0.7955182




m2=glm(survived~dt$Pclass+sex+dt$Age+dt$Embarked,family="binomial"("logit"))
summary(m2)
c=coef(m2)
c
y2=1/(1+exp(-c[1]-c[2]*dt$Pclass-c[3]*sex-c[4]*dt$Age-c[5]*EmbarkedQ-c[6]*EmbarkedS))  #predictor of logit model
z=ifelse(y2>0.5,1,0)
t=table(survived,z)
t
sum(diag(t))/sum(t)  #Accuracy of Logit=0.7955182



m3=glm(survived~dt$Pclass+sex+dt$Age+dt$Embarked,family="binomial"("probit"))
summary(m3)
c=coef(m3)
c
y3=pnorm(c[1]+c[2]*dt$Pclass+c[3]*sex+c[4]*dt$Age+c[5]*EmbarkedQ+c[6]*EmbarkedS)
z=ifelse(y3>0.5,1,0)
t=table(survived,z)
t
sum(diag(t))/sum(t)  #Accuracy of Probit=0.7983193
