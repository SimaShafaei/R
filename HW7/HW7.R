setwd("D:/PhD+/semester 4/Datamining with linear model/week 10")
ctg=read.table("CTG_Dataset.txt",header = TRUE)
ctg=ctg[,c("LB","AC","FM","UC","NSP")]
head(ctg)

#===============================================================================
#Use the instruction set.seed(777) to split the data set into training and testing
#subsets with probabilities = 0.6, 0.4 respectively

set.seed(777)
ind=sample(2,nrow(ctg), replace=TRUE, prob = c(0.6,0.4))
training= ctg[ind==1,]
testing= ctg[ind==2,]
nrow(ctg)
nrow(training)
nrow(testing)

head(training)
head(testing)


#===============================================================================
# Develop a multinom model m using the training subset. List the coefficients of m.
# Use the instruction predict(m) to predict NSP. Then compute the confusion matrix
# and the accuracy ACC
library(nnet)
m=multinom(training$NSP~training$LB+training$AC+training$FM+training$UC)
s=summary(m)
c=s$coefficients
c
fitted.values(m)  
p=predict(m)  #actual prediction

t=table(training$NSP,p)
t
acc=sum(diag(t))/sum(t)
acc


#===============================================================================
#Follow Lecture 10 to construct matrix X for the testing data subset. Then use
#coefficients of model m to compute the responses of the model. Compute the
#probabilities and predictions using matrix X. Finally find the confusion matrix 
#and accuracy ACC
X=cbind(rep(1,nrow(testing)),testing[,-5])  
head(X)
X=as.matrix(X)
Y=X%*%t(c)   
head(Y)    
Y=exp(Y)  
head(Y)
 
pb=cbind(1/(1+Y[,1]+Y[,2]),Y[,1]/(1+Y[,1]+Y[,2]),Y[,2]/(1+Y[,1]+Y[,2]))
head(pb)
p=which.max(pb[1,])
for (i in 2:nrow(testing)) {p=c(p, which.max(pb[i,]))}
p
t=table(testing$NSP,p)
t
sum(diag(t))/sum(t)
