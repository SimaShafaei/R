library(nnet)
head(mtcars)
str(mtcars)
m1=multinom(mtcars$cyl~mtcars$mpg+mtcars$disp+mtcars$hp+mtcars$drat+mtcars$wt+mtcars$qsec+mtcars$vs)
p1=predict(m1)
t=table(mtcars$cyl,p1)
t
acc=sum(diag(t))/sum(t)
acc



#========================================================

mtcars_scale=scale(mtcars[,-2])

head(mtcars_scale)

summary(mtcars[,-2])
summary(mtcars_scale)

cov(mtcars[,-2])
cov(mtcars_scale)  #diag will become 1

c=cor(mtcars_scale)
c

s=svd(c)
PC=mtcars_scale%*%s$u
head(PC)  
summary(PC) 
cov(PC)
s$d

m2=multinom(mtcars$cyl~PC[,1])
p2=predict(m2)

t=table(mtcars$cyl,p2)
t
acc=sum(diag(t))/sum(t)
acc
