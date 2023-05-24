# 1. perform t_test for Peppers. H0: mu(angles)=0 and H1: mu(angles)<>1
setwd("D:\\PhD+\\semester 4\\Datamining with linear model\\assignments\\HW4")
ds=read.table("peppers.txt", header = TRUE)
head(ds)
length(ds$angle)
mean(ds$angle)
sd(ds$angle)
summary(ds$angle)
boxplot(ds$angle)
test = t.test(ds$angle,mu = 0, conf.level = 0.95)
test

# the result shows that p-value is very small (0.003733<0.05) so we must reject 
# the null hypothesis
# the second indicator in t-test, confidence interval [1.123883 5.233259] 
# doesn't include zero so both indicator shows that we have to reject H0 

#===============================================================================
#2. perform t_test for paired observation in pulse.txt:
ds=read.table("pulse.txt", header = TRUE)
D=ds$pre-ds$post
D
test_new = t.test(D,mu = 0, conf.level = 0.95)
test_new
# the result shows that p-value is small (0.02846<0.05) so we must reject 
# the null hypothesis
# the second indicator in t-test, confidence interval [0.1786603 2.7546730] 
# doesn't include zero so both indicator shows that we have to reject H0 

hist(D)
boxplot(D)
