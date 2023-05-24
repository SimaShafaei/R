setwd("D:\\PhD+\\semester 4\\Datamining with linear model\\assignments\\HW1")
ds=read.table("myData.txt", header = TRUE)
head(ds)
str(ds)
# we can get some of statistical summary using "summary" command in R
summary(ds)
install.packages("moments")
library(moments)

# Summary Statistics of X
nrow(ds)
mean(ds$X)
sd(ds$X)
var(ds$X)
median(ds$X)
IQR(ds$X)
skewness(ds$X)
kurtosis(ds$X)
min(ds$X)
max(ds$X)
range(ds$X)
hist(ds$X)

#skewness(ds$X)= -0.09570461  shows a longer or fatter tail on the left side of the distribution but because it is a small negative we can say that the data are fairly symmetrical
# kurtosis(ds$X) = 1.851211 < 3 means the distribution produces fewer and less extreme outliers than does the normal distribution or refers to a flat-topped distribution function 


# Summary Statistics of Y
nrow(ds)
mean(ds$Y)
sd(ds$Y)
var(ds$Y)
median(ds$Y)
IQR(ds$Y)
skewness(ds$Y)
kurtosis(ds$Y)
min(ds$Y)
max(ds$Y)
range(ds$Y)
hist(ds$Y)
#skewness(ds$X)= -0.08868097  shows a longer or fatter tail on the left side of the distribution but because it is a small negative we can say that the data are fairly symmetrical
# kurtosis(ds$X) = 1.806836 < 3 means the distribution produces fewer and less extreme outliers than does the normal distribution or refers to a flat-topped distribution function 


#
var(ds)
skewness(ds)
kurtosis(ds)


