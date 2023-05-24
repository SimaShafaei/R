#===============================================================================
#                              Entropy of X                                    #
#===============================================================================
setwd("D:\\PhD+\\semester 4\\Datamining with linear model\\assignments\\HW1")
ds=read.table("myData.txt", header = TRUE)
head(ds)
str(ds)
# we can get some of statistical summary using "summary" command in R
summary(ds)
hx=hist(ds$X)
hx100=hist(ds$X,breaks = 100)
length(hx100$counts)
sum(hx100$counts)
px=hx100$counts/sum(hx100$counts)
plot(px)
qx=px[px>0]
length(qx) #shorter than p
entropy_x = sum(-qx*log2(qx))
entropy_x
max_entropy=log2(100)
max_entropy
#===============================================================================
#                              Entropy of Y                                    #
#===============================================================================
hy=hist(ds$Y)
hy100=hist(ds$Y,breaks = 100)
length(hy100$counts)
sum(hy100$counts)
py=hy100$counts/sum(hy100$counts)
plot(py)
qy=py[py>0]
length(qy) #shorter than p
entropy_y = sum(-qy*log2(qy))
entropy_y
max_entropy_y=log2(100)
max_entropy_y

#===============================================================================
#                  compute Errors in four models                               #
#===============================================================================


#1.	Remove all records with NA entries. Find the number of the available records or rows.

d=na.omit(airquality) #remove null variables

p1=0.27*d$Solar.R
lines(d$Solar.R,p1,col=2)  #col=color
e1 = p1 - d$Ozon

#  b.	Linear model; lm
m2=lm(d$Ozone~d$Solar.R)
c2=coef(m2)
p2=c2[1] + c2[2]*d$Solar.R
e2=p2-d$Ozone

#  c.	Second order polynomial 
x2=d$Solar.R * d$Solar.R
m3=lm(d$Ozone~d$Solar.R+x2)
c3=coef(m3)
p3= c3[1]+c3[2]*d$Solar.R+c3[3]*x2
e3=p3-d$Ozone

#  d.	Generalized linear model; glm
m4 = glm(d$Ozone~d$Solar.R, family = "poisson")
c4=coef(m4)
p4 = exp(c4[1]+c4[2]*d$Solar.R)
e4=p4-d$Ozone
install.packages("moments")
library(moments)
#===============================================================================
#          summary of Statistics and Entropy of Eyeball Error                  #
#===============================================================================


mean(e1)
sd(e1)
var(e1)
median(e1)
IQR(e1)
skewness(e1)
kurtosis(e1)
min(e1)
max(e1)
range(e1)
h_e1=hist(e1)


h_e1_100=hist(e1,breaks = 100)
length(h_e1_100$counts)
sum(h_e1_100$counts)
p_e1=h_e1_100$counts/sum(h_e1_100$counts)
plot(p_e1)
q_e1=p_e1[p_e1>0]
length(q_e1) #shorter than p
entropy_e1 = sum(-q_e1*log2(q_e1))
entropy_e1
max_entropy_e1=log2(100)
max_entropy_e1

#===============================================================================
#          summary of Statistics and Entropy of LM Error                       #
#===============================================================================

mean(e2)
sd(e2)
var(e2)
median(e2)
IQR(e2)
skewness(e2)
kurtosis(e2)
min(e2)
max(e2)
range(e2)
h_e2=hist(e2)


h_e2_100=hist(e2,breaks = 100)
length(h_e2_100$counts)
sum(h_e2_100$counts)
p_e2=h_e2_100$counts/sum(h_e2_100$counts)
plot(p_e2)
q_e2=p_e2[p_e2>0]
length(q_e2) #shorter than p
entropy_e2 = sum(-q_e2*log2(q_e2))
entropy_e2
max_entropy_e2=log2(100)
max_entropy_e2
#===============================================================================
#          summary of Statistics and Entropy of polynomial Error               #
#===============================================================================

mean(e3)
sd(e3)
var(e3)
median(e3)
IQR(e3)
skewness(e3)
kurtosis(e3)
min(e3)
max(e3)
range(e3)
h_e3=hist(e3)


h_e3_100=hist(e3,breaks = 100)
length(h_e3_100$counts)
sum(h_e3_100$counts)
p_e3=h_e3_100$counts/sum(h_e3_100$counts)
plot(p_e3)
q_e3=p_e3[p_e3>0]
length(q_e3) #shorter than p
entropy_e3 = sum(-q_e3*log2(q_e3))
entropy_e3
max_entropy_e3=log2(100)
max_entropy_e3


#===============================================================================
#          summary of Statistics and Entropy of GLM Error                      #
#===============================================================================
mean(e4)
sd(e4)
var(e4)
median(e4)
IQR(e4)
skewness(e4)
kurtosis(e4)
min(e4)
max(e4)
range(e4)
h_e4=hist(e4)


h_e4_100=hist(e4,breaks = 100)
length(h_e4_100$counts)
sum(h_e4_100$counts)
p_e4=h_e4_100$counts/sum(h_e4_100$counts)
plot(p_e4)
q_e4=p_e4[p_e4>0]
length(q_e4) #shorter than p
entropy_e4 = sum(-q_e4*log2(q_e4))
entropy_e4
max_entropy_e4=log2(100)
max_entropy_e4

