#Use the airquality data set to do the following in R
#1.	Remove all records with NA entries. Find the number of the available records or rows.
nrow(airquality)
d=na.omit(airquality) #remove null variables
head(d)
nrow(d)

#2.	Plot the dependent variable Ozone  as a function of the independent variable Solar.R 
plot(d$Ozone~d$Solar.R)

#3.	Evaluate the predictions using the following models for Ozone ~ Solar.R:
#  a.	Eyeball linear equation
#    (10, 1) (300,75) slope=(75-1)/(280-10)=0.27
#     interception: 0
#     estimation model is: Ozon =  0.27Solar.R
p1=0.27*d$Solar.R
lines(d$Solar.R,p1,col=2)  #col=color
#  b.	Linear model; lm
m2=lm(d$Ozone~d$Solar.R)
summary(m2)
c2=coef(m2)
p2=c2[1] + c2[2]*d$Solar.R
lines(d$Solar.R,p2,col=3)

#  c.	Second order polynomial 
x2=d$Solar.R * d$Solar.R
m3=lm(d$Ozone~d$Solar.R+x2)
summary(m3)
# we can not reject the hypothesis test for three variables to be zero (all of them are significant)
# goodness of fitness (50 percent) is higher than lm so it has a better goodness of fit
# p value for the overal model is almost zero ==> the model is significant 
c3=coef(m3)
p3= c3[1]+c3[2]*d$Solar.R+c3[3]*x2
lines(d$Solar.R,p3,col=4)

#  d.	Generalized linear model; glm
m4 = glm(d$Ozone~d$Solar.R, family = "poisson")
summary(m4)
c4=coef(m4)
p4 = exp(c4[1]+c4[2]*d$Solar.R)
lines(d$Solar.R,p4,col=5)   # the model is not linear 

#4.	In each case present the following: coefficient, summary statistics of the error vector, and SSE. Also include a plot that shows the response of these models.
#for Eyeball:
e1 = p1 - d$Ozon
summary(e1)
hist(e1)
# the histogram is not a normal distribution and mean is not 0 so it is not a good estimation
SSE1=sum(t(e1)*e1)
SSE1

c2
e2=p2-d$Ozone
summary(e2)
hist(e2)
SSE2=sum(t(e2)*e2)
SSE2

c3
e3=p3-d$Ozone
summary(e3)
hist(e3)
SSE3=sum(t(e3)*e3)
SSE3

c4
e4=p4-d$Ozone
summary(e4)
hist(e4)
SSE4=sum(t(e4)*e4)
SSE4

# SSE1 = 132417.4 which is a big number so the model is not good
#5.	Which model is the best? And why?


  