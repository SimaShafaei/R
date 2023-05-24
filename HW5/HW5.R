setwd("D:\\PhD+\\semester 4\\Datamining with linear model\\assignments\\HW5")
veneer=read.table("VENEER.txt", header = TRUE)
head(veneer)
veneer$BRAND=as.factor(veneer$BRAND)
summary(veneer$BRAND)
length(veneer$BRAND)
boxplot(veneer$WEAR~veneer$BRAND)
anova=aov(veneer$WEAR~veneer$BRAND)
summary(anova)
attributes(anova)
anova$coefficients
#H0 : means for different brands are equal 
#Ha : there are at least two  brands that have different means
# p value is smaller than 0.05 ==> reject H0 with confidance level 95%
# it is also clear from box plot