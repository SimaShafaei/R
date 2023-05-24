library(nycflights13)
library(ggplot2)
library(dplyr)
#=========================================================================================================
# Question 1
# a. first statement shows that the result is not empty and the second shows the number of missing values
filter(flights,is.na(tailnum))
sum(is.na(flights$tailnum))

# b. 
sum(flights$origin=="LGA")/nrow(flights)
sum(flights$origin=="JFK")/nrow(flights)
sum(flights$origin=="EWR")/nrow(flights)

#c. first null values in dep_dely are deleted, then mean of dep_delay is computed for each origon 
clearDelay=filter(flights,!is.na(dep_delay))
gflights=group_by(clearDelay,origin)
summarize(gflights,mean(dep_delay))

# c.[Optional] the result shows that sum square and f-value of LGA is obviously more than sum square and f-value of JFK
# so there is a significant difference between LGA and JFK 
one_way=aov(dep_delay~ as.factor(origin),data=flights)
summary(one_way)
one_way=aov(dep_delay~ as.factor(origin=="JFK"),data=flights)
summary(one_way)
one_way=aov(dep_delay~ as.factor(origin=="LGA"),data=flights)
summary(one_way)
 

# d.
EWR_flights=filter(flights,origin=="EWR")
count(filter(EWR_flights,dest=="IAH"))/count(EWR_flights)
nrow(filter(EWR_flights,dest=="IAH"))/nrow(EWR_flights)

# e. first we remove the null to make sure that difference are not cause by null values. 
# then in not_equal we keep all lines that dep_delay!=dep_time - sched_dep_time. last line shows the percentage of cases
# that equality is not hold
flights_dep=select(flights,dep_time:dep_delay)
dim(flights_dep)
flights_dep_clean=na.omit(flights_dep)
dim(flights_dep_clean)
not_equal=filter(flights_dep_clean,dep_delay!=dep_time - sched_dep_time)
head(not_equal)
count(not_equal)
count(not_equal)/count(flights_dep_clean)


# f.  the first line gives a table that 0 value the carriers that do not 
# fly out of the origin airports. 
# second line give a more clear result. in this table 1 shows that the carriers
# that do not fly out of corresponding origin and last row in table shows number 
# of origin that the carrier has not fly out
prop.table(table(flights$origin,flights$carrier))
addmargins(prop.table(table(flights$origin,flights$carrier))==0)

# g. To visualize the relationship I used a point and also a box plot. (box plot is more
# informative because it shows mean and variance of dep_dely for each origin )
ggplot(data=flights) + geom_point(mapping=aes(x=factor(origin), y=dep_delay))
ggplot(data=flights) + geom_boxplot(mapping=aes(x=factor(origin), y=dep_delay))

#h. to show some airline are better we can use a box plot and compare the average delay 
# of each airline 
ggplot(data=flights) + geom_boxplot(aes(x=factor(carrier), y=dep_delay))

# we can also compute the mean of delay for each airline and plot the mean only to be more clear and easier 
# for compairing
clearDelay=filter(flights,!is.na(dep_delay))
gflights2=group_by(clearDelay,carrier)
mean_delay=summarize(gflights2,mean(dep_delay))
ggplot(data=mean_delay) + geom_point(mapping=aes(x=factor(carrier), y=`mean(dep_delay)`))


#i. first we run one series of Anova test to select variables that have more influence on arrival delay
#sched_arr_time,month, dep_delay then we can run linear regression model 
one_way=aov(arr_delay~ as.factor(origin),data=flights)
summary(one_way)
one_way=aov(arr_delay~ as.factor(dest),data=flights)
summary(one_way)
one_way=aov(arr_delay~ as.factor(carrier),data=flights)
summary(one_way)
one_way=aov(arr_delay~ dep_delay,data=flights)
summary(one_way)
one_way=aov(arr_delay~ as.factor(month),data=flights)
summary(one_way)
one_way=aov(arr_delay~ as.factor(day),data=flights)
summary(one_way)

lmgraph = ggplot(flights, aes(x=arr_delay, y=dep_delay))+geom_point()
lmgraph = lmgraph +geom_smooth(method="lm")
lmgraph

model1=lm(arr_delay ~ sched_arr_time + month + dep_delay, data=flights)
summary(model1)
# the obtained model is not very good



#=========================================================================================================
# Question 2

# a.
setwd("D:/PhD+/semester 3/data management and analysis/datasets")
animals=read.csv("animals.csv")
str(animals)

# b.
animals$sex = ifelse(animals$sex %in% c(""," ","NA"), NA, animals$sex)

# I also checked all other attributes with three line below to make sure there 
# is not any null value saved with different format
g=group_by(animals,taxa)
result=summarise(g)
View(result)


# c.
count(select(distinct(animals,species_id),species_id))
animal_sid_group=group_by(animals,species_id)
count(animal_sid_group)
pie(table(animals$species_id))
barplot(table(animals$species_id))


#d. based on Anova result, species has the maximum f value and low p value So it 
# has the most influence on weight among other three variables after species genus 
# has more influence and sex has least influence
animals$sex <- addNA(factor(animals$sex),ifany = FALSE)
animals$genus <- addNA(factor(animals$genus),ifany = FALSE)
animals$species <- addNA(factor(animals$species),ifany = FALSE)

one_way1=aov(weight~sex,data=animals)
summary(one_way1)
one_way2=aov(weight~genus,data=animals)
summary(one_way2)
one_way3=aov(weight~species,data=animals)
summary(one_way3)

#e. the reslut shows that there is some positive correlation between these two variables
clean_animalparts=na.omit(select(animals,hindfoot_length,weight))
cor(clean_animalparts$weight,clean_animalparts$hindfoot_length)
