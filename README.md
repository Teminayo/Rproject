library(tidyverse)
library(lessR)
library(dplyr)
library(psych)
library(plyr)



#importing dataset
covid <- read.csv("COVID_Dataset.csv")    


SUPERSTORE <- read.csv("Superstore_Dataset.csv")
#headernames
colnames(SUPERSTORE)


#descriptive analysis
summary(SUPERSTORE)
summary(SUPERSTORE$Profit)
describe(SUPERSTORE)

describe(SUPERSTORE)
describe(SUPERSTORE$Quantity)
summary(SUPERSTORE$Quantity)


#measureof count
attach(SUPERSTORE)
table(SUPERSTORE$Region, SUPERSTORE$Ship.Mode)
table(SUPERSTORE$Region, SUPERSTORE$Segment)
table(Region, Segment)

#measureofcentraltendency
mean(SUPERSTORE$Sales)
mean(Sales)
median(Sales)
median(Quantity)
mode(Quantity)


#MEASURE OF DISPERSION
var(SUPERSTORE$Profit)
range(SUPERSTORE$Profit)
range(SUPERSTORE$Sales)
sd(Sales)
max(Sales) - min(Sales)

#forhelp
?sd
?var
?range




#measureofnormality
hist(Sales, xlab = "sales made by the store",
     ylab = "frequency",
     main =  "Histogram for sales of superstore",
     col = "green",
     probability = TRUE)
lines(density(Sales))     

boxplot(Sales, 
        xlab = "sales",
        ylab = "freq",
        main = "boxplot for sales of superstore",
        col = "blue")

skew(Sales)
kurtosis(Sales)
count(SUPERSTORE, Sales)
count(SUPERSTORE, Quantity)
count(Region)



#inferential analysis
min(Quantity)
max(Quantity)
#recording continous variable to a categorical variable
SUPERSTORE$Quantity_cat <- ifelse(SUPERSTORE$Quantity <=8,"Lower quantity",
                                  ifelse(SUPERSTORE$Quantity >8, "Higher quantity", 0))

view(SUPERSTORE)
count(SUPERSTORE$Quantity_cat)


#Examine the mean difference of those selling  higher quantity and lower quantity
t.test(Sales~Quantity_cat, alt="two.sided", conf=0.95, data = SUPERSTORE)


view(t.test(Sales~Quantity_cat, alt="two.sided", conf=0.95, data = SUPERSTORE))
#paired ttest
t.test(Sales,Profit, alt="two.sided", conf=0.95, paired = TRUE, data = SUPERSTORE)


#ONESAMPLE ttest
mean(Sales)
t.test(Sales, mu=150, alternative = "less", conf=0.95)

#0ne_wayAnova 
aov(Profit$Region)
oneway_anova <- aov(Profit$Region)
summary(oneway_anova)

#TWOWAY ANOVA
summary(aov(Profit~Region + Segment))

summary(aov(Profit~Segment + Region))


#chisquare
table(Ship.Mode, Category)
chisq.test(Ship.Mode, Category)

#correlation
cor.test(Sales, Quantity)

#predictive analysis

#simple linear regression
summary(lm(Profit~Quantity))

#multiplelinear regression
summary(lm(Profit~Quantity + Discount))
