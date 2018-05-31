##This is one way to do ANOVA, class_time was day, evening, mixed
install.packages("plyr")
library(plyr)

li <- read.csv("PT_Age_NON_RS.csv")
overall <- ddply(li, c("CLASS_TIME"), 
                 summarise, Mean=mean(age)
                 , Median=median(age), Std=sd(age))
overall
count(li,"CLASS_TIME")


plot(age ~ CLASS_TIME,data=li,col=rainbow(12))
attach(li)
results <- aov(age~CLASS_TIME,data=li)
oneway.test(age~CLASS_TIME)
summary(results)
pairwise.t.test(age,CLASS_TIME,p.adj="bonferroni")
exportme<-TukeyHSD(results)
exportme
capture.output(exportme,file="li_tukey.doc")
plot(exportme)

##This is another installment for ANOVA to work survey data
survey <- read.csv ("Results_DatafromSurvey.csv")  
survey$Cat <- as.factor(survey$Cat)
survey$trustee <- as.factor(survey$customerid)
str(survey)
tapply(survey$Score,survey$Cat, mean)
check<-aggregate(survey$Score, list(cat=survey$Cat, customerid=survey$customerid),mean)
check[order(check$cat,check$customerid),]

#Do ANOVA on entire survey by Cat.
results1 = aov(Score~Cat, survey)
summary(results1)
TukeyHSD(results1, conf.level=0.95)

#Do ANOVA on entire survey by Trustee.
results2 = aov(Score~customerid, survey)
summary(results2)
TukeyHSD(results2, conf.level=0.95)

#Do ANOVA on Trustees by Cat.
my_subset <- survey[ which(survey$Cat=='11'),]
results3 = aov(Score~trustee, my_subset)
summary(results3)
TukeyHSD(results3, conf.level=0.95)
