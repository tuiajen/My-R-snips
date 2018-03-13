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