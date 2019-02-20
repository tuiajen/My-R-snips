#' Load data
df2 <- read.csv("grade.csv")

# independent 2-group t-test
#t.test(y~x) # where y is numeric and x is a binary factor
# from https://www.statmethods.net/stats/ttest.html

mytest <- t.test(df2$gr~df2$location)
location_L <- subset(df2,df2$location=='L')
location_m <- subset(df2,df2$location=='M')

#get this for my effect size calculator
# https://www.socscistatistics.com/effectsize/default3.aspx

mean(location_L$gr)
nrow(location_L)
sd(location_L$gr)

mean(location_m$gr)
nrow(location_m)
sd(location_m$gr)
