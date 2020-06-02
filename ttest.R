#Copy from clipboard, test for normalization and variance 
rm(list = ls())
library(tidyverse)
library(rlang)
#copy off clipboard from SQL
df <- read.table(file = "clipboard", header=TRUE)

#create variables
#combine two items after reviewing data for appropriateness
df$item[df$item == "5481"] <- "5480"

mycluster <- '5479'
item_1 <- '5479'
item_2 <- '5480'



# Straight t-test by subsetted Cluster.
df_cluster <- df %>% 
  filter(cluster == mycluster)



#Check normal Distribution if > .05 then normal distribution if not use Wilcoxon rank test.
# Shapiro-Wilk normality test for item_1 
with(df_cluster, shapiro.test(gr_dec[item == item_1]))
# Shapiro-Wilk normality test for item_2
with(df_cluster, shapiro.test(gr_dec[item == item_2]))

#check the variances if > .05 & above is > .05 then go with classic t-test 
res.ftest <- var.test(gr_dec ~ item, data = df_cluster)
res.ftest

#classic t.test
# default is 2-sided for one-sided add in ,alternative = "greater") or "less"
my_ttest <- t.test(df_cluster$gr_dec ~ df_cluster$item,var.equal = TRUE)
my_ttest

# The unpaired two-samples Wilcoxon test (also known as Wilcoxon rank sum test 
#or Mann-Whitney test) is a non-parametric alternative to the unpaired two-samples 
#t-test, which can be used to compare two independent groups of samples. It’s used 
#when your data are not normally distributed.
res <- wilcox.test(gr_dec ~ item, data = df_cluster,
                   exact = FALSE)
res

#get other info to compute effect size in external calculator
item_a <- subset(df_cluster, df_cluster$item ==item_1)
item_b <- subset(df_cluster, df_cluster$item ==item_2)

mean(item_a$gr_dec)
nrow(item_a)
sd(item_a$gr_dec)

mean(item_b$gr_dec)
nrow(item_b)
sd(item_b$gr_dec)


#BASIC
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
