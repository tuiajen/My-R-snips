#My R snips
#read an EXCEL file
install.packages("readxl")
library(readxl)
my_data <- read_excel("myfilename.xlsx")

# Here is an example of a correlation
corr <- read.csv ("corr_ex.csv")  # get the file (import some data to test corr)
cor.test(corr$Normal,corr$Hypervent,method="pearson") #you can put spearman or kendall in here too
rm(list=ls()) #clear variable list

#get data type sapply(df2,class) #(or typeof instead of class)
#good link for scatterplots: http://www.cookbook-r.com/Graphs/Scatterplots_(ggplot2)/
#plain ol plot
attach(df2)
plot(x,y)
abline(lm(y~x), col="red")lines(lowness(y,x, col="blue"))

#import a file from a copy from Excel. 
 read.table(file="clipboard", sep = "\t", header = TRUE)

#compare two lists
x <- c('q','r','s','t')
y <- c('r','u','v')

#compare the two vectors
#one way 
intersect(unique(x), unique(y))
#another way
c <- x[(x %in% y)]
d <- y[(y %in% x)]
###
all(unique(c) == unique(d)) #not sure what this means
intersect(unique(x), unique(y))
