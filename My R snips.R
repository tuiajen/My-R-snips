#My R snips
# Here is an example of a correlation
corr <- read.csv ("corr_ex.csv")  # get the file (import some data to test corr)
cor.test(corr$Normal,corr$Hypervent,method="pearson") #you can put spearman or kendall in here too
rm(list=ls()) #clear variable list
