library(scales)
library(formattable)
library(DT)
rm(list=ls())
ds <- read.csv("ofm_april1_population_final.csv",header=TRUE)

ds2 <- subset(ds,,select=c(3,4,5,12))
ds2$Growth <- with(ds2,percent((X2017.Population.Estimate-X2010.Population.Census)/X2010.Population.Census))
#ds2$pop_pct = percent(ds2$CENSUS2010POP / sum(ds2$CENSUS2010POP))
ds2$ten <- ds2$Growth > .10
ds2$X2010.Population.Census <-formatC(ds2$X2010.Population.Census, format="d", big.mark=",")
ds2$X2017.Population.Estimate <-formatC(ds2$X2017.Population.Estimate, format="d", big.mark=",")
#ds4<-sort(ds2$Growth,decreasing=FALSE) 

names(ds2) <- c("County", "City","2010 Population","2017 Est. Population",
                "Population Growth","> 10% Growth")
#population[order(population$age),]

#ds4 <- ds2[order(ds2$Growth),]
ds2 %>%
  DT::datatable(options = list(order = list(list(5, 'desc'))))
ds3 <- formattable(ds2,list(
  
            "Population Growth"=color_tile("white","orange"),
           "> 10% Growth" = formatter("span",
                style = x ~ style(color = ifelse(x, "green", "red")),
                x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))))

ds3 %>% as.datatable(options= list(order = list(list(5, 'desc'))))

#formatPercentage('Population Growth', 2)
#as.datatable(ds3) #%>% formatPercentage('Population Growth', 2)
#%>% options = list(order = list(list(5, 'desc')))
