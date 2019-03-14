install.packages("RODBC")
library(RODBC)
library(dplyr)
rm(initdata) #use this to get rid of that variable 

setwd ("U:/path/to where I/need to be")

#Get my database table
conn <- odbcDriverConnect("driver=SQL Server;database=enter_db_name;server=enter_server_name;")
initdata <- sqlQuery(conn,paste("select * from my_table WHERE column_name = 'value';"))

#bring in the flat file
df <- read.csv("myflatfile.csv")
count(df,column_name) #count syntax - just added here for a quick reference
df[df$columnname=="X",]  #subset to get only the rows with a particular value

#Get only the columns I need from the table (which could have been done in the  SELECT connection)
df2 <- subset(initdata,,select=c(column_name_1, column_name_2,column_name_3))

df_inner<- merge(df,df2, by="column_name") #can also use column index such as by=1

#How to do other joins
df_outer <- merge(x = df, y = df2, by = "column_name", all = TRUE)
df_left <- merge(x = df, y = df2, by = "column_name", all.x = TRUE)
df_rightOuter <- merge(x = df, y = df2, by = "column_name", all.y = TRUE)
df_crossjoin <- merge(x = df, y = df2, by = NULL) 

# how to do select, filter, and pipes
df_select <- select(df,"column_name_1", "column_name_2", "column_name_3") # can do select(df,1,2,3)
df_filter <- filter(df,column_name == "F")
df_pipes <- df %>% 
   filter(column_name=="F") %>%
    select(3,4,5)  #can use column numbers instead of column names too
mutate_a <- df %>%
            mutate(src_rework = column_name*2) # do a math function #create a new column as a result of math function

# just see a few lines
df %>%
  mutate(src_rework = SRC*2) %>% 
  head
# do the math an remove the NA values too 
mutate_na <- df %>%
  mutate(src_rework = SRC*2) %>%
  filter(!is.na(column_name)) 

df %>%   #group counts
  group_by(column_name) %>%
  summarise(n=n())

df %>% #group and get the average of a numeric field
  group_by(column_name_1,column_name_2) %>%   #can do multiple columns
  summarise(new_column_name = mean(column_name_5, na.rm=TRUE)) %>%
  filter(!is.na(column_name_6))  #get rid of NA
  
df %>% #group and get the average of a numeric field
  group_by(column_name_1,column_name_2) %>%   #can do multiple columns
  summarise(new_column_name_1 = mean(column_name_5, na.rm=TRUE),
            new_column_name_2 = min(column_name_8)) # can also do multiple measures
  

#from: https://datacarpentry.org/R-genomics/04-dplyr.html
