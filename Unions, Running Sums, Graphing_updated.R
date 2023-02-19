#Necessary Packages

install.packages("lubridate")
install.packages("tidyverse")

install.packages('doBy')
install.packages("readr")
install.packages("ggthemes")
install.packages("fy")


#remember you have to load the packages. e.g. below with the #
library(tidyverse)

#library(doBy)
#library(readr)
#library(ggthemes)
#####2019 Store 3####

(WD<-getwd())


library(readr)

#####2019 Store 3#####
#I used the Import Dataset using readr (see Environment tab in the top right hand portion)

Sales2019 <- read_csv("C:/Users/w.goldman/OneDrive - Northeastern University/Bibitor Data/Bibitor Data 2019/SalesFINAL6302019.csv", 
                      col_types = cols(Brand = col_character(), 
                                       Classification = col_character(), 
                                       SalesDate = col_date(format = "%Y-%m-%d"), 
                                       Store = col_character(), VendorNo = col_character()))

#Just Store 3
library(dplyr)
store3sales2019 <- Sales2019 %>%
  filter(Sales2019$Store == "3") %>%
  select(everything())

#Check figure
sum(store3sales2019$SalesDollars)

#Writing to a CSV file.  Unnecessary but wanted to show you how to export file.
write.csv(store3sales2019, "C:/Users/w.goldman/OneDrive - Northeastern University/6205 Auditing and Big Data/R Folder/R Code for Bibitor\\2019store3sales.csv", row.names = F)



#####2018 Store 3##### 
#I used the Import Dataset using readr (see Environment tab in the top right hand portion)

Sales2018 <- read_csv("C:/Users/w.goldman/OneDrive - Northeastern University/Bibitor Data/Bibitor Data 2018/SalesFINAL6302018a.csv", 
                      col_types = cols(Brand = col_character(), 
                                       Classification = col_character(), 
                                       SalesDate = col_date(format = "%Y-%m-%d"), 
                                       Store = col_character(), VendorNo = col_character()))


#notice if you %>% doesn't work, make sure to load dplyr from library.  %>% means "then"


library(dplyr)

x2018store3sales <- Sales2018 %>%
  filter(Sales2018$Store == "3") %>%
  select(everything())

#Writing to a CSV file.  Unnecessary but wanted to show you how to export file.

write.csv(store3sales2018, "C:/Users/w.goldman/OneDrive - Northeastern University/6205 Auditing and Big Data/R Folder/R Code for Bibitor\\2018store3sales.csv", row.names = F)

#remove the initial big files
remove(Sales2018, Sales2019)

####Join the sales####
#Look what happens to the the rows :(

library(dplyr)
Store3Joined <- full_join(store3sales2018,store3sales2019, by = "Brand")

#head function just lists top 5 rows by default. Can be a useful function depending on number of columns
head(Store3Joined)

#let's remove the join so we don't get confused
remove(Store3Joined)

setwd("~/Desktop/ACCT 6205/Bibitor")

####What happened with the Join???? SOMETHING NOT GOOD!!!!!####

Store3_2018_2019 <- union(X2018store3sales,X2019store3sales)

####Now what happened with Union?####
#Look at number of rows and # of columns.  The columns are consistent and the rows is the sum of the other files


#check figures
sum(X2018store3sales$SalesDollars)+sum(X2019store3sales$SalesDollars)
sum(Store3_2018_2019$SalesDollars)



####Graphing####




####Running Totals####
library(dplyr)
#I included dplyr just to make sure nothing was masked
#In the dplyr package, mutate adds a column
by_date <- Store3_2018_2019 %>%
  arrange(SalesDate) %>%
  mutate(CumSales=cumsum(SalesDollars)) %>%
  mutate(totalSales = sum(SalesDollars)) %>%
  mutate(percent_of_total = CumSales/totalSales*100)

#View the data.  What is the totalSales?  I told it to sum the sales dollars for two years

####How can we fix the date issue####
# I want it by year....so I need to separate out the date

#need the separate function so I ran tidyverse but could run tidyr
library(tidyverse)
by_date_year <- Store3_2018_2019 %>%
  arrange(SalesDate)%>%
  separate(SalesDate,c("year","month","day"),remove = F)%>%
  group_by (year) %>%
  mutate(CumSales=cumsum(SalesDollars)) %>%
  mutate(totalSales = sum(SalesDollars)) %>%
  mutate(percent_of_total = CumSales/totalSales*100)

#Look at the data.  I'm getting closer but what's wrong with he totalSales????
#Hint: Bibitor does not have a 12/31 year end

#Need to get Fiscal Year

#This looks better!
install.packages("fy")
library(fy)
by_date_year$FY <-date2fy(by_date_year$SalesDate)

#Fortunately, I had created a variable for Fiscal Year (FY)
library(tidyverse)
by_date_year2 <- by_date_year %>%
  arrange(SalesDate)%>% #orders data
  group_by (FY) %>%
  mutate(CumSales=cumsum(SalesDollars)) %>%
  mutate(totalSales = sum(SalesDollars)) %>%
  mutate(percent_of_total = CumSales/totalSales*100)



install.packages("ggthemes")
library(ggplot2)
library(ggthemes)

str(by_date_year2)

ggplot(by_date_year2,aes(x=as.factor(FY), y=CumSales))+
  geom_point(color="red") +
  labs(x="Date", y="Cumulative Sales", 
       title = "Sales by FY")  

ggplot(by_date_year2,aes(x=by_date_year2$SalesDate, y = by_date_year2$SalesDollars)) + 
  geom_point(color="blue") + theme_economist()+
  labs(x="Date", y="Cumulative Sales", 
       title = "Sales by FY")



library(dplyr)
####Date Differential####
YearMonth <- 
  by_date_year2 %>%
  mutate(YearMonth = format(SalesDate, "%Y-%m"))

#sales by month in own dataset
#tidyverse
library(tidyverse)
SalesbyMonth <- YearMonth %>%
  group_by(YearMonth) %>%
  summarize(MonthlySales = sum(SalesDollars)) %>%
  arrange(YearMonth)%>%
  select(everything())

#understand your data...certainly the visualization could use some improvement
ggplot(SalesbyMonth,aes(x=YearMonth, y = MonthlySales)) +
  geom_point() + theme_economist()+
  labs(x="Date", y="Cumulative Sales", 
       title = "Sales by Month")


monthly_report <- SalesbyMonth %>%
  mutate(
    MoM = (MonthlySales - lag(MonthlySales)) / lag(MonthlySales),
    YoY = (MonthlySales - lag(MonthlySales, 12)) / lag(MonthlySales, 12)
  )

ggplot(monthly_report,aes(x=MoM, y = MonthlySales)) + 
  geom_point() + theme_economist()+
  labs(x="% Change from Prior", y="Sales", 
       title = "Changes by Month")  

#what about changes from PY
ggplot(monthly_report,aes(x=YoY, y = MonthlySales)) + 
  geom_point() + theme_economist()+
  labs(x="% Change from Prior", y="Sales", 
       title = "Changes from PY") 


####Sales by day####
library(lubridate)
as.Date(by_date_year2$SalesDate)

by_date_year2$day_week <-weekdays(by_date_year2$SalesDate)

ggplot(by_date_year2,aes(x=day_week, y = SalesDollars, color=(FY))) + 
  geom_col() + theme_economist()+
  labs(x="Day of Week", y="Sales", 
       title = "Sales by Day of Week") 

#to determine which packages are in your library
(.packages())
