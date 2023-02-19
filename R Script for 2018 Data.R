####Set Working Directory####
#Click on Session dropdown menu...you can select the folder where your data is located

setwd("C:/Users/w.goldman/OneDrive - Northeastern University/Bibitor Data 2018")

####Checking for Proper Approval####
#load dplyr package...if you have not done so already, install the package
library("dplyr")

#Read in data#
purchases <- read.csv("InvoicePurchases6302018a.csv")

#add a column to the existing datafram
#Create a new variable (column) for combined purchase order and freight amount.
#Add column to dataframe to create a combined purchase order and freight total dollar#

purchases$totaldollars <- purchases$Dollars+purchases$Freight #a new column was added to the purchases dataframe.  Notice the $

#Create a new variable (column) for combined purchase order and freight amount.
#Add column to dataframe to create a combined purchase order and freight total dollar#

purchases$totaldollars <- purchases$Dollars+purchases$Freight #a new column was added to the purchases dataframe.  Notice the $


#Here are a few ways to approach the question of whether there was proper sign-off.
#Option 1 = Find the maximum approval of non-authorized personnel for P.O.'s <= $250,000

#table1 is  a new dataframe (think of a dataframe as an new excel spread) that is created from the purchases dataframe.
table1 <- purchases %>%                  #note the %>% are 'pipes' that mean "then"
  group_by(Approval) %>%               #creates groups (rows) based on who is approving the P.O.
  select(totaldollars, Approval, PONumber, PODate, Quantity) %>%       #select will keep the columns from the purchases dataframe
  summarize(n(),min(totaldollars), max(totaldollars) ) #n = count,

#Here are a few ways to approach the question of whether there was proper sign-off.
#Option 2 = Filter P.O.'s greater than or equal to $250,000 then select the appropriate columns.  
#filter purchases >= to $250,000 to check for approval based on PO Dollar Amount
#filters will keep (eliminate) rows 

largepurchases <- purchases %>% #new dataframe large purchases from the purchases dataframe
  filter(totaldollars >= 250000) %>%
  select(VendorNumber, VendorName, InvoiceDate, PONumber, PODate, Quantity, Dollars, Freight, totaldollars, Approval)

#filter purchases >= to $250,000 to check for approval based on PO Dollar Amount
#Option 3 - Filter P.O.'s greater than or equal to $250,000 then look for someone else besides the proper personnel 
largepurchases1 <- purchases %>%
  filter(totaldollars >= 250000) %>%
  filter(Approval != "Frank Delahunt")
select(VendorNumber, VendorName, InvoiceDate, PONumber, PODate, Quantity, Dollars, Freight, totaldollars, Approval)



#filter totaldollars >= $250,000
largepurchases2 <- purchases %>%
  filter(totaldollars >= 250000) %>%
  select(VendorNumber, VendorName, InvoiceDate, PONumber, PODate, Quantity, Dollars, Freight, totaldollars, Approval)


