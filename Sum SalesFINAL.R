library(doBy)
library(data.table)
library(ggplot2)

setwd('/Users/brechtvanbuggbrechtenhout')

# Read in the Sales Data

Sales <- fread("F:Desktop/Bibitor/PurchasesFINAL6302018a.csv",header=TRUE,stringsAsFactors = FALSE)

# sum of sales
sum(SalesFINAL6302018a$SalesDollars)
colnames(SalesFINAL6302018a)
