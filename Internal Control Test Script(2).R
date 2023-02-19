# Test of Purchase Controls
# Purchase Control - All purchases (Materials plus freight) greater than or equal to $250,000 must be approved by the Director of Purchasing, Frank Delahunt.

Purchases <- read.csv("F:/HUB of Analytics Education/DataCreation/Bibitor/Jun2018/InvoicePurchases6302018a.csv", header =TRUE,stringsAsFactors = FALSE)

# Subset all POs greater than $250,000

ApprovedPurch <- subset(Purchases, Purchases$Dollars + Purchases$Freight >= 250000)

# Identify Approved Purchases without an approval

Exceptions <- ApprovedPurch[is.na(ApprovedPurch$Approval),]

# Based on the procedure performed, there are no exceptions noted.