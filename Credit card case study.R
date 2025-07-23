setwd("D:/Avi Laptop/R programming Analytixlabs/Case studies")
repayment <-read.csv("Repayment.csv")
customeracq <- read.csv("Customer Acqusition.csv")
spend <-read.csv("spend.csv")
library(dplyr)
library(lubridate)
library(ggplot2)
customeracq$Age <- ifelse(customeracq$Age<18,mean(customeracq$Age),customeracq$Age)
cust_spend <- inner_join(customeracq,spend,by="Customer")
cust_spend$Amount <- ifelse(cust_spend$Amount>cust_spend$Limit,0.5*cust_spend$Limit,cust_spend$Amount)
cust_spend$Amount>cust_spend$spend
cust_repay <- inner_join(customeracq,repayment,by="Customer")
head(cust_repay)
cust_repay[Amount>Limit]
cust_repay$Amount <- ifelse(cust_repay$Amount>cust_repay$Limit,cust_repay$Limit,cust_repay$Amount)
# How many distinct customers exist?
count(distinct(customeracq,Customer)) #100
# How many distinct categories exist?
count(distinct(customeracq,Segment)) #5
# What is the average monthly spend by customers?
typeof(spend$Month)
spend$Month <- as.Date(spend$Month,format="%d-%b-%y")
spend %>%
  group_by(year(Month),month(Month)) %>%
  summarise(month_average = mean(Amount)) %>% View()
# What is the average monthly repayment by customers?
typeof(repayment$Month)
repayment$Month <- as.Date(repayment$Month,format = "%d-%b-%y")
repayment %>% group_by(year(Month),month(Month)) %>% summarise(month_average=mean(Amount)) %>%View()
# If the monthly rate of interest is 2.9%, what is the profit for the bank for each month?(Profit is defined as 
#  Interest earned on monthly profit. Monthly profit =Monthly repayment - Monthly spend. Interest is only earned on positive profit and not on negative amount)
InterestRate=2.9/100
cust_spend$Month <- as.Date(cust_spend$Month,format = "%d-%b-%y")
cust_spend_repay <-inner_join(cust_spend,repayment,by="Customer")
head(cust_spend_repay)
cust_spend_repay$Amount.y
df <- cust_spend_repay %>% group_by(year(Month.x),year(Month.y),month(Month.x),month(Month.y))%>% select(16,18,17,19,12,15)
df$`year(Month.x)` <- sort(df$`year(Month.x)`)
df$`month(Month.x)` <- sort(df$`month(Month.x)`)
df$`year(Month.y)`<- sort(df$`year(Month.y)`)
df$`month(Month.y)`<- sort(df$`month(Month.y)`) 
df$profit<-df$Amount.y-df$Amount.x
df$Interestearned <- ifelse(df$profit>0,df$profit*InterestRate,df$profit)

# What are the top 5 products types?
head(spend)
count(spend$Type)
spend %>% select(Amount,Type) %>% group_by(Type) %>%  sum(spend$Amount)
distinct(spend,Type)
a1 <- table(spend$Type)
a1 <- sort(a1,decreasing = TRUE)
typeof(a1)
spend$Type
top5 <- spend %>% select(Type,Amount)%>%arrange(desc(Amount))%>%group_by(Type)%>%summarise(sum=sum(Amount))
as.data.frame(top5)
sort(top5$sum,decreasing = TRUE)
# Top 5 product types are Petro,CAMERA,FOOD,Train ticket,AIR TICKET,

# Which city is having maximum spend?
cust_spend_acq <-inner_join(customeracq,spend,by="Customer")
head(cust_spend_acq)
cust_spend_acq%>%select(City,Amount)%>%arrange(desc(Amount))%>%group_by(City)%>%summarise(sum=sum(Amount))
# Cochin is having the maximum spend.
typeof(cust_spend_acq$Age)
# Which age group is spending more money?
cust_spend_acq$Age_Grp <- ifelse(cust_spend_acq$Age>=18 & cust_spend_acq$Age<25,"Youth",ifelse(cust_spend_acq$Age>=25 & cust_spend_acq<40,"Matured",ifelse(cust_spend_acq$Age>=40 & cust_spend_acq$Age<65,"Old",ifelse(cust_spend_acq>=65,"Senior","Seniormost"))))
cust_spend_acq%>%select(Age_Grp,Amount)%>%group_by(Age_Grp)%>%summarise(sum=sum(Amount))
# AGE GRP >25 AND <40 people are spending the most according to the data.

# Who are the top 10 customers in terms of repayment?
top_10 <- cust_repay%>%select(Customer,Amount) %>% group_by(Customer)%>%summarise(sum(Amount))
top_10<-as.data.frame(top_10)
top_10_dec<- top_10[with(top_10,order(-top_10[,2])),]
top_10_dec[1:10,1:2]

# Calculate the city wise spend on each product on yearly basis. Also include a graphical representation for the same.
# year(cust_spend_acq$Month)
a2<- cust_spend_acq%>%select(City,Product,Month,Amount) %>% group_by(year(Month),Amount)
a2_ord<-a2[with(a2,order(-a2[,5])),]
a2_ord<-a2_ord[,-3]
a4<- a2_ord%>%group_by(Product,City,`year(Month)`)%>%summarise(AMT=sum(Amount))
ggplot(data = a4,aes(City,AMT,fill=Product,color=factor(`year(Month)`)))+geom_bar(stat = "identity",position = "stack") 
library(plotly)
ggplotly(ggplot(data = a4,aes(City,AMT,fill=Product,color=factor(`year(Month)`)))+geom_bar(stat = "identity",position = "stack"))

# Create graphs for Monthly comparison of total spends, city wise
a5=cust_spend_acq%>%select(City,Product,Month,Amount) %>% group_by(month(Month),Amount)
a5_ord<- a5%>%group_by(City,`month(Month)`)%>% summarise(AMT2=sum(Amount))
ggplotly(ggplot(data = a5_ord,aes(City,AMT2,color=factor(`month(Month)`)))+geom_bar(stat = "identity",position = "stack"))

# Comparison of yearly spend on air tickets
a6<-cust_spend_acq%>%select(Type,Month,Amount)%>%group_by(year(Month),Amount)
a6<-a6[a6$Type=='AIR TICKET',]
a6<-a6[,-2]
a6<-a6%>%select(Type,Amount,`year(Month)`)%>%group_by(Type,`year(Month)`)%>%summarise(sum(Amount))
ggplot(data = a6,aes(Type,`sum(Amount)`,color=factor(`year(Month)`)))+geom_bar(stat = "Identity",position = "dodge")
ggplotly(ggplot(data = a6,aes(Type,`sum(Amount)`,color=factor(`year(Month)`)))+geom_bar(stat = "Identity",position = "dodge"))

# Comparison of monthly spend for each product (Look for seasonality that exists in time of spend)
a7 <- cust_spend_acq%>%select(Product,Month,Amount)%>%group_by(month(Month),Amount)
a7<- a7[,-2]
a7<-a7%>%group_by(Product,`month(Month)`)%>%summarise(sum(Amount))
ggplot(data = a7,aes(x=Product,y=`sum(Amount)`,color=factor(`month(Month)`)))+geom_bar(stat = "Identity",position = "dodge")
ggplotly(ggplot(data = a7,aes(x=Product,y=`sum(Amount)`,color=factor(`month(Month)`)))+geom_bar(stat = "Identity",position = "dodge"))

ggplotly(ggplot(data = a7,aes(x=Product,y=`sum(Amount)`,color=factor(`month(Month)`)))+geom_bar(stat = "Identity",position = "stack"))
# I think that people usually purchase the products at the sbeginning of the year and as we approach the end of the year, the demand of the products falls.

# Write a User defined Function for the following analysis:
# You need to find top 10 customers for each city in terms of their repayment amount by different products 
# and by different time periods i.e. year or month. 
# The user should be able to specify the product(Gold,Siver,Platinum) and time period(yearly or monthly)
# and the function should automatically take these inputs while identifying the top 10 customers.


custacq_repay<-inner_join(customeracq,repayment,by="Customer")
custacq_repay<- custacq_repay%>%group_by(year(Month),month(Month))
custacq_repay <- custacq_repay[,-9]
typeof(custacq_repay)
as.data.frame(custacq_repay)
a8 <- function(product,Time_Period) {
  custacq_repay$Amount <- sort(custacq_repay$Amount,decreasing = TRUE)
  custacq_repay%>%filter(Product==product,Time_Period==`year(Month)`|Time_Period==`month(Month)`) %>% head(10) %>% View()
}  
a8(product="Gold",Time_Period = 2005)






















# a8 <- function(product,Time_Period) {
# custacq_repay%>%filter(Product==product,Time_Period==`year(Month)`|Time_Period==`month(Month)`)
# }  
# a8(product="Gold",Time_Period = 2004)
# typeof(custacq_repay$Product)
# filter(custacq_repay$Product)
# 
# 
# 
# 
# 
# custacq_repay<-inner_join(customeracq,repayment,by="Customer")
# custacq_repay<- custacq_repay%>%group_by(year(Month),month(Month))
# custacq_repay <- custacq_repay[,-9]
# typeof(custacq_repay)
# as.data.frame(custacq_repay)
# a8 <- function(product,Time_Period) {
#   custacq_repay%>%filter(Product==product,Time_Period==`year(Month)`|Time_Period==`month(Month)`)%>%View()
# }  
# a8(product="Gold",Time_Period = 2004)
# typeof(custacq_repay$Product)
# filter(custacq_repay$Product)
# a8(product="Gold")
# # custacq_repay$`year(Month)`<-as.factor(custacq_repay$`year(Month)`)
# # head(custacq_repay$Product  custacq_repay$`year(Month)`)
# # head(custacq_repay$Product & custacq_repay$`year(Month)`)
# print(custacq_repay$Product & custacq_repay$`year(Month)`)
# custacq_repay$`year(Month)` <- as.character(custacq_repay$`year(Month)`)
# custacq_repay$`year(Month)` <- as.integer(custacq_repay$`year(Month)`)
# custacq_repay$Product <- as.factor(cust)
# typeof(custacq_repay$`year(Month)`)
# typeof(custacq_repay$Product) 
