#Install Packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
#Load Packages
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(readxl)
#Import Transaction Data and Purchase Behaviour Data
transaction_data <- read_excel("QVI_transaction_data.xlsx")
purchase_behaviour <- read.csv("QVI_purchase_behaviour.csv")
#Check if Data Loaded Correctly
head(transaction_data)
head(purchase_behaviour)
#Data Structure
str(transaction_data)
#summary of the data
summary(transaction_data)
#Data Cleaning
#1. Missing val
colSums(is.na(transaction_data))
colSums(is.na(purchase_behaviour))
#2. dups
sum(duplicated(transaction_data))
#there is one dupe therefore
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
transaction_data <- distinct(transaction_data)
sum(duplicated(transaction_data))
#fix date formats
head(transaction_data$DATE)
transaction_data$DATE <- as.Date(transaction_data$DATE,
                                 origin = "1899-12-30")
head(transaction_data$DATE)

#Analysis Of Products
unique(transaction_data$PROD_NAME)
#remove salsa products
transaction_data <- transaction_data %>%
  filter(!str_detect(PROD_NAME, "Salsa"))

#Create New Features
#1 pack size col
transaction_data$PACK_SIZE <- str_extract(transaction_data$PROD_NAME,
                                          "\\d+")
summary(transaction_data$PACK_SIZE)
#convert to numeric
transaction_data$PACK_SIZE <- as.numeric(transaction_data$PACK_SIZE)
summary(transaction_data$PACK_SIZE)
#2 brand col
transaction_data$BRAND <- word(transaction_data$PROD_NAME, 1)
unique(transaction_data$BRAND)

# visualization: find outliers
boxplot(transaction_data$PROD_QTY)
transaction_data %>%
  filter(PROD_QTY > 10)
#remove outliers
transaction_data <- transaction_data %>%
  filter(PROD_QTY < 200)
boxplot(transaction_data$PROD_QTY)

#MERGE BOTH DATA SETS TO FIND INSIGHTS
merged_data <- merge(transaction_data,
                     purchase_behaviour,
                     by = "LYLTY_CARD_NBR")
head(merged_data)

#BUSINESS ANALYSIS
#1 total sales: chip revenue
sum(merged_data$TOT_SALES)
#2 sales by lifestage
sales_lifestage <- merged_data %>%
  group_by(LIFESTAGE) %>%
  summarise(Total_Sales = sum(TOT_SALES))
sales_lifestage
#3 sales by premium customers
sales_premium <- merged_data %>%
  group_by(PREMIUM_CUSTOMER) %>%
  summarise(Total_Sales = sum(TOT_SALES))
sales_premium
#what size of pack customers prefer
pack_sales <- merged_data %>%
  group_by(PACK_SIZE) %>%
  summarise(Total_Sales = sum(TOT_SALES))
pack_sales
#what brand sales most or least
brand_sales <- merged_data %>%
  group_by(BRAND) %>%
  summarise(Total_Sales = sum(TOT_SALES)) %>%
  arrange(desc(Total_Sales))
brand_sales
#Final Visuals 
ggplot(sales_lifestage,
       aes(x = LIFESTAGE,
           y = Total_Sales,
           fill = LIFESTAGE)) +
  geom_bar(stat = "identity")

write.csv(merged_data,
          "merged_data.csv",
          row.names = FALSE)
