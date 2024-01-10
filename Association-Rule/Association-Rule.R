
#### -------------------------- ASSOCIATION RULE MINING ------------------------


### IMPORT THE DATA ------------------------------------------------------------

# set the environment
library(readr)
library(tidyverse)
library(reshape2)
#library(shiny)
#library(plotly)
library(stringr)
library(arules)


# bread_basket
bread_basket_df <- read.csv("bread_basket.csv")


dim(bread_basket_df)
# we can see that the dataset has 20507 observations and 5 variables 

summary(bread_basket_df)
# to have a general understanding of the distribution and class of the different variables


# remove some EXTRA variables from bread_basket_df (not necessary for the purpose 
# of the analysis)
bread_basket_df <- bread_basket_df[, !(names(bread_basket_df) %in% c("date_time", "period_of_day",
                                                                     "weekday_or_weekend"))]


## CONVERT THE DATAFRAME: convert the dataframe into a transaction.

##  reshape the dataframe into a wide format and then convert it to a logical matrix

# converting to a matrix
bread_basket_df$const = TRUE # 'const' is a variable that I created and all the 
#                               values are TRUE (value inside the matrix that we are 
#                               about to create)

# --> the other 2 variables (Transaction_Number and Item) will be our row and
#     columns respectively 

# remove duplicates
duplicate_rows_b <- sum(duplicated(bread_basket_df)) # there are 1620 duplicated rows
#                                                      so we remove them
bread_basket_df <- unique(bread_basket_df)


# check for non valid values
nulls_per_column_b <- colSums(is.na(bread_basket_df))
nulls_dataframe_b <- data.frame(Colonna = names(nulls_per_column_b), Valori_Mancanti = nulls_per_column_b)
# there are no NULL values


## create a unique set of transaction and item combinations

# reshape the matrix (this will create a lot of NULL values)
bread_basket_mat_prep <- reshape(data = bread_basket_df,
                                 idvar = "Transaction_Number",
                                 timevar = "Item",
                                 direction = 'wide') # to have a wide dataset

# drop the "Transaction_Number" (we do not need it because the row iteself is that transaction)
bread_basket_matrix <- as.matrix(bread_basket_mat_prep[, -1])

# clean up the missing values to be FALSE (transform the NULL to FALSE)
bread_basket_matrix[is.na(bread_basket_matrix)] <- FALSE

# clean up names (remove 'const' from the variable names)
colnames(bread_basket_matrix) <- gsub(x=colnames(bread_basket_matrix),
                                      pattern="const\\.", replacement = "")



##  look at how many items there are in each basket

# Store the resulting values in the item.in.basket variable.
item.in.basket <- apply(bread_basket_matrix, 2, sum)

# create a dataframe to store the results
item.in.basket_df <- data.frame(Count = item.in.basket)

# Get the actual number of baskets (transactions)
n.baskets <- nrow(bread_basket_matrix)

# Calculate the proportion of baskets with each item in it and add the column to item.in.basket_df
item.in.basket_df$percent.in.basket <- round(item.in.basket_df$Count / n.baskets * 100, 2)




### GENERATE SET OF RULES

# Generate a set of rules (this code will consider couples, triples, etc.)
rules <- apriori(apply(bread_basket_matrix, 2, as.numeric), 
                 parameter = list(supp = 0.04, # set support = 0.04
                                  conf = 0.01, 
                                  target = "rules"))

# --> conf = 0.01 indicates we are looking for rules with a minimum confidence of 1%.
#     In other words, we want rules for which, at least 1% of the times the 
#     items on the left side of the rule appear in a transaction, the 
#     items on the right side of the rule appear in the same transaction.



# Filter only the rules with lift > 1.0 (as requested)
rules <- subset(rules, lift > 1.0)

# inspect the rules that we have find
inspect(rules)

# RULES EXPLANATION
#        lhs         rhs      support    confidence coverage   lift     count
# [1] {Pastry} => {Coffee} 0.04754358 0.55214724 0.08610671 1.154168 450  
# [2] {Coffee} => {Pastry} 0.04754358 0.09938163 0.47839408 1.154168 450  
# [3] {Cake}   => {Coffee} 0.05472795 0.52695829 0.10385631 1.101515 518  
# [4] {Coffee} => {Cake}   0.05472795 0.11439929 0.47839408 1.101515 518  


