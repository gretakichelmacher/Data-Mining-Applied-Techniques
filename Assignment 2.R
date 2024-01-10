
### IMPORT THE DATA ------------------------------------------------------------

# set the environment
library(readr)
library(tidyverse)
library(reshape2)
library(stats)
library(psych)
library(fastICA)
library(factoextra)


# load the dataset 
winedata=read.csv("wine.data",sep=",", header=F)

# features names 
feature_names <- c(
  "cultivars",
  "Alcohol",
  "Malic acid",
  "Ash",
  "Alcalinity of ash",
  "Magnesium",
  "Total phenols",
  "Flavanoids",
  "Nonflavanoid phenols",
  "Proanthocyanins",
  "Color intensity",
  "Hue",
  "OD280/OD315 of diluted wines",
  "Proline"
)

# Assign features names to winedata columns 
colnames(winedata) <- feature_names


### EDA ------------------------------------------------------------------------

dim(winedata)
# we can see that our dataset has 178 observations and 14 variables 

summary(winedata)
# to have a general understanding of the distribution and class of the different variables

## The first variable 'cultivars' is the wine identifier (1-3)
# let's see how many instances there are in each class:

# Calculate the class distribution for 'cultivars'
class_distribution <- table(winedata$cultivars)

# Create a dataframe for class distribution
class_df <- data.frame(Class = names(class_distribution),
                       Frequency = as.numeric(class_distribution))

# Create a barplot to visualize the class distribution
ggplot(class_df, aes(x = Class, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Frequency), vjust = -0.5, size = 3) +  
  labs(title = "Class Distribution for cultivars",
       x = "Class",
       y = "Number of Instances") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

## from the wine.names file we know that there are no null values

## check for duplicates
duplicate_rows <- sum(duplicated(winedata)) # there are none


## Scaling the features
wine_s<-scale(winedata[,2:14])
# --> Standardization involves subtracting the mean of each column from its values and then 
#     dividing by the standard deviation. This process transforms the data into a standard 
#     normal distribution with a mean of 0 and a standard deviation of 1 for each variable.
#     This means that now the total variance is given by the total number of variables.


# ----------------------------------------------------------------------------------------------

## 1. PERFORM A PCA ON THIS DATA AND REDUCE TO 5PC


### PCA -----------------------------------------------------------------------------------------


# run PCA
pca_result <- prcomp(wine_s) 

# look at the summary of PCA
summary(pca_result) 
# --> Here we can see that the cumulative proportion in the 5PC is 0.80162. 
#     This means that when we use 5 out of 13 components, about 80% of the
#     variance is accounted for.

# Scree plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 60), ncp = 13)


# ------------------------------------------------------------------------------

## 1a. Do a scatterplot matrix of these 5PCs and use color to display the class
#      associated with each wine (Do you see separation of the 3 cultivars?)

# principal component scores
score_pca <- pca_result$x
# select only the first 5PC 
selected_scores_pca <- score_pca[, 1:5]

# merge the scores with cultivars column
cultivars_column <- winedata$cultivars
merged_data_pca <- cbind(selected_scores_pca, Cultivars = cultivars_column)
merged_data_pca <- data.frame(merged_data_pca)


## SCATTERPLOT

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr")
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits = 2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Upper panel
upper.panel_pca <- function(x, y) {
  points(x, y, pch = 19, col = my_cols_pca)
  r <- round(cor(x, y), digits = 2)
  txt <- paste0("R = ", r)
  usr <- par("usr")
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}

# define the colors
my_cols_pca <- c("red", "green3", "blue")[merged_data_pca$Cultivars]

# plot
pairs(merged_data_pca[, 1:5], lower.panel = NULL, upper.panel = upper.panel_pca)

## --> by observing all the plots, it seems that the combination of PC1 vs PC2 
#      provides the best separations among the three wine cultivars.


# ------------------------------------------------------------------------------

## 1b. interpret the first 2PCs

### INTERPRETATION PCA ----------------------------------------------------------


## the interpretation is based on the loadings. 
# 1. ignore loadings close to 0 
# 2. look at the positive/negative loadings and try to attach a label (what define
#    a large or small unit?)

# Extract the loadings (i.e., the Eigen vectors)
loadings_5 <- pca_result$rotation[, 1:5]
# - 1PC: we can see that the highest values (in absolute value) refers to the variables
#        Flavanoids, OD280/OD315 of diluted wines and Total phenols.
# - 2PC: we can see that the highest values refers to the variables Alcohol, 
#        Color intensity and Proline.


# Biplot
fviz_pca_biplot(pca_result, axes = c(1, 2), label = "var", invisible = "ind")


# ---------------------------------------------------------------------------------

## 2. PERFORM A FASTICA ON THE DATA

## 2a. Ask for 5 independent components

# Set the seed for reproducibility
set.seed(123)

# Perform ICA on the 'wine_s' dataset
ica_result <- fastICA(wine_s, n.comp = 5, method = "R", row.norm = FALSE) 


# ------------------------------------------------------------------------------

## 2b. Create a scatterplot matrix with color to display the class associated with 
#      each wine (Do you see separation of the 3 cultivars?)

# Extract the independent component scores
score_ica <- ica_result$S

# Merge the scores with the 'cultivars' column
merged_data_ica <- cbind(score_ica, Cultivars = cultivars_column)

# Convert the result to a data frame
merged_data_ica <- data.frame(merged_data_ica)


## SCATTERPLOT

# upper panel
upper.panel_ica <- function(x, y) {
  points(x, y, pch = 19, col = my_cols_ica)
  r <- round(cor(x, y), digits = 2)
  txt <- paste0("R = ", r)
  usr <- par("usr")
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}

# Define the colors
my_cols_ica <- c("red", "green3", "blue")[merged_data_ica$Cultivars]

# plot
pairs(merged_data_ica[, 1:5], lower.panel = NULL, upper.panel = upper.panel_ica)

## --> From this graph we can see that:
#      - R=0 in each plot suggesting that the components are statistically independent, 
#        which is expected since ICA aims to find components that are mutually independent.

#      - By observing all the plots, it seems that the combination of V1 vs V2 
#        provides the best separations among the three wine cultivars.

# -------------------------------------------------------------------------------------------


