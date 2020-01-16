#=======================================================================================================================================
# Class: CIS-544 DATA MINING & MACHINE LRNG
# Discussion & Exercises: Naive Bayes
# Max Franke
# 01/15/20
#=======================================================================================================================================

#---------------------------------------------------------------------------------------------------------------------------------------
# Workspace preparation
#---------------------------------------------------------------------------------------------------------------------------------------

# --- Clean workspace ---
rm(list = ls())

# --- Install packages ---
# install.packages()

# --- Load packages ---
library(e1071)
library(tidyverse)
library(caret)
library(naivebayes)

#---------------------------------------------------------------------------------------------------------------------------------------
# Train and test data
#---------------------------------------------------------------------------------------------------------------------------------------

# Function to split into train and test with a share of 0.75
train_test <- function(x){
  # Set the random seed for repeatability
  set.seed(123)
  # Define size of share
  train_share <- 0.75
  # Splitting into training and test data set
  sample_size <- nrow(x)
  sample_train <- sample(x = 1:sample_size, size = floor(sample_size*train_share), 
                         replace = FALSE)
  sample_test <- setdiff(1:sample_size, sample_train)
  # Get name of x
  name <- deparse(substitute(x))
  # name for train dataframe
  train <- paste("train", sep = "")
  # name for test dataframe
  test <- paste("test", sep = "")
  # safe train dataset
  assign(train,x[sample_train,],envir = .GlobalEnv)
  # safe test dataset
  assign(test,x[sample_test,],envir = .GlobalEnv)
}

#---------------------------------------------------------------------------------------------------------------------------------------
# Naive Bayes function with e1071
#---------------------------------------------------------------------------------------------------------------------------------------

naiveBayesA <- function(train, test){
  # Save the name of the first column of train input
  first_column <- colnames(train[1])
  # Rename the first column as "dummy"
  names(train)[names(train) == first_column] <- "dummy"
  names(test)[names(test) == first_column] <- "dummy"
  # Train the model to classify on the first column of input train set
  nb_model <- naiveBayes(dummy ~ ., data = train)
  # Prediction
  pred_test_nb <- predict(nb_model, newdata = test, type = "class")
  # Confusion matrix with caret library
  confusion_matrix <- confusionMatrix(test$dummy, pred_test_nb, positive = "spam")
  # Results
  resultsA <- data.frame(Actual = test$dummy,
                         Prediction = pred_test_nb)
  # safe results
  assign("resultsA",resultsA,envir = .GlobalEnv)
  # Accuarcy
  Accuracy <- confusion_matrix$overall[1]
  # Return
  return(Accuracy)
}

#---------------------------------------------------------------------------------------------------------------------------------------
# Naive Bayes function with e1071
#---------------------------------------------------------------------------------------------------------------------------------------

naiveBayesB <- function(train, test){
  # Save the name of the first column of train input
  first_column <- colnames(train[1])
  # Rename the first column as "dummy"
  names(train)[names(train) == first_column] <- "dummy"
  names(test)[names(test) == first_column] <- "dummy"
  # Train the model to classify on the first column of input train set
  nb_model <- naive_bayes(dummy ~ ., data = train)
  # Prediction
  pred_test_nb <- predict(nb_model, newdata = test, type = "class")
  # Confusion matrix with caret library
  confusion_matrix <- confusionMatrix(test$dummy, pred_test_nb, positive = "spam")
  # Results
  resultsB <- data.frame(Actual = test$dummy,
                         Prediction = pred_test_nb)
  # safe results
  assign("resultsB",resultsB,envir = .GlobalEnv)
  # Accuarcy
  Accuracy <- confusion_matrix$overall[1]
  # Return
  return(Accuracy)
}

#---------------------------------------------------------------------------------------------------------------------------------------
# Use iris, diamonds, and mtcars for testing of sig. difference between the two functions
#---------------------------------------------------------------------------------------------------------------------------------------

# Iris
#---------------------------------------------------------------------------------------------------------------------------------------

# Load dataframe
iris <- data.frame(iris)

# Define Species as first column
iris <- iris %>%
  select(Species, everything())

# Create train and test data for iris
train_test(iris)

# Import iris in function A
acc_iris_A <- naiveBayesA(train, test)
# Safe results
resultsA_iris <- resultsA

# Import iris in function B
acc_iris_B <- naiveBayesB(train, test)
# Safe results
resultsB_iris <- resultsB

# Diamonds
#---------------------------------------------------------------------------------------------------------------------------------------

# Load dataframe
diamonds <- data.frame(diamonds)

# Define cut as first column
diamonds <- diamonds %>%
  select(cut, everything())

# Create train and test data for diamonds
train_test(diamonds)

# Import diamonds in function A
acc_diamonds_A <- naiveBayesA(train, test)
# Safe results
resultsA_diamonds <- resultsA

# Import diamonds in function B
acc_diamonds_B <- naiveBayesB(train, test)
# Safe results
resultsB_diamonds <- resultsB


# Mtcars
#---------------------------------------------------------------------------------------------------------------------------------------

# Load dataframe
mtcars <- data.frame(mtcars)

# Classify a car's MPG as Excellent, Good, Fair, or Poor
first_qu <- summary(mtcars$mpg)[2]
Median <- summary(mtcars$mpg)[3]
third_qu <- summary(mtcars$mpg)[5]
mtcars$mpg <- if_else(mtcars$mpg < first_qu, "Poor",
                      if_else(mtcars$mpg < Median, "Fair",
                              if_else(mtcars$mpg < third_qu, "Good",
                                      if_else(mtcars$mpg >= first_qu, "Excellent","NA"))))
# Safe as factor
mtcars_factor <- factor(mtcars$mpg, ordered = TRUE,
                        levels = c("Poor", "Fair", "Good", "Excellent"))
mtcars$mpg <- mtcars_factor

# Create train and test data for mtcars
train_test(mtcars)

# Import mtcars in function A
acc_mtcars_A <- naiveBayesA(train, test)
# Safe results
resultsA_mtcars <- resultsA

# Import mtcars in function B
acc_mtcars_B <- naiveBayesB(train, test)
# Safe results
resultsB_mtcars <- resultsB



