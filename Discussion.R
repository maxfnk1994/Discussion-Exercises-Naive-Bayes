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
