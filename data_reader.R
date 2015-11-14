# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# data_reader.R - Main Program Entry Point
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# Notes:
# This code is designed to run in batch mode with RScript.exe
# Example invocation from a Windows command prompt:
# "C:\Program Files\R\R-3.1.3\bin\x64\Rscript.exe" data_reader.R
#
# Output can be redirected to a file with the > operator, such as 
# "C:\Program Files\R\R-3.1.3\bin\x64\Rscript.exe" data_reader.R > results.txt
# --------------------------------------------

# --------------------------------------------
# User configurable parameters:
program_name <- "Homesite Kaggle" 
working_directory <- "C:/Users/marmo/Documents/RProjects/HomesiteKaggle" 
output_logs_to_console <- FALSE 
log_file <- "HomesiteKaggle.log" 
DEBUG=FALSE 

# --------------------------------------------
# Environment variables:
interactive_session <- interactive()

# --------------------------------------------
# Load required source files:
source("initializer.R")
source("data_explorer.R")

# --------------------------------------------
# Data loading functions
load_data <- function(){
  homesite <- read.csv("train.csv", stringsAsFactors = TRUE)
  homesite
}

# --------------------------------------------
# Data preprocessing functions
data_preprocessing <- function(homesite){
  homesite$Original_Quote_Date_Typed <- as.Date(as.character(homesite$Original_Quote_Date, format = "%Y/%m%/%d"))
  homesite$Original_Quote_Date_Day <- as.numeric(format(homesite$Original_Quote_Date_Typed,format="%d"))
  homesite$Original_Quote_Date_Month <- as.numeric(format(homesite$Original_Quote_Date_Typed,format="%m"))
  homesite$Original_Quote_Date_Year <- as.numeric(format(homesite$Original_Quote_Date_Typed,format="%Y"))
  
  #homesite$Field6_B <- 
  
  
  homesite
}

binaryze_factor_columns <- function(homesite){
  factor_columns <- which(sapply(homesite,is.factor))
  for (column in factor_columns)
  {
    print(names(homesite[column]))
  }
  
}

main <- function(){

  initialize_program(program_name, working_directory, log_file, output_logs_to_console, interactive_session)
  homesite <- load_data()
  homesite <- data_preprocessing(homesite)
  numeric_columns <- which(sapply(homesite,is.numeric))
  univariate_numerical_exploration(homesite, numeric_columns, "Homesite")
  univariate_visual_exploration(homesite, numeric_columns, "Homesite")

  
  
}

main()

