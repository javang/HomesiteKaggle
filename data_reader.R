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

require(yaml)
conf = yaml.load_file("project.conf")
# Environment variables:
interactive_session <- interactive()

# --------------------------------------------
# Load required source files:
source("initializer.R")
source("data_explorer.R")
source("clean.R")
require(data.table)



# --------------------------------------------
# Data loading functions
load_data <- function(fnData) {
  homesite = fread(fnData, sep=",", stringsAsFactors=TRUE)
  return(homesite)
}

# --------------------------------------------
# Data preprocessing functions
data_preprocessing <- function(homesite){
  homesite$Original_Quote_Date_Typed <- as.Date(as.character(homesite$Original_Quote_Date, format = "%Y/%m%/%d"))
  homesite$Original_Quote_Date_Day <- as.numeric(format(homesite$Original_Quote_Date_Typed,format="%d"))
  homesite$Original_Quote_Date_Month <- as.numeric(format(homesite$Original_Quote_Date_Typed,format="%m"))
  homesite$Original_Quote_Date_Year <- as.numeric(format(homesite$Original_Quote_Date_Typed,format="%Y"))
  # Remove quote ID, it is an index variable
  homesite[,QuoteNumber:=NULL]
  
  # create factors
  homesite[,QuoteConversion_Flag:= as.factor(QuoteConversion_Flag)]
  homesite[,Field6:= as.factor(Field6)]
  homesite[,Field12:= as.factor(Field12)]
  homesite[,Field10:= as.numeric(gsub(",", "", Field10))]
  homesite[,CoverageField5A:= as.factor(CoverageField5A)]
  homesite[,CoverageField5B:= as.factor(CoverageField5B)]
  homesite[,CoverageField6A:= as.factor(CoverageField6A)]
  homesite[,CoverageField6B:= as.factor(CoverageField6B)]
  homesite[,CoverageField6B:= as.factor(CoverageField6B)]
  homesite[,GeographicField5A:= as.factor(GeographicField5A)]
  homesite[,GeographicField10A:= NULL]
  homesite[,GeographicField10B:= as.factor(GeographicField10B)]
  homesite[,GeographicField14A:= as.factor(GeographicField14A)]
  homesite[,GeographicField18A:= as.factor(GeographicField18A)]
  homesite[,GeographicField21A:= as.factor(GeographicField21A)]
  homesite[,GeographicField22A:= as.factor(GeographicField22A)]
  
  return(homesite)
}

binaryze_factor_columns <- function(homesite){
  factor_columns <- which(sapply(homesite,is.factor))
  for (column in factor_columns)
  {
    print(names(homesite[column]))
  }
}

main <- function(){
    program_name = conf$general$program_name
    working_directory = conf$general$data_directory
    log_file = conf$general$log_file
    output_logs_to_console = conf$general$output_logs_to_console
    DEBUG = conf$general$DEBUG
    initialize_program(program_name, working_directory, 
                       log_file, output_logs_to_console, interactive_session)
    
    fnData = file.path(conf$general$data_directory, "train.csv")
    homesite <- load_data(fnData)
    
    homesite <- data_preprocessing(homesite)
    numeric_columns <- which(sapply(homesite,is.numeric))

    univariate_numerical_exploration(homesite, numeric_columns, "Homesite")
    univariate_visual_exploration(homesite, numeric_columns, "Homesite")
    bivariate_numerical_exploration(homesite[,numeric_columns], "Homesite")
    
    clean_data(homesite)
    str(homesite)

}

main()

