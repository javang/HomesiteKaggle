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
  #SALES FACTORS
  homesite[,SalesField3:= as.factor(SalesField3)]
  homesite[,SalesField4:= as.factor(SalesField4)]
  homesite[,SalesField5:= as.factor(SalesField5)]
  homesite[,SalesField6:= as.factor(SalesField6)]
  homesite[,SalesField9:= as.factor(SalesField9)]
  homesite[,SalesField10:= as.factor(SalesField10)]
  homesite[,SalesField11:= as.factor(SalesField11)]
  homesite[,SalesField12:= as.factor(SalesField12)]
  homesite[,SalesField13:= as.factor(SalesField13)]
  homesite[,SalesField14:= as.factor(SalesField14)]
  homesite[,SalesField15:= as.factor(SalesField15)]
  
  #PERSONAL FACTORS
  homesite[,PersonalField1:= as.factor(PersonalField1)]
  homesite[,PersonalField2:= as.factor(PersonalField2)]
  homesite[,PersonalField5:= as.factor(PersonalField5)]
  homesite[,PersonalField6:= as.factor(PersonalField6)]
  homesite[,PersonalField8:= as.factor(PersonalField8)]
  homesite[,PersonalField9:= as.factor(PersonalField9)]
  homesite[,PersonalField11:= as.factor(PersonalField11)]
  homesite[,PersonalField12:= as.factor(PersonalField12)]
  homesite[,PersonalField13:= as.factor(PersonalField13)]
  homesite[,PersonalField22:= as.factor(PersonalField22)]
  homesite[,PersonalField28:= as.factor(PersonalField28)]
  homesite[,PersonalField29:= as.factor(PersonalField29)]
  homesite[,PersonalField34:= as.factor(PersonalField34)]
  homesite[,PersonalField35:= as.factor(PersonalField35)]
  homesite[,PersonalField36:= as.factor(PersonalField36)]
  homesite[,PersonalField37:= as.factor(PersonalField37)]
  homesite[,PersonalField38:= as.factor(PersonalField38)]
  homesite[,PersonalField39:= as.factor(PersonalField39)]
  homesite[,PersonalField40:= as.factor(PersonalField40)]
  homesite[,PersonalField41:= as.factor(PersonalField41)]
  homesite[,PersonalField42:= as.factor(PersonalField42)]
  homesite[,PersonalField43:= as.factor(PersonalField43)]
  homesite[,PersonalField48:= as.factor(PersonalField48)]
  homesite[,PersonalField49:= as.factor(PersonalField49)]
  homesite[,PersonalField50:= as.factor(PersonalField50)]
  homesite[,PersonalField51:= as.factor(PersonalField51)]
  homesite[,PersonalField52:= as.factor(PersonalField52)]
  homesite[,PersonalField53:= as.factor(PersonalField53)]
  homesite[,PersonalField58:= as.factor(PersonalField58)]
  homesite[,PersonalField59:= as.factor(PersonalField59)]
  homesite[,PersonalField60:= as.factor(PersonalField60)]
  homesite[,PersonalField61:= as.factor(PersonalField61)]
  homesite[,PersonalField62:= as.factor(PersonalField62)]
  homesite[,PersonalField63:= as.factor(PersonalField63)]
  homesite[,PersonalField64:= as.factor(PersonalField64)]
  homesite[,PersonalField65:= as.factor(PersonalField65)]
  homesite[,PersonalField66:= as.factor(PersonalField66)]
  homesite[,PersonalField67:= as.factor(PersonalField67)]
  homesite[,PersonalField68:= as.factor(PersonalField68)]
  homesite[,PersonalField69:= as.factor(PersonalField69)]
  homesite[,PersonalField70:= as.factor(PersonalField70)]
  homesite[,PersonalField71:= as.factor(PersonalField71)]
  homesite[,PersonalField72:= as.factor(PersonalField72)]
  homesite[,PersonalField73:= as.factor(PersonalField73)]
  homesite[,PersonalField78:= as.factor(PersonalField78)]
  homesite[,PersonalField83:= as.factor(PersonalField83)]

  #GEOGRAPHIC FACTORS
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

