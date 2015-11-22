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
source("data_preprocessor.R")
source("factor_analyzer.R")
source("utility.R")
source("feature_constructor.R")
require(data.table)


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

    # numeric_columns <- which(sapply(homesite,is.numeric))
    # univariate_numerical_exploration(homesite, numeric_columns, "Homesite")
    # univariate_visual_exploration(homesite, numeric_columns, "Homesite")
    # bivariate_numerical_exploration(homesite[,numeric_columns], "Homesite")

    # Explore test data
#     fnTest = file.path(conf$general$data_directory, "test.csv")
#     testData = load_data(fnTest)
#     testData[,QuoteNumber:=NULL]

#     columnIndices = which(sapply(testData, is.integer))
#     integerColumnNames = names(testData)[columnIndices]  
    # test_as_factors(testData, integerColumnNames, "Test Data")
    # compare_test_vs_train_factors(homesite, testData)    
    #factor_analysis(homesite)
    #new_homesite <- append_reduced_numeric_features(homesite, 42)
    reduced_homesite <- create_reduced_dataset(homesite, 
                                               conf$dimension_reduction$n_numeric_features_to_keep, 
                                               conf$dimension_reduction$n_categorical_features_to_keep)
    dataDir = conf$general$data_directory
    
    source("utility.R")
    splitDataset(reduced_homesite,
                 conf$dataset_splitting$train_fraction, 
                 conf$dataset_splitting$test_fraction, 
                 file.path(dataDir, conf$input$fn_reduced_training),
                 file.path(dataDir, conf$input$fn_reduced_testing),
                 file.path(dataDir, conf$input$fn_reduced_cross_val),
                 writeToRData=TRUE)
    
    #Not returning new_homesite for now
    return(homesite)
}
homesite = main()

