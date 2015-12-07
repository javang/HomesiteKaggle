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

source("initializer.R")
source("data_explorer.R")
source("clean.R")
source("data_preprocessor.R") # data_preprocessing
source("utility.R")
source("feature_constructor.R") # create_reduced_dataset
require(data.table)
require(yaml)
conf = yaml.load_file("project.conf")
interactive_session <- interactive()


main <- function(){
    ' Read the original training dataset provided by Kaggle and 
    explore/preprocess it.
    '
    standardInit()
    dataDir = conf$general$data_directory
    fnData = file.path(dataDir, "train.csv")
    homesite <- load_data(fnData)

    # Data exploration    
    numeric_columns <- which(sapply(homesite,is.numeric))
    univariate_numerical_exploration(homesite, numeric_columns, "Homesite")
    univariate_visual_exploration(homesite, numeric_columns, "Homesite")
    bivariate_numerical_exploration(homesite[,numeric_columns], "Homesite")
    
    # Preprocessing    
    homesite <- data_preprocessing(homesite)

    # Load test data and compare it to the training data
    fnTest = file.path(conf$general$data_directory, "test.csv")
    testData = load_data(fnTest)
    columnIndices = which(sapply(testData, is.integer))
    integerColumnNames = names(testData)[columnIndices]  
    test_as_factors(testData, integerColumnNames, "Test Data")
    compare_test_vs_train_factors(homesite, testData)    
    factor_analysis(homesite)
    new_homesite <- append_reduced_numeric_features(homesite, 42)

    
    # Perform feature selection and dimensionality reduction
    reduced_homesite <- create_reduced_dataset(homesite, 
                       conf$dimension_reduction$n_numeric_features_to_keep, 
                       conf$dimension_reduction$n_categorical_features_to_keep)

    # Split Kaggle's reduced dataset into train, test and cross-validation sets
    splitDataset(reduced_homesite,
                 conf$dataset_splitting$train_fraction, 
                 conf$dataset_splitting$test_fraction, 
                 file.path(dataDir, conf$input$fn_reduced_training),
                 file.path(dataDir, conf$input$fn_reduced_testing),
                 file.path(dataDir, conf$input$fn_reduced_cross_val),
                 writeToRData=TRUE)
    
    return(homesite)
}

homesite = main()

