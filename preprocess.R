# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# preprocess.R - Run this script to preprocess both datasets after exploration
# The script will:
#    - Assign the proper data types to the variables
#    - Clean the data
#    - Reorder the categorical variables by their importance according to the chi-squared feature selection algorithm
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# THIS SCRIPT IS RUN IN STANDALONE MODE TO PREPROCESS THE TRAINING AND TEST DATSETS
# PROVIDED BY KAGGLE. BOTH DATASETS ARE SAVED AT THE END OF THE PREPROCESSING.

# --------------------------------------------

source("data_processor2.R")
require(yaml)
conf = yaml.load_file("project.conf")

# preprocess the train data
loginfo(paste("Preprocess training dataset"))
dataDir = conf$general$data_directory
fnOriginalTrain = file.path(dataDir, conf$input$whole_train_data)
originalTrainData = load_data(fnOriginalTrain)
originalTrainData = data_preprocessing(originalTrainData)

# get the Chi-Squared importance of the categorical variables. Train with 30% of the variables due to 
# memory limitations
chiSquaredSortedFeatures = apply_chi_square_feature_selection(originalTrainData, trainingFraction=0.3)
numericFeatures = get_numeric_features(originalTrainData)

# reordered the originalTrainData
originalTrainData = as.data.table(cbind(QuoteConversion_Flag=originalTrainData$QuoteConversion_Flag, 
                                        originalTrainData[,numericFeatures, with=FALSE],
                                        originalTrainData[,row.names(chiSquaredSortedFeatures), with=FALSE]))

# preprocess the test data:
fnTestData = file.path(dataDir, conf$input$whole_test_data)  
originalTestData = load_data(fnTestData)
originalTestData = assignDataTypes(originalTestData)
originalTestData = transformAndClean(originalTestData)
numericFeatures = get_numeric_features(originalTestData)
originalTestData = as.data.table(cbind(originalTestData[,numericFeatures, with=FALSE],
                                 originalTestData[,row.names(chiSquaredSortedFeatures), with=FALSE]))

# save the data
save(originalTrainData, file=file.path(dataDir, conf$input$preprocessed_train))  
save(originalTestData, file=file.path(dataDir, conf$input$preprocessed_test))  

