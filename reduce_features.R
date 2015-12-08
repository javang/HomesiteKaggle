# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# reduce_features.R - Run this script on the pre-processed datasets to do feature selection.
#    - Create a PCA decomposition of the numeric variables
#    - Select a certain number of categorical values.
#    - NOTE: the script assumes that the preprocessed data has sorted the categorical
#    values according to their chi-squared importance
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------
source("feature_constructor.R")
source("utility.R")
require(yaml)
conf = yaml.load_file("project.conf")

loginfo(paste("Applying feature reduction/selection on the preprocessed datasets"))

# loads originalTrainData and original TestData
dataDir = conf$general$data_directory
load(file.path(dataDir, conf$input$preprocessed_train)) 
load(file.path(dataDir, conf$input$preprocessed_test)) 

# Apply PCA dimensionality reduction of the numeric features
pca_result <- pca_factor_analysis(originalTrainData)
eigenvectors <- pca_result$loadings
write.csv(eigenvectors, file.path(dataDir, "PCAloadings.txt"))
pca_reduced_features <- pca_dimension_reduction(originalTrainData, eigenvectors, 
                                        conf$dimension_reduction$n_numeric_features_to_keep)

# Select categorical values
factorFeatureNames = get_factor_features(originalTrainData)
factorFeatureNames = factorFeatureNames[2:length(factorFeatureNames)] # first feature is the target value

# Create the final reduced training dataset
reducedTrainData = as.data.table(cbind(QuoteConversion_Flag=originalTrainData$QuoteConversion_Flag,
                                       pca_reduced_features,
                                       originalTrainData[,factorFeatureNames, with=FALSE]))

# create the reduce test data set
reducedPcaFeaturesTest = pca_dimension_reduction(originalTestData, eigenvectors, 
                                    conf$dimension_reduction$n_numeric_features_to_keep)
factorFeatureNames = get_factor_features(originalTestData)
reducedTestData = as.data.table(cbind(reducedPcaFeaturesTest,
                                      originalTestData[,factorFeatureNames, with=FALSE]))

# save the data
save(reducedTrainData, file=file.path(dataDir, conf$input$whole_train_data_reduced))  
save(reducedTestData, file=file.path(dataDir, conf$input$whole_test_data_reduced))  

# created the splitted datasets for training and testing 
splitDataset(reducedTrainData,
             conf$dataset_splitting$train_fraction, 
             conf$dataset_splitting$test_fraction, 
             file.path(dataDir, conf$input$fn_reduced_training),
             file.path(dataDir, conf$input$fn_reduced_testing),
             file.path(dataDir, conf$input$fn_reduced_cross_val),
             writeToRData=TRUE)
