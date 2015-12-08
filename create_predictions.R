# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# Create predictions for the test set provided by Kaggle
#
# THIS FILE IS RUN AS A STANDALONE SCRIPT THAT WILL CREATE PREDICITON TO
# SUBMIT TO KAGGLE.
# --------------------------------------------
source("initializer.R")
source("utility.R")
source("data_processor2.R")
source("feature_constructor.R")
source("predictions_common.R")
require(yaml)
conf = yaml.load_file("project.conf")

createReducedTestSet = function() {
    ' Reads the Homesite test dataset provided by Kaggle and:
        1) Applies to it the same transformations/preprocess used for the train dataset. 
        2) Uses the PCA eigenvectors created from the training set and 
        applies them to the test dataset
        3) Reads the categorical features used for training and extracts them
        from the test dataset.
        4) Writes the transformed test dataset to file. 

    The transformed test dataset can be fed directly into the models created
    to get the predictions
    '
    dataDir = conf$general$data_directory
    fnTestData = file.path(dataDir, conf$input$whole_test_data)  
    homesiteTestData = load_data(fnTestData)
    
    # Preprocess and transform test data to put it in the format required for the model
    homesiteTestData = assignDataTypes(homesiteTestData)
    homesiteTestData = transformAndClean(homesiteTestData)
    fnPcaEigenvectors = file.path(dataDir, conf$output$pca_eigenvectors)  
    pcaEigenvectors = read.csv(fnPcaEigenvectors)
    pcaEigenvectors$X = NULL # the first column of pcaEigenvectors is the names of the variables. Remove it to do the dimensionality reduction
    numPcaDimensions = conf$dimension_reduction$n_numeric_features_to_keep
    pcaCoordinates = pca_dimension_reduction(homesiteTestData, pcaEigenvectors, numPcaDimensions)
    
    # Extract the names of the categorical features used from the training dataset. Not very elegant. IMPROVE THIS
    dataDir = conf$general$data_directory
    #Reading feature names from file, the CSV does not have the label
    #The following code is no longer required--->
    #load(file.path(dataDir, conf$input$fn_reduced_training)) # loads modelTrainData
    #featureNames = names(modelTrainData)
    #featureNames = featureNames[2:length(featureNames)] # Remove first name is the target feature
    #<---- End unrequired code
    fnFeatureNames = file.path(dataDir, conf$input$feature_names)
    featureNames = read.csv(fnFeatureNames, stringsAsFactors = FALSE)
    featureNames = featureNames[,1]
    categoricalFeatureNames = featureNames[(numPcaDimensions+1):length(featureNames)] # remove the PCA dimensions
    # Create the transformed test dataset and save it as a R object
    homesiteTestData = cbind(pcaCoordinates, 
                             homesiteTestData[,categoricalFeatureNames, with=FALSE])
    save(homesiteTestData, file=file.path(dataDir, conf$input$fn_reduced_test_data))  
}

standardInit()
# createReducedTestSet() # run this once

load(file.path(dataDir, conf$input$fn_reduced_test_data)) # loads homesiteTestData

# create predictions
modelPredictions = predict(fittedModelResults, newdata=homesiteTestData, type="prob")
fnTestData = file.path(dataDir, conf$input$whole_test_data)  
df = load_data(fnTestData)
quotenums = df[,QuoteNumber ]
preds = data.frame(QuoteNumber=quotenums, QuoteConversion_Flag=modelPredictions$yes)
fnPredictions = file.path(dataDir, conf$output$fn_output_predictions)  
write.csv(preds,file=fnPredictions,row.names = FALSE)

