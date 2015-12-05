# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# utility.R - Common functions
# --------------------------------------------
require(data.table)

load_data <- function(fnData, stringsAsFactors=FALSE) {
    ' Data loading function.
    '
    homesite = fread(fnData, sep=",", stringsAsFactors=TRUE) # fread ignores stringsAsFactors.
    if (stringsAsFactors == TRUE) {
            types <- data.frame(sapply(homesite, class))
            chrFeatures <- row.names(types)[types[[1]] == 'character'] 
            homesite[,(chrFeatures):=lapply(.SD, as.factor),.SDcols=chrFeatures]            
    }
    return(homesite)
}

randomSelect = function(numObservations, fraction) {
    ' Select a random fraction of indices
    '    
    x = runif(numObservations)
    return(which(x <= fraction))
}

splitIndices = function(numObservations, trainFraction, testFraction) {
    ' Select random fractions of indices.
    '
    x = runif(numObservations)
    trainIndices = (x < trainFraction)
    testIndices = (x >= trainFraction & x <(trainFraction + testFraction))
    cvIndices = (x >= (trainFraction + testFraction))
    return(list(train_ind=trainIndices, test_ind=testIndices, cv_ind=cvIndices))
}

splitDataset = function(dataTable, trainFraction, testFraction,
                        fnTrain, fnTest, fnCrossValidation,
                        writeToRData=FALSE) {
    ' Splits a dataset for training into 3 datasets:
        - A model dataset. The data that is going to be used to create models
        - A test dataset. A dataset to evaluate models and optimize hyperparameters.
        - A cross-validation dataset. A dataset to evaluate how the model generalizes
    Each of the datasets is written to a different file.
    '
    loginfo("Splitting dataset into train, test, and cross-validation")
    splits = splitIndices(nrow(dataTable), trainFraction, testFraction)
    if (writeToRData==TRUE) {
        loginfo("Saving datasets to RData files")
        modelTrainData = dataTable[splits$train_ind,]
        save(modelTrainData, file=fnTrain)
        modelTestData = dataTable[splits$test_ind,]
        save(modelTestData, file=fnTest)
        modelXvalidationData = dataTable[splits$cv_ind,]
        save(modelXvalidationData, file=fnCrossValidation)
    } else {
        loginfo("Saving datasets to csv files")
        write.csv(dataTable[splits$train_ind,], fnTrain, row.names = FALSE)
        write.csv(dataTable[splits$test_ind,], fnTest, row.names = FALSE)
        write.csv(dataTable[splits$cv_ind,], fnCrossValidation, row.names = FALSE)
    }
}

selectFeatures = function(dataTable, nVariables) {
    ' Select the first features from a data table
    '
    return(dataTable[,1:nVariables, with=FALSE])
}


get_numeric_features <- function(dataset){
    return(which(sapply(dataset,is.numeric)))
}

get_factor_features <- function(dataset){
    return(which(sapply(dataset,is.factor)))
}

get_character_features <- function(dataset){
    return(which(sapply(dataset,is.character)))
}

