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

load_data <- function(fnData) {
    ' Data loading function.
    '
    homesite = fread(fnData, sep=",", stringsAsFactors=TRUE)
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
                        fnTrain, fnTest, fnCrossValidation) {
    ' Splits a dataset for training into 3 datasets:
        - A model dataset. The data that is going to be used to create models
        - A test dataset. A dataset to evaluate models and optimize hyperparameters.
        - A cross-validation dataset. A dataset to evaluate how the model generalizes
    Each of the datasets is written to a different file.
    '
    splits = splitIndices(nrow(dataTable), trainFraction, testFraction)
    write.csv(dataTable[splits$train_ind,], fnTrain, row.names = FALSE)
    write.csv(dataTable[splits$test_ind,], fnTest, row.names = FALSE)
    write.csv(dataTable[splits$cv_ind,], fnCrossValidation, row.names = FALSE)
}


