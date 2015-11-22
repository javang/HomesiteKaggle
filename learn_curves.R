# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# learning_curves - Create learning curves for a model
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# Notes: 
# TODO: Persist eigenvalues and read them from file instead of calling PCA each time.
# --------------------------------------------

source("utility.R")
source("data_preprocessor.R")
require(randomForest)
require(ROCR) # prediction, performance
require(glmnet)
require(yaml)
conf = yaml.load_file("project.conf")


trainModel = function(trainData) {
    ' Train a random forest
    '
    formula = QuoteConversion_Flag ~ Field6 + Field7 + Field12
    model <- randomForest(formula, data=trainData, importance=TRUE,
                          proximity=TRUE, ntree=100, mtry=2)    
    return(model)
}

trainLogisticRegression = function(modelTrainData) {
    ' Train a lasso/ridge regression with the model data. First remove the 
    target variable QuoteConversion_Flag when building the model model.matrix
    '
    indices = get_factor_features(modelTrainData)
    featureNames = names(modelTrainData)[indices]
    remove = c( "QuoteConversion_Flag", "PropertyField6", "GeographicField10A") # QuoteConversion_Flag is the variable to predict, the others are NOT NEEDED FOR FINAL PREPARED INPUT
    featureNames = featureNames[! featureNames %in% remove]# NOT NEEDED FOR FINAL PREPARED INPUT
    
    formula = as.formula(paste("~ ",paste(featureNames, collapse="+"),sep = ""))
    options(na.action="na.fail")
    modelMatrix = model.matrix(formula, data=modelTrainData)
    model = glmnet(modelMatrix, modelTrainData[,QuoteConversion_Flag],
                   family="binomial", alpha=0.5, nlambda=100)
    return(model)
}

evaluateModel = function(model, testData) {
    modelPredictions = predict(model, testData)
    pred = prediction(as.integer(modelPredictions), 
                      as.integer(testData[,QuoteConversion_Flag]))
    perf = performance(pred, measure = "f")
    # print(perf)
    return(perf@y.values[[1]][2]) # value of f
}

createLearningCurves = function() {
    dataDir = conf$general$data_directory
    modelTrainData = load_data(file.path(dataDir, "reduced_training_labeled", "reduced_training.csv"))
    assignDataTypes(modelTrainData)
    modelTestData = load_data(file.path(dataDir, conf$input$fn_model_testing))
    assignDataTypes(modelTestData)
    
    # Remove useless values (NOT NEEDED WHEN WE PREPARE THE FINAL VALUES)
    modelTrainData[,PropertyField6:=NULL]
    modelTrainData[, GeographicField10A:=NULL]
    # Impute
    m = Mode(modelTrainData$PersonalField84) # impute the mode 
    modelTrainData[is.na(PersonalField84), PersonalField84:=m] 
    m = Mode(modelTrainData$PropertyField29) # impute the mode 
    modelTrainData[is.na(PropertyField29), PropertyField29:=m] 
    
    # modelCrossValidation = load_data(file.path(dataDir, conf$input$fn_model_cross_val))
    dataPointsFractions = seq(0.01,0.05,0.01)
    nrows = nrow(modelTrainData)
    numCurvePoints = length(dataPointsFractions)
    trainFs = rep(0, numCurvePoints)
    testFs = rep(0, numCurvePoints)
    for (i in c(1:numCurvePoints)) {
        indices = randomSelect(nrows, dataPointsFractions[i])
        # model = trainModel(modelTrainData[indices,])
        model = trainLogisticRegression(modelTrainData)
        
        trainF = evaluateModel(model, modelTrainData[indices,]) # evaluate the model on the data used to create
        trainFs[i] = trainF
        testF = evaluateModel(model, modelTestData) # evalue on the test data
        testFs[i] = testF
    }
    plot(dataPointsFractions, testFs)
}
