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
source("initializer.R")
require(ROCR) # prediction, performance
require(glmnet)
require(ggplot2)
require(yaml)
# require(gbm)
require(xgboost) 
# To install it, install.packages("devtools"); 
# devtools::install_github('dmlc/xgboost', subdir='R-package', threads=8, auth_token="8faccc1d2a8af8d49c71cf5f164f1c42b9886749")
conf = yaml.load_file("project.conf")
standardInit()

trainLogisticRegression = function(modelTrainData, indices) {
    ' Train a lasso/ridge regression with the model data. First remove the 
    target variable QuoteConversion_Flag when building the model model.matrix
    '
    modelMatrix = getModelMatrix(modelTrainData[indices,])    
    model = cv.glmnet(modelMatrix, modelTrainData[indices,QuoteConversion_Flag],
                      family="binomial", alpha=0.5, nlambda=10, nfolds=3)
    return(model)
}

trainGradientBoostedTrees = function(modelTrainData, indices) {
    ' Train a gradient boosted tree algorithm'
    loginfo("Gradient boosted trees")
    # gbm does not work well. Always NaNs! Bug?
    # trainFormula = QuoteConversion_Flag ~ .
    #     model = gbm(formula = trainFormula, distribution = "adaboost", 
    #                 data=modelTrainData[indices,], n.trees=100,interaction.depth = 2,
    #                 shrinkage = 0.001,bag.fraction = 0.5,train.fraction = 1.0,cv.folds=0)
    designMatrix = getSparseModelMatrix(modelTrainData[indices,])
    outputVector = modelTrainData[indices,QuoteConversion_Flag] == 1
    model = xgboost(data = designMatrix, label = outputVector, max.depth = 6,
            eta = 1, nthread = 8, nround = 12,objective = "binary:logistic",
            verbose =2 )
    return(model)
}

getSparseModelMatrix = function(data) {
    designMatrix = sparse.model.matrix(QuoteConversion_Flag~.-1, data=data)
    return(designMatrix)
}

getModelMatrix = function(data) {
    featureNames = names(data)
    remove = c("QuoteConversion_Flag") # QuoteConversion_Flag is the target variable. Remove it to prepare model
    featureNames = featureNames[! featureNames %in% remove]
    formula = as.formula(paste("~ ",paste(featureNames, collapse="+"),sep = ""))
    options(na.action="na.fail")
    modelMatrix = model.matrix(formula, data=data)
    return(modelMatrix)
}

evaluateLogisticRegression = function(model, testData) {
    ' Calculate the F-measure of performance for the classification model
    created with the glmnet package
    '
    loginfo(paste("Evaluating a dataset with",nrow(testData), "datapoints"))
    modelMatrix = getModelMatrix(testData)
    modelPredictions = predict(model, modelMatrix, type="response", s= model$lambda.min)
    pred = prediction(modelPredictions,testData[,QuoteConversion_Flag])
    perf = performance(pred, measure = "f")
    # print(perf)
    cutoff = 0.5
    cutoffIndex = which(abs(perf@x.values[[1]] - cutoff) < 0.01)
    f = perf@y.values[[1]][cutoffIndex]
    approxF = mean(f) # use the mean, in case there are more than 0 values
    return(approxF) # value of f
}

evaluateGradientBoostedTrees = function(model, testData) {
    loginfo(paste("Evaluating a dataset with",nrow(testData), "datapoints"))
    designMatrix = getSparseModelMatrix(testData)
    modelPredictions = predict(model, designMatrix)
    pred = prediction(modelPredictions,testData[,QuoteConversion_Flag])
    perf = performance(pred, measure = "f")
    cutoff = 0.5
    cutoffIndex = which(abs(perf@x.values[[1]] - cutoff) < 0.01)
    f = perf@y.values[[1]][cutoffIndex]
    approxF = mean(f) # use the mean, in case there are more than 0 values
    return(approxF) # value of f
}

createLearningCurves = function() {
    dataDir = conf$general$data_directory
    load(file.path(dataDir, conf$input$fn_reduced_training)) # loads modelTrainData
    load(file.path(dataDir, conf$input$fn_reduced_testing)) # loads modelTestData
#     modelTrainData = selectFeatures(modelTrainData, 61) 
#     modelTestData = selectFeatures(modelTestData, 61)
    
    # dataPointsFractions = seq(0.01,0.05,0.01)
    dataPointsFractions = c(0.10, 0.15, 0.25, 0.50, 0.75, 1.0)
    nrows = nrow(modelTrainData)
    numCurvePoints = length(dataPointsFractions)
    trainFs = rep(0, numCurvePoints)
    testFs = rep(0, numCurvePoints)
    fnCurves = file.path(dataDir, "LearningCurves.XGB.155.Features.txt.2") 
    fileConn = file(fnCurves, "w")
    writeLines(c(paste("PointsFraction", "TrainFMeasure", "TestFMeasure",sep=",")), fileConn)
    close(fileConn)
    for (i in c(1:numCurvePoints)) {
        indices = randomSelect(nrows, dataPointsFractions[i])
        loginfo(paste("Creating a model using",length(indices), "datapoints"))
        # model = trainLogisticRegression(modelTrainData, indices)
        model = trainGradientBoostedTrees(modelTrainData, indices)
        # evaluate the model on the data used to create it
        # trainF = evaluateLogisticRegression(model, modelTrainData[indices,]) 
        trainF = evaluateGradientBoostedTrees(model, modelTrainData[indices,]) 
        trainFs[i] = trainF
        # testF = evaluateLogisticRegression(model, modelTestData) 
        testF = evaluateGradientBoostedTrees(model, modelTestData) 
        testFs[i] = testF
        fileConn = file(fnCurves, "at")
        writeLines(c(paste(dataPointsFractions[i], trainF, testF, sep=",")), fileConn)
        close(fileConn)    
    }
    df = data.frame("PointsFraction"=dataPointsFractions, "TrainFMeasure"=trainFs, "TestFMeasure"=testFs)
    ggplot(df) +
        ggtitle("Gradient Boosted Trees (xgboost), all features") +
        xlab("Fraction of training points") +
        ylab("F-measure") +
        geom_point(aes(x=PointsFraction, y=TrainFMeasure, color="Train")) +
        geom_line(aes(x=PointsFraction, y=TrainFMeasure, color="Train")) +
        geom_point(aes(x=PointsFraction, y=TestFMeasure, color="Test")) +
        geom_line(aes(x=PointsFraction, y=TestFMeasure, color="Test")) 
    }


fnData = file.path(dataDir, "LearningCurves.XGB.155.Features.txt") 
df = read.csv(fnData)
