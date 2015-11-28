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
require(gbm)
conf = yaml.load_file("project.conf")
standardInit()

trainModel = function(trainData) {
    ' Train a random forest
    '
    formula = QuoteConversion_Flag ~ Field6 + Field7 + Field12
    model <- randomForest(formula, data=trainData, importance=TRUE,
                          proximity=TRUE, ntree=100, mtry=2)    
    return(model)
}

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

    trainFormula = QuoteConversion_Flag ~ .
    #     model = gbm(formula = trainFormula, distribution = "adaboost", 
    #                 data=modelTrainData[indices,], n.trees=100,interaction.depth = 2,
    #                 shrinkage = 0.001,bag.fraction = 0.5,train.fraction = 1.0,cv.folds=0)
    
    mdt = modelTrainData[indices,]  
    model = gbm(formula = trainFormula, distribution = "adaboost", interaction.depth = 1,
                n.minobsinnode = 3,data=mdt[,c(1,1:9), with=FALSE], n.trees=100, n.cores=8)

    pr = predict(model, modelTrainData, type="response", n.trees=100)
    pr[0:1000]
        sum(pr > 0.5)
    return(model)
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

evaluateModel = function(model, testData) {
    modelPredictions = predict(model, testData)
    pred = prediction(as.integer(modelPredictions), 
                      as.integer(testData[,QuoteConversion_Flag]))
    perf = performance(pred, measure = "f")
    # print(perf)
    return(perf@y.values[[1]][2]) # value of f
}


selectFeatures = function(dataTable, nVariables) {
    return(dataTable[,1:nVariables, with=FALSE])
}

createLearningCurves = function() {
    dataDir = conf$general$data_directory
    load(file.path(dataDir, conf$input$fn_reduced_training)) # loads modelTrainData
    load(file.path(dataDir, conf$input$fn_reduced_testing)) # loads modelTestData
    modelTrainData = selectFeatures(modelTrainData, 41) 
    modelTestData = selectFeatures(modelTestData, 21)
    
    # dataPointsFractions = seq(0.01,0.05,0.01)
    dataPointsFractions = c(0.02, 0.05, 0.10, 0.15, 0.20)
    nrows = nrow(modelTrainData)
    numCurvePoints = length(dataPointsFractions)
    trainFs = rep(0, numCurvePoints)
    testFs = rep(0, numCurvePoints)
    fnCurves = file.path(dataDir, "LearningCurves.60.Features.txt") 
    fileConn = file(fnCurves, "w")
    writeLines(c(paste("PointsFraction", "TrainFMeasure", "TestFMeasure",sep=",")), fileConn)
    close(fileConn)
    for (i in c(1:numCurvePoints)) {
        indices = randomSelect(nrows, dataPointsFractions[i])
        loginfo(paste("Creating a model using",length(indices), "datapoints"))
        model = trainLogisticRegression(modelTrainData, indices)
        # evaluate the model on the data used to create it
        trainF = evaluateLogisticRegression(model, modelTrainData[indices,]) 
        trainFs[i] = trainF
        testF = evaluateLogisticRegression(model, modelTestData) 
        testFs[i] = testF
        fileConn = file(fnCurves, "at")
        writeLines(c(paste(dataPointsFractions[i], trainF, testF, sep=",")), fileConn)
        close(fileConn)    
    }
    df = data.frame("PointsFraction"=dataPointsFractions, "TrainFMeasure"=trainFs, "TestFMeasure"=testFs)
    ggplot(df) +
        ggtitle("Logistic-Regression, first 60 features") +
        xlab("Fraction of training points") +
        ylab("F-measure") +
        geom_point(aes(x=PointsFraction, y=TrainFMeasure, color="Train")) +
        geom_line(aes(x=PointsFraction, y=TrainFMeasure, color="Train")) +
        geom_point(aes(x=PointsFraction, y=TestFMeasure, color="Test")) +
        geom_line(aes(x=PointsFraction, y=TestFMeasure, color="Test")) 
    }
