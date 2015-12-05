# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# optimize gbtree - Use the caret package to optimize the the parameters of the xgboost 
# algorithm for Gradient Boosted trees
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------
require(caret) # trainControl, train
require(yaml)
require(ROCR) # prediction, performance
source("initializer.R")
source("utility.R")

conf = yaml.load_file("project.conf")
standardInit()

dataDir = conf$general$data_directory
load(file.path(dataDir, conf$input$fn_reduced_training)) # loads modelTrainData
load(file.path(dataDir, conf$input$fn_reduced_testing)) # loads modelTestData
# modelTrainData = modelTrainData[1:1000,]

# Change levels of the factor  QuoteConversion_Flag (caret complaints if the labels are 0 1)
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "0"] = "no"
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "1"] = "yes"


# Define the grid to search: nrounds, max_depth, eta
# parametersGrid = expand.grid(max_depth = c(2), nrounds = c(3), eta = c(0.3))
# parametersGrid = expand.grid(max_depth = c(1, 3, 5), nrounds = c(10,20,30), eta = c(0.3))
# parametersGrid = expand.grid(max_depth = c(5,8,11), nrounds = c(25,50,100), eta = c(0.3,0.5))
parametersGrid = expand.grid(max_depth = c(5), nrounds = c(150,200,250,500), eta = c(0.3,0.1))

ctrl = trainControl(method = "cv",number=5,
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary
                    )
fittedModelResults = train(form = QuoteConversion_Flag ~ ., 
                           data = modelTrainData, 
                           method = "xgbTree",
                           trControl = ctrl,
                           metric = "ROC",
                           tuneGrid = parametersGrid
        )
fittedModelResults
plot(fittedModelResults, plotType="level")
save(fittedModelResults, file=file.path(dataDir, "BestGBTmodel.RData"))

fMeasure = evaluateCaret(fittedModelResults, modelTestData)

evaluateCaret = function(model, modelTestData) {
    ' Calculate the F-measure of performance for the classification model
    created with the glmnet package
    '
    loginfo(paste("Evaluating a dataset with",nrow(modelTestData), "datapoints"))
    modelPredictions = predict(model, newdata=modelTestData, type="prob")
    pred = prediction(modelPredictions$yes,modelTestData[,QuoteConversion_Flag])
    perf = performance(pred, measure = "tpr", x.measure="fpr")
    plot(perf)
    perf = performance(pred, measure = "f")
    # print(perf)
    cutoff = 0.5
    cutoffIndex = which(abs(perf@x.values[[1]] - cutoff) < 0.01)
    f = perf@y.values[[1]][cutoffIndex]
    approxF = mean(f) # use the mean, in case there are more than 0 values
    return(approxF) # value of f
}

