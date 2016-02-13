# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# Variable Importance GBTree - Use the caret package to identify the most important variables 
#                               for the optimized xgboost algorithm for Gradient Boosted trees
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 

# THIS SCRIPT IS RUN IN STANDALONE MODE TO OPTIMIZE A GBT MODEL USING THE CARET
# R PACKAGE
# --------------------------------------------
require(caret) # trainControl, train
require(yaml)
require(ROCR) # prediction, performance
source("initializer.R")
source("utility.R")

conf = yaml.load_file("project.conf")
standardInit()

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
    return(perf@y.values[[1]])
    # print(perf)
#     cutoff = 0.5
#     cutoffIndex = which(abs(perf@x.values[[1]] - cutoff) < 0.01)
#     f = perf@y.values[[1]][cutoffIndex]
#     approxF = mean(f) # use the mean, in case there are more than 0 values
#     return(approxF) # value of f
}


dataDir = conf$general$data_directory
load(file.path(dataDir, conf$input$fn_reduced_training)) # loads modelTrainData
load(file.path(dataDir, conf$input$fn_reduced_testing)) # loads modelTestData


# Change levels of the factor  QuoteConversion_Flag (caret complaints if the labels are 0 1)
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "0"] = "no"
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "1"] = "yes"


#Configure grid with optimal parameters
parametersGrid <- expand.grid(max_depth = c(1, 2), nrounds = c(500, 550), eta = c(0.1, 0.2, 0.25)) #Optimal parameters


ctrl = trainControl(method = "cv",number=5,
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary

)

fittedModelResults = train(form = QuoteConversion_Flag ~ ., 
                           data = modelTrainData, 
                           method = "xgbTree",
                           trControl = ctrl,
                           metric = "ROC",
                           tuneGrid = parametersGrid)

fittedModelResults = train(form = QuoteConversion_Flag ~ 
                               PropertyField37+
                               SalesField5+
                               PersonalField12+
                               Field7+
                               PersonalField1+
                               PersonalField2+
                               PersonalField12+
                               Comp.24+
                               Comp.22+
                               PersonalField13+
                               PersonalField11+
                               Comp.2+
                               SalesField1A+
                               Comp.11+
                               GeographicField33B+
                               Comp.1+
                               Comp.7+
                               Comp.26+
                               PersonalField83+
                               CoverageField11A,
                           data = modelTrainData, 
                           method = "xgbTree",
                           trControl = ctrl,
                           metric = "ROC",
                           tuneGrid = parametersGrid)


xgbTreeVarImp <- varImp(fittedModelResults)
plot(xgbTreeVarImp, top = 20)

fMeasure = evaluateCaret(fittedModelResults$finalModel, modelTestData)


