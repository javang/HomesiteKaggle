# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# PLSDA model - Evaluate a PLSDA model
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------

require(caret)
require(yaml)
require(pROC)
require(ROCR)
source("initializer.R")
source("utility.R")
conf = yaml.load_file("project.conf")
standardInit()

dataDir = conf$general$data_directory
load(file.path(dataDir, conf$input$fn_reduced_leveled_training)) # loads modelTrainData
load(file.path(dataDir, conf$input$fn_reduced_leveled_testing)) # loads modelTestData


# Change levels of the factor  QuoteConversion_Flag (caret complaints if the labels are 0 1)
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "0"] = "no"
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "1"] = "yes"
levels(modelTestData$QuoteConversion_Flag)[levels(modelTestData$QuoteConversion_Flag) == "0"] = "no"
levels(modelTestData$QuoteConversion_Flag)[levels(modelTestData$QuoteConversion_Flag) == "1"] = "yes"



ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats  = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary, 
                     verboseIter = TRUE)

trainIndices <- randomSelect(nrow(modelTrainData), 0.15)


plsFit <- train(QuoteConversion_Flag ~ ., 
                data = modelTrainData[trainIndices, ],
                #data = modelTrainData,
                method = "pls",
                tuneLength = 30,
                metric = "ROC",
                trControl = ctrl)

plsFit
plot(plsFit)
plot(plsFit, plotType = "level")
plsFit$finalModel

plsRoc <- roc(plsFit, modelTestData[1:100]$QuoteConversion_Flag)
#Saving plsFit
save(plsFit, file = "plsFit2.RData")

plsClasses <- predict(plsFit, newdata = modelTestData[1:100,-1, with = FALSE])
plsClasses
confusionMatrix(data = plsClasses, 
                reference = modelTestData[1:100]$QuoteConversion_Flag, 
                positive = "yes")

levels(plsClasses)[levels(plsClasses) == "no"] = "0"
levels(plsClasses)[levels(plsClasses) == "yes"] = "1"
levels(modelTestData$QuoteConversion_Flag)[levels(modelTestData$QuoteConversion_Flag) == "no"] = "0"
levels(modelTestData$QuoteConversion_Flag)[levels(modelTestData$QuoteConversion_Flag) == "yes"] = "1"

pred = prediction(as.numeric(plsClasses),as.numeric(modelTestData[1:100]$QuoteConversion_Flag))
perf = performance(pred, measure = "f")
perf = performance(pred, measure = "auc")

>>>>>>> e96b4396a4b7ac480e45728be93526293ab84919
