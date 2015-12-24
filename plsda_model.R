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
source("initializer.R")
conf = yaml.load_file("project.conf")
standardInit()

dataDir = conf$general$data_directory
load(file.path(dataDir, conf$input$fn_reduced_leveled_training)) # loads modelTrainData
load(file.path(dataDir, conf$input$fn_reduced_leveled_testing)) # loads modelTestData


# Change levels of the factor  QuoteConversion_Flag (caret complaints if the labels are 0 1)
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "0"] = "no"
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "1"] = "yes"


ctrl <- trainControl(method = "repeatedcv", 
                     repeats  = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

plsFit <- train(QuoteConversion_Flag ~ ., 
                data = modelTrainData[1:10000, ],
                method = "pls",
                tuneLength = 30,
                metric = "ROC",
                
                trControl = ctrl)

plsFit
#Saving plsFit
save(plsFit, file = "plsFit.RData")

