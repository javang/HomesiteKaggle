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
source("learn_curves.R")
conf = yaml.load_file("project.conf")
standardInit()
interactive_session <- interactive()

dataDir <- conf$general$data_directory
load(file.path(dataDir, conf$input$fn_reduced_leveled_training)) # loads modelTrainData
load(file.path(dataDir, conf$input$fn_reduced_leveled_testing)) # loads modelTestData


# Change levels of the factor  QuoteConversion_Flag (caret complaints if the labels are 0 1)
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "0"] = "no"
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "1"] = "yes"
levels(modelTestData$QuoteConversion_Flag)[levels(modelTestData$QuoteConversion_Flag) == "0"] = "no"
levels(modelTestData$QuoteConversion_Flag)[levels(modelTestData$QuoteConversion_Flag) == "1"] = "yes"


inTrain <- createDataPartition(y = modelTrainData$QuoteConversion_Flag, p = trainPartitionPercent, list = FALSE)
x <- getModelMatrix(modelTrainData[as.vector(inTrain),])
y <- modelTrainData[as.vector(inTrain)]$QuoteConversion_Flag

load(file.path(conf$plsda$directory, "plsFit_0.35_3_30_0.7.RData"))
