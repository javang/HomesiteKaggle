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

arguments <- commandArgs(trailingOnly = TRUE)

trainPartitionPercent = as.numeric(arguments[1]) #0.1
numResamples <- as.numeric(arguments[2])#3
tuneLength <- as.numeric(arguments[3])#30
testPartitionPercent <- as.numeric(arguments[4])#0.1

dataDir = conf$general$data_directory
load(file.path(dataDir, conf$input$fn_reduced_leveled_training)) # loads modelTrainData
load(file.path(dataDir, conf$input$fn_reduced_leveled_testing)) # loads modelTestData


# Change levels of the factor  QuoteConversion_Flag (caret complaints if the labels are 0 1)
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "0"] = "no"
levels(modelTrainData$QuoteConversion_Flag)[levels(modelTrainData$QuoteConversion_Flag) == "1"] = "yes"
levels(modelTestData$QuoteConversion_Flag)[levels(modelTestData$QuoteConversion_Flag) == "0"] = "no"
levels(modelTestData$QuoteConversion_Flag)[levels(modelTestData$QuoteConversion_Flag) == "1"] = "yes"


inTrain <- createDataPartition(y = modelTrainData$QuoteConversion_Flag, p = trainPartitionPercent, list = FALSE)
training <- modelTrainData[as.vector(inTrain),]

#trainIndicesList <- createResample(modelTrainData$QuoteConversion_Flag, times = 3)
#trainIndicesList <- createFolds(modelTrainData$QuoteConversion_Flag, k = 10, times = 3, returnTrain = TRUE)
#trainIndicesList <- createMultiFolds(modelTrainData$QuoteConversion_Flag, k = 10, times = 3)


ctrl <- trainControl(method = "boot",
                     number = numResamples,
                     #repeats  = 3,
                     index = createResample(training$QuoteConversion_Flag, numResamples),
                     savePredictions = TRUE,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary, 
                     verboseIter = TRUE,
                     returnData = FALSE)


plsFit <- train(#QuoteConversion_Flag ~ ., 
                #data = modelTrainData[trainIndices, ],
                #data = training,
		x = training[,-1, with = FALSE],
		y = training$QuoteConversion_Flag,
                method = "pls",
                tuneLength = tuneLength,
                metric = "ROC",
                trControl = ctrl)

#plsFit
png(filename = file.path(conf$plsda$directory, conf$plsda$fn_roc_plot))
plot(plsFit)
dev.off()
#plot(plsFit, plotType = "level")
#plsFit$finalModel

#Saving plsFit
save(plsFit, file = file.path(conf$plsda$directory, conf$plsda$fn_fit_file))

inTest <- createDataPartition(y = modelTestData$QuoteConversion_Flag, p = testPartitionPercent, list = FALSE)
testing = modelTestData[as.vector(inTest), ]


plsClasses <- predict(plsFit, newdata = testing[,-1, with = FALSE])
plsClasses
confusionMatrix(data = plsClasses, 
                reference = testing$QuoteConversion_Flag, 
                positive = "yes")

levels(plsClasses)[levels(plsClasses) == "no"] = "0"
levels(plsClasses)[levels(plsClasses) == "yes"] = "1"
levels(modelTestData$QuoteConversion_Flag)[levels(modelTestData$QuoteConversion_Flag) == "no"] = "0"
levels(modelTestData$QuoteConversion_Flag)[levels(modelTestData$QuoteConversion_Flag) == "yes"] = "1"

#pred = prediction(as.numeric(plsClasses),as.numeric(testing$QuoteConversion_Flag))
#perf = performance(pred, measure = "f")
#perf = performance(pred, measure = "auc")

plsRoc <- roc(as.numeric(plsClasses), as.numeric(testing$QuoteConversion_Flag))
print(plsRoc)

