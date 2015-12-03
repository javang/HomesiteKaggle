# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# svm_model.R - Model with an SVM
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------


require(e1071)
source("learn_curves.R")



train_svm <- function(dataset, indices){
    model <- svm(QuoteConversion_Flag~., 
                 data=dataset[indices], 
                 method="C-classification", 
                 kernel="radial",
                 cost = 1,
                 gamma = 0.00729927,
                 na.action = na.omit, 
                 probability = TRUE)
    return (model)
}

tune_svm <- function(dataset, indices){
params <- tune.svm(QuoteConversion_Flag~.,
                   data=dataset[indices], 
                   gamma = 10^(-6:2), cost = 10^(-1:2))
}

evaluate_svm <- function(model, dataset){
    svm_prediction <- predict(model, dataset[,-1,with=FALSE], probability = TRUE)
    #perf_result <- performance(svm_prediction, measure = "f")
    tab <- table(pred = svm_prediction, true = dataset[,QuoteConversion_Flag])
    
    class_agreement <- classAgreement(tab = tab)
    return(class_agreement$diag)
}

fscore <- function(tab){
    tp = tab[2,2]
    fp = tab[2,1]
    tn = tab[1,1]
    fn = tab[1,2]
    precision <- tp/(tp+fp)
    recall <- tp/(tp+fn)
    return(2*((precision*recall)/(precision+recall)))
}


createLearningCurvesSVM = function(dataPointsFractions, numberOfFeatures) {
    dataDir = conf$general$data_directory
    load(file.path(dataDir, conf$input$fn_reduced_training)) # loads modelTrainData
    load(file.path(dataDir, conf$input$fn_reduced_testing)) # loads modelTestData
    modelTrainData = selectFeatures(modelTrainData, numberOfFeatures) 
    modelTestData = selectFeatures(modelTestData, numberOfFeatures)
    
    nrows = nrow(modelTrainData)
    ncols <- ncol(modelTrainData)
    numCurvePoints = length(dataPointsFractions)
    trainFs = rep(0, numCurvePoints)
    testFs = rep(0, numCurvePoints)
    fnCurves = file.path(dataDir, paste0("LearningCurves.SVM.", numberOfFeatures, ".Features.txt"))
    fileConn = file(fnCurves, "w")
    writeLines(c(paste("PointsFraction", "TrainFMeasure", "TestFMeasure",sep=",")), fileConn)
    close(fileConn)
    for (i in c(1:numCurvePoints)) {
        indices = randomSelect(nrows, dataPointsFractions[i])
        loginfo(paste("Creating a SVM model using",length(indices), "datapoints"))
        model <- train_svm(modelTrainData, indices)
        # evaluate the model on the data used to create it
        #trainF = evaluateLogisticRegression(model, modelTrainData[indices,]) 
        #trainFs[i] = trainF
        trainF = evaluate_svm(model, modelTrainData[indices, ])
        trainFs[i] = trainF

        testF <- evaluate_svm(model, modelTestData)
        testFs[i] = testF
        fileConn = file(fnCurves, "at")
        writeLines(c(paste(dataPointsFractions[i], trainF, testF, sep=",")), fileConn)
        close(fileConn)    
    }
    df = data.frame("PointsFraction"=dataPointsFractions, "TrainFMeasure"=trainFs, "TestFMeasure"=testFs)
    ggplot(df) +
        ggtitle(paste("Support Vector Machine, first", numberOfFeatures, "features")) +
        xlab(paste("Fraction of training points")) +
        ylab("F-measure") +
        geom_point(aes(x=PointsFraction, y=TrainFMeasure, color="Train")) +
        geom_line(aes(x=PointsFraction, y=TrainFMeasure, color="Train")) +
        geom_point(aes(x=PointsFraction, y=TestFMeasure, color="Test")) +
        geom_line(aes(x=PointsFraction, y=TestFMeasure, color="Test")) 
}

createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),10)
createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),20)
createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),30)
createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),40)
createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),50)
createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),60)
createLearningCurvesSVM(seq(0.01,0.2, by=0.01),1)
