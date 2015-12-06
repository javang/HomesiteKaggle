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
require("ROCR")
require(ggplot2)
source("utility.R")
source("initializer.R")
#source("learn_curves.R")
require(yaml)
conf = yaml.load_file("project.conf")
standardInit()

train_svm <- function(dataset){
    model <- svm(QuoteConversion_Flag~., 
                 data=dataset, 
                 method="C-classification", 
                 kernel="radial",
                 cost = 1,
                 gamma = 0.00729927,
                 na.action = na.omit, 
                 probability = TRUE)
    return (model)
}

tune_svm <- function(dataset){
params <- tune.svm(QuoteConversion_Flag~.,
                   data=dataset, 
                   gamma = 10^(-6:2), cost = 10^(-1:2))
}

evaluate_svm <- function(model, dataset, metric){
    svm_prediction <- predict(model, dataset[,-1,with=FALSE], probability = TRUE)
    pred <- prediction(as.numeric(svm_prediction), as.numeric(dataset$QuoteConversion_Flag))
    tab <- table(pred = svm_prediction, true = dataset[,QuoteConversion_Flag])
    if(metric=="Diagonal"){

        class_agreement <- classAgreement(tab = tab)
        return(class_agreement$diag)
    }else{
        if(metric=="F-measure"){
#             auc <- performance(pred, "f")
#             cutoff = 0.5
#             cutoffIndex = which(abs(auc@x.values[[1]] - cutoff) < 0.01)
#             fs <- auc@y.values[[1]][cutoffIndex]
#             result <- mean(fs)
            fscore_result <- fscore(tab) #compare the results...
            return(fscore_result)
        }else{
            if(metric=="Error rate"){
                error <- error_rate(tab)
                #error <- performance(pred, "err") 
                return(error)
            }
        }
    }
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

error_rate <- function(tab){
    tp = tab[2,2]
    fp = tab[2,1]
    tn = tab[1,1]
    fn = tab[1,2]
    return((fp+fn)/(tp+fp+tn+fn))
}
createLearningCurvesSVM = function(dataPointsFractions, numberOfFeatures, evaluation_metric) {
    dataDir = conf$general$data_directory
    vizDir = conf$general$visualizations_directory
    load(file.path(dataDir, conf$input$fn_reduced_training)) # loads modelTrainData
    load(file.path(dataDir, conf$input$fn_reduced_testing)) # loads modelTestData
    modelTrainData = selectFeatures(modelTrainData, numberOfFeatures) 
    modelTestData = selectFeatures(modelTestData, numberOfFeatures)
    
    nrows = nrow(modelTrainData)
    ncols <- ncol(modelTrainData)
    numCurvePoints = length(dataPointsFractions)
    trainFs = rep(0, numCurvePoints)
    testFs = rep(0, numCurvePoints)
    fnCurves = file.path(dataDir, paste0("LearningCurves.SVM.", evaluation_metric, ".", numberOfFeatures, ".Features.txt"))
    fileConn = file(fnCurves, "w")
    writeLines(c(paste("PointsFraction", "TrainFMeasure", "TestFMeasure",sep=",")), fileConn)
    close(fileConn)
    for (i in c(1:numCurvePoints)) {
        indices = randomSelect(nrows, dataPointsFractions[i])
        loginfo(paste("Creating a SVM model using",length(indices), "datapoints"))
        model <- train_svm(modelTrainData[indices, ])
        # evaluate the model on the data used to create it
        #trainF = evaluateLogisticRegression(model, modelTrainData[indices,]) 
        #trainFs[i] = trainF
        trainF = evaluate_svm(model, modelTrainData[indices, ], evaluation_metric)
        trainFs[i] = trainF

        testF <- evaluate_svm(model, modelTestData, evaluation_metric)
        testFs[i] = testF
        fileConn = file(fnCurves, "at")
        writeLines(c(paste(dataPointsFractions[i], trainF, testF, sep=",")), fileConn)
        close(fileConn)    
    }
    df = data.frame("PointsFraction"=dataPointsFractions, "TrainFMeasure"=trainFs, "TestFMeasure"=testFs)
    fnCurvesPlot = file.path(vizDir, paste0("LearningCurves.SVM.", evaluation_metric, ".", numberOfFeatures, ".Features.png"))
    png(file = fnCurvesPlot)
    p<-ggplot(df) +
        ggtitle(paste("Support Vector Machine, first", numberOfFeatures, "features")) +
        xlab(paste("Fraction of training points")) +
        ylab(evaluation_metric) +
        geom_point(aes(x=PointsFraction, y=TrainFMeasure, color="Train")) +
        geom_line(aes(x=PointsFraction, y=TrainFMeasure, color="Train")) +
        geom_point(aes(x=PointsFraction, y=TestFMeasure, color="Test")) +
        geom_line(aes(x=PointsFraction, y=TestFMeasure, color="Test")) 
    print(p)
    dev.off()
}

svm_learning_curves <- function(){
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),10, "Diagonal")
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),20, "Diagonal")
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),30, "Diagonal")
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),40, "Diagonal")
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),50, "Diagonal")
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),60, "Diagonal") 
}

svm_learning_curves_fscore <- function(){
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),10, "F-measure")
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),20, "F-measure")
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),30, "F-measure")
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),40, "F-measure")
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),50, "F-measure")
    createLearningCurvesSVM(c(0.1, 0.15, 0.2, 0.25, 0.3),60, "F-measure")
}

