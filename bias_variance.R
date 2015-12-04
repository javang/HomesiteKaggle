# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# bias_variance.R - Create bias/variance plots for a model
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------

source("svm_model.R")

create_bias_variance_plots <- function(data_point_fraction, number_of_features_list) {
    
    dataDir = conf$general$data_directory
    load(file.path(dataDir, conf$input$fn_reduced_training)) # loads modelTrainData
    load(file.path(dataDir, conf$input$fn_reduced_testing)) # loads modelTestData
    nrows = nrow(modelTrainData)
    indices = randomSelect(nrows, data_point_fraction)
    
    for(i in 1:length(number_of_features_list)){

        #Vertical partition of the dataset
        
        train_dataset = selectFeatures(modelTrainData, number_of_features_list[i])
        test_dataset = selectFeatures(modelTestData, number_of_features_list[i])

        ncols <- ncol(test_dataset)
        
        numCurvePoints = length(number_of_features_list)
        trainFs = rep(0, numCurvePoints)
        testFs = rep(0, numCurvePoints)
        loginfo(paste("Creating a SVM model using",length(indices), "datapoints and ", number_of_features_list[i], "features"))
        model <- train_svm(train_dataset, indices)
        trainF = evaluate_svm(model, train_dataset)
        trainFs[i] = trainF
        testF <- evaluate_svm(model, test_dataset)
        testFs[i] = testF
    }
    #fnCurves = file.path(dataDir, paste0("Bias-Variance.SVM.", data_point_fraction, ".Features.txt"))
#     fileConn = file(fnCurves, "w")
#     writeLines(c(paste("PointsFraction", "TrainFMeasure", "TestFMeasure",sep=",")), fileConn)
#     close(fileConn)
#     for (i in c(1:numCurvePoints)) {
#         fileConn = file(fnCurves, "at")
#         writeLines(c(paste(dataPointsFractions[i], trainF, testF, sep=",")), fileConn)
#         close(fileConn)    
#     }
    df = data.frame("NumberFeatures"=number_of_features_list, "TrainFMeasure"=trainFs, "TestFMeasure"=testFs)
    ggplot(df) +
        ggtitle(paste("Bias/Variance plot. Support Vector Machine.", length(indices), "observations.")) +
        xlab(paste("Number of features")) +
        ylab("F-measure") +
        geom_point(aes(x=NumberFeatures, y=TrainFMeasure, color="Train")) +
        geom_line(aes(x=NumberFeatures, y=TrainFMeasure, color="Train")) +
        geom_point(aes(x=NumberFeatures, y=TestFMeasure, color="Test")) +
        geom_line(aes(x=NumberFeatures, y=TestFMeasure, color="Test")) 
}

test <- function(){
    number_of_features_list <- seq(from = 10, to =100, by=10)
    create_bias_variance_plots(0.01, number_of_features_list)
}