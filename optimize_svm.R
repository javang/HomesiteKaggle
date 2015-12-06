# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# optimize_svm.R - Use e1071 package to optimize the the parameters of the SVM.
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------
require(e1071)
require(yaml)
require(ROCR) # prediction, performance
require(yaml)

source("initializer.R")
source("utility.R")

conf = yaml.load_file("project.conf")
standardInit()



create_svm_baseline_models <- function(){
    loginfo("Creating SVM baseline models")
    dataDir = conf$general$data_directory_submission2
    fnTrainingDataset <- file.path(dataDir, conf$input$fn_reduced_training_submission2) # loads modelTrainData
    fnTestingDataset <- file.path(dataDir, conf$input$fn_reduced_testing_submission2) # loads modelTestData
    
    load(fnTrainingDataset)
    load(fnTestingDataset)
    
    svmDir <- conf$svm$svm_directory
    fnSVMBaselineParameters <- file.path(svmDir, conf$svm$fn_svm_baseline_parameters)
    svm_parameters <- read.csv(fnSVMBaselineParameters)
    
    for(i in 1:nrow(svm_parameters))
    {
        loginfo(paste("Processing baseline model", i))
        numberOfFeatures <- svm_parameters$numFeatures[i]
        dataPointsFractions <-svm_parameters$dataPointsFractions[i]

        
        training_indices <- randomSelect(nrow(modelTrainData), dataPointsFractions)
        training_dataset = selectFeatures(modelTrainData, numberOfFeatures) 
        training_dataset <- training_dataset[training_indices, ]
        
        testing_dataset = selectFeatures(modelTestData, numberOfFeatures)
        
        loginfo(paste("Number of features:", numberOfFeatures))
        loginfo(paste("Data points fractions: ", dataPointsFractions))
        loginfo(paste("Number of observations: ", length(training_indices)))
        
        baseline_model <- svm(QuoteConversion_Flag~., data = training_dataset, method = "C-classification", kernel = "radial", probability = FALSE)
        
        fnBaselineModel <- file.path(svmDir, conf$svm$fn_svm_baseline_model)
        fnBaselineModel <- sub("[.]", paste0("_",i,"."), fnBaselineModel)
        save(baseline_model, file = fnBaselineModel)
        fnBaselineModelResult = file.path(svmDir,conf$svm$fn_svm_baseline_model_result)
        print(baseline_model)
    }
    
}

create_svm_baseline_models()