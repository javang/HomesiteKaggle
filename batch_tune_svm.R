# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# batch_tune_svm.R - Use e1071 package to batch tune the the parameters of the SVM.
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# NOTES:    RScript.exe batch_tune_svm.R DATASET_VERSION, NUM_FEATURES TRAINING_FRACTION, TESTING_FRACTION VALIDATION_FRACTION
#           DATASET_VERSION = {V0, V2}
#           RScript.exe batch_tune_svm.R V0 100 0.5 0.5 0.5 
# SAMPLES:
#  batch_tune_svm.R V0 20 0.001 0.001 0.001 > batch_v0_20_0.001_0.001_0.001.txt
#  batch_tune_svm.R V2 40 0.001 0.001 0.001 > batch_v2_40_0.001_0.001_0.001.txt
#  batch_tune_svm.R V0 100 0.4 0.4 0.4 > batch_v0_100_0.4_0.4_0.4.txt
#  batch_tune_svm.R V2 100 0.4 0.4 0.4 > batch_v2_100_0.4_0.4_0.4.txt
# --------------------------------------------

require(e1071)
require(uuid)
require(yaml)

source("svm_model.R")
source("initializer.R")
source("utility.R")

conf = yaml.load_file("project.conf")
standardInit()


batch_tune_svm <- function(){
    loginfo("Batch tuning SVM")
    arguments <- commandArgs(trailingOnly = TRUE)
    
    #Parameters:
    datasetVersion <- arguments[1] #V0, V2
    numberOfFeatures <- as.numeric(arguments[2]) #100
    dataPointsFractionsTraining <- as.numeric(arguments[3])#0.01
    dataPointsFractionsTesting <- as.numeric(arguments[4]) #0.01
    dataPointsFractionsValidation <- as.numeric(arguments[5])# 0.5

    #TODO: Encode this from this from the command prompt
    gammas <- c(0.000003, 0.00003, 0.0003, 0.0003979308, 0.003, 0.03)
    costs <- c(0.1, 1, 10, 100, 1000)
    
    #Read dataset, tuning parameters, tune, save tune result


    if(datasetVersion == "V0"){
        dataDir = conf$general$data_directory
        fnTrainingDataset <- file.path(dataDir, conf$input$fn_reduced_training) # loads modelTrainData
        fnTestingDataset <- file.path(dataDir, conf$input$fn_reduced_testing) # loads modelTestData
        
    }else{
        if(datasetVersion == "V2"){
            dataDir = conf$general$data_directory_submission2
            fnTrainingDataset <- file.path(dataDir, conf$input$fn_reduced_training_submission2) # loads modelTrainData
            fnTestingDataset <- file.path(dataDir, conf$input$fn_reduced_testing_submission2) # loads modelTestData
        }
    }
    
    
    load(fnTrainingDataset)
    load(fnTestingDataset)
    
    training_indices <- randomSelect(nrow(modelTrainData), dataPointsFractionsTraining)
    training_dataset = selectFeatures(modelTrainData, numberOfFeatures+1) 
    training_dataset <- training_dataset[training_indices, ]
    
    testing_indices <- randomSelect(nrow(modelTestData), dataPointsFractionsTesting)
    testing_dataset <- selectFeatures(modelTestData, numberOfFeatures+1)
    testing_dataset <- testing_dataset[testing_indices, ]
    
    validation_indices <- randomSelect(nrow(modelTestData), dataPointsFractionsValidation)
    validation_dataset = selectFeatures(modelTestData, numberOfFeatures+1)
    validation_dataset <- validation_dataset[-testing_indices, ]

    params <- tune.svm(QuoteConversion_Flag~.,
                       data=training_dataset, 
                       validation.x = validation_dataset,
                       gamma = gammas, cost = costs)
    
    best_model <- params$best.model
    
    predictions <- predict(best_model, testing_dataset[,-1, with=FALSE], probability = FALSE)
    tab <- table(pred = predictions, true = testing_dataset$QuoteConversion_Flag)
    print(paste("Diagonal: ", classAgreement(tab)$diagonal))
    print(paste("Dataset version", datasetVersion))
    print(paste("Number of features", numberOfFeatures))
    print(paste("dataPointsFractionsTraining", dataPointsFractionsTraining))
    print(paste("dataPointsFractionsTesting", dataPointsFractionsTraining))
    print(paste("dataPointsFractionsValidation", dataPointsFractionsTraining))
    print(paste("F-Score", fscore(tab)))
    print(paste("Error rate:", error_rate(tab)))
    print(paste("Best model: "))
    print(best_model)
    print(paste("Best parameters: "))
    print(params$best.parameters)
    fnParams <- paste0("Prediction_",UUIDgenerate(),".RData")
    print(paste("Parameter file: ", fnParams))
    save(params, file = fnParams)
    
}
batch_tune_svm()