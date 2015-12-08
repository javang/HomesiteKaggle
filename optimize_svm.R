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

source("svm_model.R")
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

evaluate_svm_baseline_models <- function(){
    dataPointsFractionsTraining <- 0.1
    dataPointsFractionsTesting <- 0.001
    numberOfFeatures <- 155
    datasetVersion = "V0"
    
    loginfo("Creating SVM baseline models")
    if(datasetVersion == "V0"){
        dataDir = conf$general$data_directory
        fnTrainingDataset = file.path(dataDir, conf$input$fn_reduced_training)
        fnTestingDataset = file.path(dataDir, conf$input$fn_reduced_testing)
        
    } else{
        if(datasetVersion = "V2"){
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
    testing_dataset = selectFeatures(modelTestData, numberOfFeatures+1)
    validation_dataset <- testing_dataset[-testing_indices, ]
    testing_dataset <- testing_dataset[testing_indices, ]
    
    svmDir <- conf$svm$svm_directory
    fnModel <- file.path(svmDir, "svm_baseline_model_1.RData")
    fnModel <- file.path(svmDir, "svm_baseline_model_2.RData")
    fnModel <- file.path(svmDir, "svm_baseline_model_3.RData")
    fnModel <- file.path(svmDir, "Prediction_c70ec4a2-9d16-11e5-9b95-959b047162e7.RData")
    fnModel <- file.path(svmDir, "Prediction_b5555776-9d39-11e5-83aa-0b4414b781be.RData")
    #
    
    load(fnModel) #PArameters are referred as params
    probability_model <- params$best.model # No need to train here.
    
    probability_model <- train_svm(training_dataset, 
                                             params$best.parameters$cost, 
                                             params$best.parameters$gamma,  
                                             FALSE)
    
    probability_model <- train_svm(training_dataset, 
                                   params$best.parameters$cost, 
                                   params$best.parameters$gamma,  
                                   FALSE)
    
    p2 <- train_svm(training_dataset, 
                                   1, 
                                   0.00729927,  
                                   FALSE)

    predictions <- predict(probability_model, modelTestData[1:10,], probability = FALSE, decision.values = FALSE)
    tab <- table(pred = predictions, true = modelTestData[1:10]$QuoteConversion_Flag)
    
        
    predictions <- predict(probability_model, testing_dataset, probability = FALSE, decision.values = FALSE)
    tab <- table(pred = predictions, true = testing_dataset$QuoteConversion_Flag)
    classAgreement(tab)
    fscore(tab)
    error_rate(tab)
    attr(predictions, "decision.values")
    attr(predictions, "probabilities")
    #Save model for later reuse...
    save(probability_model, file = "probability_model_optimized_v0.RData")
    
    svm_model <- attach(fnModel)
    pred <- prediction(as.integer(predictions), as.integer(testing_dataset$QuoteConversion_Flag))

    perf <- performance(pred, measure ="tpr", x.measure = "fpr")
    plot(perf, avg = "threshold", colorize = T, lwd = 3, main = "ROC")

    
    perf <- performance(pred, measure ="prec", x.measure = "rec")
    plot(perf, avg = "threshold", colorize = T, lwd = 3, main = "Precision/Recall")
    
    perf <- performance(pred, measure ="sens", x.measure = "spec")
    plot(perf, avg = "threshold", colorize = T, lwd = 3, main = "Sensitivity")
    
    perf <- performance(pred, measure ="lift", x.measure = "rpp")
    plot(perf, avg = "threshold", colorize = T, lwd = 3, main = "Lift")
    
    
    gammas <- c(0.000003, 0.00003, 0.0003, 0.0003979308, 0.003, 0.03)
    costs <- c(0.1, 1, 10, 100, 1000)
    params <- tune.svm(QuoteConversion_Flag~.,
                       data=training_dataset[1:100, ], 
                       validation.x = validation_dataset[1:100, ],
                       gamma = gammas, cost = costs)
    
    #Save params, print best parameters
    probability_model <- svm(QuoteConversion_Flag~., 
                 data=training_dataset[1:100, ], 
                 method="C-classification", 
                 kernel="radial",
                 cost = params$best.parameters$cost,
                 gamma = params$best.parameters$gamma,
                 na.action = na.omit, 
                 probability = TRUE)
    
    #TODO: Save probability model.
    
    
    

    save(params, file = "params.RData")
    rm(params)
    attach("Prediction_24fa2b5c-9cb2-11e5-913c-3d512d341a89.RData")
    params$best.parameters
    best_model <- params$best.model
    search()
    detach("file:params.RData")
    

    
    predictions <- predict(best_model, training_dataset[1:1000,-1, with=FALSE], probability = FALSE)
    tab <- table(pred = predictions, true = training_dataset[1:1000]$QuoteConversion_Flag)
    classAgreement(tab)
    fscore(tab)
    error_rate(tab)
    
    auc <- performance(predictions, "err")
    svm_evaluation <- evaluate_svm(baseline_model, validation_dataset, FALSE, "Error rate") 
    
    
    
    
    
    
}

create_svm_baseline_models()