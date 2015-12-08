# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# create_predictions_svm.R - Create predictions for the test set provided by Kaggle
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------

require(data.table)
require(yaml)
source("predictions_common.R")
source("create_predictions.R")

conf = yaml.load_file("project.conf")

standardInit()


predict_v0 <- function(){
    dataDir = conf$general$data_directory
    load(file.path(dataDir, conf$input$fn_reduced_test_data)) # loads homesiteTestData
    #homesiteTestData <- fixVariables(homesiteTestData)
    #TODO: Load probability model

    svmDir <- conf$svm$svm_directory
    fnProbabilityModelSVM <- file.path(svmDir, conf$svm$fn_probability_model_v0)
    load(fnProbabilityModelSVM)
    
    #probability_model <- params$best.model -> no longer needed
    predictions <- predict(probability_model, homesiteTestData, probability = TRUE, decision.values = TRUE)
    
    fnTestData <- file.path(conf$general$data_directory, conf$input$whole_test_data)
    df_test_data <- load_data(fnTestData)
    quotenums = df_test_data[,QuoteNumber ]
    
    p <- attr(predictions, "probabilities")[,1]
    preds = data.frame(QuoteNumber=quotenums, QuoteConversion_Flag=p)
    fnPredictions = file.path(dataDir, conf$output$fn_output_predictions)  
    write.csv(preds,file=fnPredictions,row.names = FALSE)
}

predict_v2 <- function(){
    
}