# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# feature_selector.R - Feature selection models
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------
require(RRF)
require(caret)
require(ROCR)
require(data.table)
require(uuid)
require(dplyr)
require(tidyr)
require(yaml)

source("initializer.R")
source("data_processor2.R")
source("utility.R")
source("feature_constructor.R") # create_reduced_dataset

conf = yaml.load_file("project.conf")
interactive_session <- interactive()



load_homesite_preprocessed <- function(){
    standardInit()
    dataDir = conf$general$data_directory
    fnData = file.path(dataDir, "train.csv")
    homesite <- load_data(fnData)
    homesite <- data_preprocessing(homesite)
    
}

rrf_feature_selection <- function(homesite, n_trees, coef_regs, 
                                  train_fraction, train_subset_fraction, test_subset_fraction){

    train_ind <- createDataPartition(homesite$QuoteConversion_Flag, p = train_fraction, list = TRUE)

    exclude_columns <- c("QuoteConversion_Flag", "PersonalField16", "PersonalField17", 
                         "PersonalField18", "PersonalField19")
    
    x <- homesite[train_ind$Resample1, setdiff(colnames(homesite), exclude_columns), with = FALSE]
    y <- homesite[train_ind$Resample1, QuoteConversion_Flag]
    
    x_test <- homesite[!train_ind$Resample1, setdiff(colnames(homesite), exclude_columns), with = FALSE]
    y_test <- homesite[!train_ind$Resample1, QuoteConversion_Flag]
    
    train_subset_ind <- createDataPartition(y, p = train_subset_fraction, list = TRUE)
    test_subset_ind <- createDataPartition(y_test, p = test_subset_fraction, list = TRUE)
    

    df_results <- data.frame(ntrees = NA, coefregs = NA, acc_train = NA, acc_test = NA, features = NA)
    for(ntree in n_trees){
        for(coefreg in coef_regs){
        
            rrf_result <- RRF(x = x[train_subset_ind$Resample1,], 
                              y = y[train_subset_ind$Resample1], 
                              xtest = x_test[test_subset_ind$Resample1,], 
                              ytest = y_test[test_subset_ind$Resample1], 
                              subset = x_subset_ind$Resample1, ntree = ntree, importance = TRUE, 
                              do.trace = TRUE, keep.forest = TRUE, coefReg = coefreg)
            
            confusion <- rrf_result$confusion
            caret_confusion <- confusionMatrix(rrf_result$predicted, y[train_subset_ind$Resample1], positive = "1")
            
            acc_train <- caret_confusion$overall[1] #Accuracy
            
            confusion_test <- rrf_result$test$confusion
            caret_confusion_test <- confusionMatrix(rrf_result$test$predicted, y_test[test_subset_ind$Resample1], positive = "1")
            acc_test <- caret_confusion_test$overall[1]
            
            features <- rrf_result$feaSet
            
            df_results <- rbind(df_results, data.frame(ntrees = ntree, coefregs = coefreg, acc_train = acc_train, 
                                                       acc_test = acc_test, features = I(features), row.names = NULL))
            png(filename = file.path(conf$feature_selector$directory, 
                                     sub("[.]", paste0("_",UUIDgenerate(),"."), conf$feature_selector$fn_varImpPlot)),
                width = 480*3, height = 480*3)
            varImpPlot(rrf_result, main = paste0("VarImp ", "ntree=", ntree, " coefreg=", coefreg))
            dev.off()
        }
    }
    result <- list()
    result$df_results <- df_results[-1,]
    result$cols <- colnames(x)
    return(result)
}

read_results <- function(){
    #Still work
    df_results <- as.data.table(read.csv(file = file.path(conf$feature_selector$directory, conf$feature_selector$fn_rrf_results)))
    col_names <- as.data.table(read.csv(file.path(conf$feature_selector$directory, conf$feature_selector$fn_col_names)))

}

main <- function(){
    homesite <- load_homesite_preprocessed()
    n_trees = c(5, 10)#c(250, 500, 1000)
    coef_regs <- c(0.1, 0.3)#, 0.5, 0.8, 0.9)
    train_fraction <- 0.6
    train_subset_fraction <- 0.3
    test_subset_fraction <- 0.3
    result <- rrf_feature_selection(homesite, n_trees, coef_regs, 
                                         train_fraction, train_subset_fraction, test_subset_fraction)
    write.csv(result$df_results, file = file.path(conf$feature_selector$directory, conf$feature_selector$fn_rrf_results), row.names = FALSE)
    write.csv(result$cols, file = file.path(conf$feature_selector$directory, conf$feature_selector$fn_col_names), row.names = FALSE)
}

main()