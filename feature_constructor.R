# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# feature_constructor.R - Builds features for the model
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# Notes: 
# TODO: Persist eigenvalues and read them from file instead of calling PCA each time.
# --------------------------------------------
source("factor_analyzer.R")

#eigenvalues <- pca_result$eig$eigenvalue
create_reduced_numeric_features_pca <- function(homesite, eigenvalues, target_dimensions){
    loginfo("Reducing numeric features with PCA.")
    numeric_columns <- get_numeric_features(homesite)
    numeric_column_names <- names(homesite)[numeric_columns]
    numeric_data_table <- homesite[,c(numeric_column_names),with=FALSE]
    #p is a d x k matrix with columns being the k principal components (eigenvalues from pca_result)
    p <- matrix(rep(eigenvalues, length(numeric_columns)),ncol=target_dimensions, byrow=TRUE) #nrow=length(numeric_columns), 
    #z is the matrix with the lower dimensional representation of the data
    dimension_names <- paste0("num_dim_", 1:target_dimensions)
    names(z) <- dimension_names
    z <- as.matrix(numeric_data_table) %*% p
    return(as.data.table(z))
}

pca_dimension_reduction <- function(homesite, eigenvectors, target_dimensions){
    loginfo("Reducing dimensions with PCA.")
    numeric_columns <- get_numeric_features(homesite)
    numeric_column_names <- names(homesite)[numeric_columns]
    numeric_data_table <- homesite[,c(numeric_column_names),with=FALSE]
    p <- as.matrix(eigenvectors[,1:target_dimensions], ncol = target_dimensions, byrow = TRUE) #pca_result$loadings
    z <- as.matrix(numeric_data_table) %*% p
    return(as.data.table(z))
}
#Deprecating: 
append_reduced_numeric_features<- function(homesite, target_dimensions){
    loginfo("Reducing numeric features with PCA.")
    eigen_file = file.path(conf$general$resources_directory, "pca-eigenvalues.csv")
    if (!file.exists(eigen_file)){
        loginfo("Eigen file not found. Calling PCA.")
        pca_result <- PCA(homesite, quali.sup = as.vector(get_factor_features(homesite)), graph = FALSE)
        write.csv(pca_result$eig, file = eigen_file)
        eigenvalues <- pca_result$eig$eigenvalue[1:target_dimensions]
    }else{
        loginfo("Eigen file found, reading from it.")
        eigen_csv <- read.csv(eigen_file)
        eigenvalues <- eigen_csv$eigenvalue[1:target_dimensions]
    }
    z <- create_reduced_numeric_features_pca(homesite, eigenvalues ,target_dimensions)
    return(cbind(homesite, z))
}

chi_squared_feature_reduction <- function(homesite, target_categorical_features){
    chi_squared_result <- apply_chi_square_feature_selection(homesite, 0.1)
    features_to_keep <- row.names(chi_squared_result)[1:target_categorical_features]
    return(homesite[,features_to_keep, with = FALSE])
}

create_reduced_dataset <- function(homesite, target_numeric_dimensions, target_categorical_features){
    loginfo("Creating a dimension reduced dataset, from the original dataset")
    loginfo("to PCA reduced numeric features")
    loginfo("and chi squared reduced categorical features.")

    pca_result <- pca_factor_analysis(homesite) #decouple by persisting pca_results
    eigenvectors <- pca_result$loadings
    pca_reduced_features <- pca_dimension_reduction(homesite, eigenvectors, target_numeric_dimensions)
    
    chi_squared_reduced_features <- chi_squared_feature_reduction(homesite, target_categorical_features) #decouple by persisting indexes 
    reduced_dataset <- cbind(homesite[,conf$model$label_field, with=FALSE], 
                             pca_reduced_features, 
                             chi_squared_reduced_features)
    return(as.data.table(reduced_dataset))
    
}