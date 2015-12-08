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
source("utility.R")

pca_dimension_reduction <- function(homesite, eigenvectors, target_dimensions){
    ' Transforms the numeric features in a dataset to the PCA features
    
    homesite: data.table with the dataset
    eigenvectors: The eigenvectors of the PCA decomposition
    target_dimensions: Number of PCA dimensions to keep. 
    :return: A data.table with the values of the PCA features. The number of 
    rows is the same as in homesite, and the number of columns is target_dimensions.
    '
    loginfo("Reducing dimensions with PCA.")
    numeric_columns <- get_numeric_features(homesite)
    numeric_column_names <- names(homesite)[numeric_columns]
    numeric_data_table <- homesite[,c(numeric_column_names),with=FALSE]
    p <- as.matrix(eigenvectors[,1:target_dimensions], ncol = target_dimensions, byrow = TRUE) #pca_result$loadings
    z <- as.matrix(numeric_data_table) %*% p
    return(as.data.table(z))
}

chi_squared_feature_reduction <- function(homesite, target_categorical_features){
    ' Apply Chi-Squared feature prioritization to a dataset and retain the most
    significant fetures based on their Chi-Squared importance
    
    homesite: A data.table with the dataset
    target_categorical_features: Number of categorical values to retain. 
    :return: This function returns a data.table object with the columns given
    by the categorical variables selected by Chi-Squared.
    '
    loginfo("Performing feature reduction of categorical variables")
    chi_squared_result <- apply_chi_square_feature_selection(homesite, trainingFraction=0.3)
    sortingIndices = order(chi_squared_result, decreasing = TRUE)
    loginfo(paste("Selecting the best",target_categorical_features, "features"))
    selectedFeatures = row.names(chi_squared_result)[sortingIndices[1:target_categorical_features]]
    return(homesite[,selectedFeatures, with = FALSE])
}

create_reduced_dataset <- function(homesite, target_numeric_dimensions, target_categorical_features){
    ' Feature selection based on reducing the dimensionality of the continuous 
    variables using PCA and reducing the number of categorical values by
    applying the chi-squared algorithm 
    '
    loginfo("Creating a dimension reduced dataset, from the original dataset")
    loginfo("to PCA reduced numeric features")
    loginfo("and chi squared reduced categorical features.")
    pca_result <- pca_factor_analysis(homesite) #decouple by persisting pca_results
    eigenvectors <- pca_result$loadings
    dataDir = conf$general$data_directory
    write.csv(eigenvectors, file.path(dataDir, "PCAloadings.txt"))
    
    pca_reduced_features <- pca_dimension_reduction(homesite, eigenvectors, target_numeric_dimensions)
    # chi_squared_reduced_features <- chi_squared_feature_reduction(homesite, target_categorical_features) #decouple by persisting indexes 

        # instead of selecting features, use all of them
    chi_squared_reduced_features = get_factor_features(homesite)
    chi_squared_reduced_features = chi_squared_reduced_features[2:length(chi_squared_reduced_features)] # first feature is the target value
    chi_squared_reduced_features = homesite[,chi_squared_reduced_features, with=FALSE] 
    
    reduced_dataset = as.data.table(cbind(QuoteConversion_Flag=homesite$QuoteConversion_Flag, pca_reduced_features,chi_squared_reduced_features))
    return(reduced_dataset)
}


