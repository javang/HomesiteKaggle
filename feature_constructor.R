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

#eigenvalues <- pca_result$eig$eigenvalue
create_reduced_numeric_features_pca <- function(homesite, eigenvalues, target_dimensions){
    loginfo("Reducing numeric features with PCA.")
    numeric_columns <- get_numeric_features(homesite)
    numeric_column_names <- names(homesite)[numeric_columns]
    numeric_data_table <- homesite[,c(numeric_column_names),with=FALSE]
    #p is a d x k matrix with columns being the k principal components (eigenvalues from pca_result)
    p <- matrix(rep(eigenvalues, length(numeric_columns)),ncol=target_dimensions, nrow=length(numeric_columns), byrow=TRUE)
    #z is the matrix with the lower dimensional representation of the data
    dimension_names <- paste0("num_dim_", 1:target_dimensions)
    names(z) <- dimension_names
    z <- as.matrix(numeric_data_table) %*% p
    return(as.data.table(z))
}
append_reduced_numeric_features<- function(homesite, target_dimensions){
    loginfo("Reducing numeric features with PCA.")
    pca_result <- PCA(homesite, quali.sup = as.vector(get_factor_features(homesite)), graph = FALSE)
    z <- create_reduced_numeric_features_pca(homesite, pca_result$eig$eigenvalue[1:target_dimensions],target_dimensions)
    return(cbind(homesite, z))
}
