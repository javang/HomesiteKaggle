# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# factor_analizer.R - Factor Analizer. Contains functions to analyze features
# and applying algorithms for dimensionality reduction.
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# Notes: 
# PCA supports datasets with numeric features only.
# The Homesite model we built has both numeric and categorical features.
# This module shall perform factor analysis for both the numeric and categorical features.
# Initial analysis shall be performed with Multiple Factor Analysis with the FactoMineR library
# For an overview of the package please review: http://factominer.free.fr/
# --------------------------------------------
require("FactoMineR")
require(MASS) # write.matrix
require(FSelector)  # 
source("data_processor2.R")
source("utility.R")


factor_analysis <- function(homesite){
    loginfo("Peforming factor analysis")
    #Executing the following line of code is very slow, uncomment on an as-needed basis:    
    #print(pca_result, file = "pca_result2.txt", sep = "\t")
    pca_result <- PCA(homesite, quali.sup = as.vector(get_factor_features(homesite)))
    plot_individuals_by_factor(pca_result, homesite)

    #Looks like PCA::print does not support file and sep attributes for the following cases:
#     print(pca_result$eig)
#     print(pca_result$var)
#     print(pca_result$var$coord)
#     print(pca_result$var$cor)
#     print(pca_result$var$cos2)
#     print(pca_result$var$contrib)
#     print(pca_result$ind)
#     print(pca_result$ind$coord)
#     print(pca_result$ind$cos2)
#     print(pca_result$ind$contrib)
#     print(pca_result$quali.sup)
#     print(pca_result$quali.sup$coord)
#     print(pca_result$quali.sup$v.test)
#     print(pca_result$call)
#     print(pca_result$call$centre)
#     print(pca_result$call$ecart.type)
#     print(pca_result$call$row.w)
#     print(pca_result$call$col.w)
}

pca_factor_analysis <- function(homesite){
    ' Runs PCA on the input dataset and returns the result (the R object)
    '
    loginfo("Peforming Principal Component analysis")
    numeric_columns <- get_numeric_features(homesite)
    pca_result <- princomp(homesite[,numeric_columns, with=FALSE], cor = TRUE, scores = TRUE)
    #write.csv(pca_result$scores)
    return(pca_result)
}

mca_factor_analysis <- function(homesite){
    ' Runs MCA on the input dataset and returns the result (the R object)
    '
    loginfo("Peforming MCA factor analysis")
    homesite[,Original_Quote_Date:=NULL]
    homesite[,Original_Quote_Date_Typed:=NULL]
    homesite[,QuoteConversion_Flag:=NULL]
    mca_result <- MCA(homesite, quanti.sup = as.vector(get_numeric_features(homesite)), graph = FALSE)
    return(mca_result)
}

famd_factor_analysis <- function(homesite){
    ' Runs FAMD on the input dataset and returns the result (the R object)
    '
    loginfo("Peforming FAMD factor analysis")
    homesite[,Original_Quote_Date:=NULL]
    homesite[,Original_Quote_Date_Typed:=NULL]
    homesite[,QuoteConversion_Flag:=NULL]
    famd_result <- FAMD(homesite, graph = FALSE)
    retrun(famd)
}

plot_individuals_by_factor <- function(pca_result, homesite){
    ' Generate plots for each of the factors created by the PCA analysis'
    loginfo("Plotting individuals according to categorical value.")
    factor_features <- get_factor_features(homesite)
    for(i in 1:length(factor_features))
    {
        fileName = paste0(names(factor_features[i]),".png") 
        outputPath = file.path(conf$general$data_directory, "visualizations/PCA",  fileName)
        print(outputPath)
        png(outputPath)
        plot(pca_result, habillage = as.numeric(factor_features[i]))
        dev.off()
    }
}

apply_mca = function(homesite) {
    ' Apply Multiple Component Analysis to a data table object. This function
    is producing results that are difficult to interpret.
    '
    factorColumns = get_factor_features(homesite)
    factorColumnNames = names(homesite)[factorColumns]
    factorsDataTable = homesite[, c(factorColumnNames), with=FALSE]
    factorsDataTable[,QuoteConversion_Flag:=NULL]
    for (i in c(1:10)) {
        selectedIndices = splitDataTable(factorsDataTable, trainingFraction = 0.01)
        trainingTable = factorsDataTable[selectedIndices == TRUE, ]
        mcaAnalysis = MCA(trainingTable, ncp=20)
        fnEig = file.path(conf$general$data_directory, paste0("mca-eigen-",i,".txt"))
        write.matrix(mcaAnalysis$eig, file=fnEig, sep='\t')
    }
}

