# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# factor_analizer.R - Factor Analizer
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
source("data_preprocessor.R")
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
    ' Runs PCA on the input dataset and returns the result
    '
    loginfo("Peforming Principal Component analysis")
    numeric_columns <- get_numeric_features(homesite)
    pca_result <- princomp(homesite[,numeric_columns, with=FALSE], cor = TRUE, scores = TRUE)
    #write.csv(pca_result$scores)
    return(pca_result)
}

mca_factor_analysis <- function(homesite){
    loginfo("Peforming MCA factor analysis")
    homesite[,Original_Quote_Date:=NULL]
    homesite[,Original_Quote_Date_Typed:=NULL]
    homesite[,QuoteConversion_Flag:=NULL]
    
    mca_result <- MCA(homesite, quanti.sup = as.vector(get_numeric_features(homesite)), graph = FALSE)
}

famd_factor_analysis <- function(homesite){
    loginfo("Peforming FAMD factor analysis")
    homesite[,Original_Quote_Date:=NULL]
    homesite[,Original_Quote_Date_Typed:=NULL]
    homesite[,QuoteConversion_Flag:=NULL]
    
    famd_result <- FAMD(homesite, graph = FALSE)
}

plot_individuals_by_factor <- function(pca_result, homesite){
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

factominer_tutorial <- function(){
    data("decathlon")
    fileName = paste0("PCA_tutorial_%03d.png") 
    outputPath = file.path(conf$general$data_directory, "visualizations",  fileName)
    png(outputPath)
    
    res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
    #Plot individuals according to categorical value, color individuals by factor specified with habillage
    plot(res.pca, habillage = 13)
    #Draw a barplot with the eigen values
    barplot(res.pca$eig[,1], main = "Eigenvalues", names.arg = paste("Dim", 1:nrow(res.pca$eig), sep=""))
    #Plot a graph with 2 dimensions
    plot(res.pca, choix = "var", axes = c(3,4), lim.cos2.var = 0)
    #Print results
    print(res.pca, file="pca_result.txt", sep = "\t")
    #Describe each pricipal component
    print(dimdesc(res.pca, proba = 0.2))
    dev.off()
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

# apply_mca(homesite)

bernoulli_sampling <- function(dt, trainingFraction){
    #Refactoring candidate
    values = runif(nrow(dt))
    val = dt[values < trainingFraction,]
    return(val)
}

apply_chi_square_feature_selection = function(homesite, trainingFraction=0.3) {
    ' Apply the chi-square algorithm for dimensionality reduction of categorical
    values. 
    '
    factorColumns = get_factor_features(homesite)
    factorColumnNames = names(homesite)[factorColumns]
    factorsDataTable = homesite[, c(factorColumnNames), with=FALSE]
    trainingTable = bernoulli_sampling(factorsDataTable, trainingFraction)
    formula = QuoteConversion_Flag ~ .
    DT = chi.squared(formula, trainingTable)
    return(DT)
}


