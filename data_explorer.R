# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# data_explorer.R - Data Exploration Functions
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------
require(e1071)
require(caret)
require(corrplot)
require(caret)

univariate_numerical_exploration <- function(dataset, col_range, dataset_name){
  loginfo(paste("Univariate numerical exploration for ", dataset_name))
  print("*********************************************************************")
  print(paste("Univariate numerical exploration for ", dataset_name))
  print("*********************************************************************")
  print("Head")
  print(head(dataset))
  print("Structure")
  print(str(dataset))
  print("Summary")
  print(summary(dataset))
  for (i in col_range){
    if(conf$general$DEBUG){loginfo(paste("Column=",i))}
    var_name <- names(dataset)[i]
    print(paste("Inner quantile range for ",var_name))
    print(IQR(dataset[[i]], na.rm = TRUE))
    print(paste("Quantile for ", var_name))
    print(quantile(dataset[[i]], na.rm = TRUE))
  }
}

univariate_visual_exploration <- function(dataset, col_range, dataset_name){
  loginfo(paste("Univariate Visual Exploration for ", dataset_name))
  for (i in col_range){
    fileName = paste0(names(dataset)[i],".png") 
    outputPath = file.path(conf$general$data_directory, "visualizations",  fileName)
    # pdf(outputPath, onefile = TRUE)
    png(outputPath)
    par(mfrow = c(2,2))
    hist(dataset[[i]],main = names(dataset)[i], sub="Histogram")
    plot(density(dataset[[i]], na.rm = TRUE), main = paste("Density for", names(dataset)[i]))
    plot(ecdf(dataset[[i]]), main = paste("ECDF for", names(dataset)[i]))
    qqnorm(dataset[[i]], main = paste("QQnorm for", names(dataset)[i]))
    dev.off()
  }
}

test_as_factors = function(homesite, columNames, datasetName) {
    ' Convert to factors all the columns with integers and print the levels.
    This function intends to check if what we see as integer columns are indeed factors
    '
    for (columnName in columNames) {
        fac = as.factor(homesite[,get(columnName)])
        txt =  paste(levels(fac), collapse=" ")
        loginfo(paste("Levels for column", columnName, ":",txt))
    }
}



bivariate_numerical_exploration <- function(dataset, dataset_name){
  loginfo(paste("Bivariate numerical exploration for",dataset_name))
  
  print("*********************************************************************")
  print(paste("Bivariate numerical exploration for",dataset_name))
  print("*********************************************************************")
  
  print("Variance:")
  print(var(dataset, na.rm = TRUE))
  print("Covariance:")
  print(cov(dataset, use = "complete.obs"))
  print("Correlation:")
  print(cor(dataset, use = "complete.obs"))
}


exploreFactors = function() {
    require(yaml)
    source("data_processor2.R")
    conf = yaml.load_file("project.conf")
    dataDir = conf$general$data_directory
    fnData = file.path(dataDir, conf$input$whole_train_data)
    fnTest = file.path(dataDir, conf$input$whole_test_data)
    homesite <- load_data(fnData)
    homesite = transformAndClean(homesite)
    homesite = assignDataTypes(homesite)
    homesiteTestData = load_data(fnTest)
    homesiteTestData = transformAndClean(homesiteTestData)
    homesiteTestData = assignDataTypes(homesiteTestData)
    compare_test_vs_train_factors(homesite, homesiteTestData)        
    
}

compare_test_vs_train_factors = function(homesite, testData) {
    ' This function compares the training and test dataset to determine:

    - If features with numeric values are indeed numeric or categorical.
    - When considering a variable categorical, check if the training and test
    sets from kaggle all contain the same levels for the factors.

    homesite: A data.table with the homesite training data from Kaggle
    testData: A data.table with the homesite testing data from Kaggle
    '
    testColumnNames = names(testData)
    trainColumnNames = names(homesite)
    for (columnName in testColumnNames) {
        print(columnName)
        if ((columnName %in% trainColumnNames) && is.integer(testData[,get(columnName)])) {
            trainFactor = as.factor(homesite[,get(columnName)])
            testFactor = as.factor(testData[,get(columnName)])
            trainLevels = levels(trainFactor)
            testLevels = levels(testFactor)
            loginfo(paste("*******************", columnName, "*******************"))
            loginfo(paste("TRAIN VALUES:",paste(trainLevels, collapse=" ")))
            loginfo(paste("TEST  VALUES:",paste(testLevels, collapse=" ")))
            setDiffTrain = setdiff(trainLevels, testLevels)            
            setDiffTest = setdiff( testLevels, trainLevels)            
            loginfo(paste("TRAIN VALUES NOT IN TEST:",paste(setDiffTrain, collapse=" ")))
            loginfo(paste("TEST VALUES NOT IN TRAIN:",paste(setDiffTest, collapse=" ")))
        }
    }
}


checkSkewness = function(dataset) {
    ' Calculate the skewness of the numeric features of a dataset
    '
    nFeatures = originalTrainData[,numericFeatures, with=FALSE]
    sks = sapply(nFeatures, skewness)
    return(sks)
}

getNearZero = function(dataset) {
    nFeatures = originalTrainData[,numericFeatures, with=FALSE]
    indices = nearZeroVar(nFeatures)
    names = numericFeatures[indices]
}


exploreCorrelations = function(originalTrainData, featureNames) {
    ' Calculate and plot correlations between the numeric variables and plot them 
    '
    # featureNames = definedNumericFeatures
    features = originalTrainData[,featureNames, with=FALSE]
    correlations = cor(features)     
    corrplot(correlations, order="hclust")
    # use caret to find the indices of highly correlated variables
    highCorr = findCorrelation(correlations, cutoff = 0.90) 
    return(featureNames[highCorr]) # names of the variables recommended for deletion
}
