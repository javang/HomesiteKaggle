# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# data_processor2.R - Second version of the data processing functions
#
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# THIS SCRIPT CONTAINS MOST OF THE FUNCTIONS IN data_preprocessor.R but 
# it was created when refining the algorithms. Some of the features previously
# considered as numeric are now made factors.
# --------------------------------------------

source("utility.R")
source("definitions.R") # numericFeatures, getSelectedFactorFeatures

data_preprocessing <- function(homesite) {
    ' Preprocess the training data by cleaning de data, asigning data types
    and fixing factor levels so they include the levels observed in the test dataset
    '
    homesite = transformAndClean(homesite)
    homesite = assignDataTypes(homesite)
    homesite = fixFactorLevels(homesite)
    # Target variable
    homesite[,QuoteConversion_Flag:= as.factor(QuoteConversion_Flag)]
    return(homesite)
}


transformAndClean = function(dataTable) {
    ' Remove useless features and transform the dates to numeric values
    '
    dataTable[,Field10:= as.numeric(gsub(",", "", Field10))]
    # Remove quote ID, it is an index variable
    dataTable[,QuoteNumber:=NULL]
    # Remove useless values
    dataTable[,PropertyField6:=NULL]
    dataTable[, GeographicField10A:=NULL]
    dataTable[, PersonalField84:=NULL]
    dataTable[, PropertyField29:=NULL]
    
    # All the following features have near-zero variance according to the caret function nearZeroVar
    dataTable[,SalesField13:=NULL]
    dataTable[,PersonalField23:=NULL]
    dataTable[,PersonalField24:=NULL]
    dataTable[,PersonalField25:=NULL]
    dataTable[,PersonalField26:=NULL]
    dataTable[,PersonalField49:=NULL]
    dataTable[,PersonalField50:=NULL]
    dataTable[,PersonalField51:=NULL]
    dataTable[,PersonalField52:=NULL]
    dataTable[,PersonalField54:=NULL]
    dataTable[,PersonalField55:=NULL]
    dataTable[,PersonalField56:=NULL]
    dataTable[,PersonalField57:=NULL]
    dataTable[,PersonalField66:=NULL]
    dataTable[,PersonalField67:=NULL]
    dataTable[,PersonalField69:=NULL]
    dataTable[,PersonalField70:=NULL]
    dataTable[,PersonalField79:=NULL]
    dataTable[,PersonalField80:=NULL]
    dataTable[,PersonalField81:=NULL]
    dataTable[,PersonalField82:=NULL]
    
        # Extract the day, month and year and remove the original date fields.
    dataTable[,qt := as.Date(as.character(Original_Quote_Date, format = "%Y/%m%/%d"))]
    dataTable[,Original_Quote_Date_Day := as.numeric(format(qt,format="%d"))]
    dataTable[,Original_Quote_Date_Month := as.numeric(format(qt, format="%m"))]
    dataTable[,Original_Quote_Date_Year := as.numeric(format(qt, format="%Y"))]
    dataTable[,Original_Quote_Date:=NULL]
    dataTable[,qt:=NULL]
    return(dataTable)
}

fixFactorLevels = function(homesite) {
    ' Fix the factor levels in the training dataset so they include the levels 
    observed in the test dataset. This involves manually adding the levels

    homesite: The training dataset
    '
    levels(homesite$PropertyField5) = append(levels(homesite$PropertyField5), "")
    levels(homesite$PropertyField2B) = append(levels(homesite$PropertyField2B), "-1")
    levels(homesite$PropertyField7) = append(levels(homesite$PropertyField7), "T")
    levels(homesite$PropertyField23) = c(levels(homesite$PropertyField23), c("12")) 
    levels(homesite$PropertyField27) = c(levels(homesite$PropertyField27), c("5","16", "20")) 
    levels(homesite$PropertyField30) = c(levels(homesite$PropertyField30), c("")) 
    levels(homesite$PropertyField37) = append(levels(homesite$PropertyField37), " ")
    
    levels(homesite$GeographicField7A) = c(levels(homesite$GeographicField7A), c("17")) 
    levels(homesite$GeographicField7B) = c(levels(homesite$GeographicField7B), c("11", "12")) 
    levels(homesite$GeographicField8B) = c(levels(homesite$GeographicField8B), c("12")) 
    # Ensure proper levels for GeographicField10B (Some splits of the data do not have both values)
    levels(homesite$GeographicField10B) = c("-1","25")
    levels(homesite$GeographicField11B) = c(levels(homesite$GeographicField11B), c("14")) 
    levels(homesite$GeographicField12B) = c(levels(homesite$GeographicField12B), c("12", "11")) 
    levels(homesite$GeographicField15B) = c(levels(homesite$GeographicField15B), c("10", "21")) 
    levels(homesite$GeographicField18B) = append(levels(homesite$GeographicField18B), "1")
    levels(homesite$GeographicField24A) = append(levels(homesite$GeographicField24A), "6")
    levels(homesite$GeographicField24B) = append(levels(homesite$GeographicField24B), "19")
    levels(homesite$GeographicField25A) = append(levels(homesite$GeographicField25A), "3")
    levels(homesite$GeographicField25B) = append(levels(homesite$GeographicField25B), "9")
    levels(homesite$GeographicField27A) = c(levels(homesite$GeographicField27A), c("4")) 
    levels(homesite$GeographicField27B) = c(levels(homesite$GeographicField27B), c("13", "15")) 
    levels(homesite$GeographicField47B) = append(levels(homesite$GeographicField47B), "22")
    levels(homesite$GeographicField56B) = append(levels(homesite$GeographicField56B), "1")
    levels(homesite$GeographicField61B) = append(levels(homesite$GeographicField61B), "1")

    levels(homesite$PersonalField17) = c(levels(homesite$PersonalField17), c("XF", "XZ", "YO", "ZJ"))
    levels(homesite$PersonalField16) = c(levels(homesite$PersonalField16), c("XG", "YG", "ZM"))
    levels(homesite$PersonalField18) = c(levels(homesite$PersonalField18), c("XB"))
    levels(homesite$PersonalField19) = c(levels(homesite$PersonalField19), c("ZS"))
    levels(homesite$PersonalField71) = c(levels(homesite$PersonalField71), c("6"))
    
    return(homesite)
}

assignDataTypes = function(homesite) {
    ' Prepare the raw input dataframe with the proper types that we are going to use.
    We need this because R does not assign the types correctly for the variables
    that we consider as factors.
    '
    # Set all the features that we want to use as factors
    factorFeatures = getSelectedFactorFeatures()
    homesite[,(factorFeatures):=lapply(.SD, as.factor),.SDcols=factorFeatures]            
    # Set all the features that we want to use as numeric
    homesite[,(definedNumericFeatures):=lapply(.SD, as.numeric),.SDcols=definedNumericFeatures]            
    return(homesite)
}

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

binaryze_factor_columns <- function(homesite){
    factor_columns <- which(sapply(homesite,is.factor))
    for (column in factor_columns)
    {
        print(names(homesite[column]))
    }
}

compare_test_vs_train_factors = function(homesite, testData) {
    ' Function to compare the differences in labels between train data and
    test data after converting integer values to factors'
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

apply_chi_square_feature_selection = function(homesite, trainingFraction=0.3) {
    ' Apply the chi-square algorithm for dimensionality reduction of categorical
    values. 
    Return a dataframe with the selected features and their importance, sorted
    by importance. 
    '
    loginfo(paste("Applying Chi-Squared feature selection on the categorical variables. Fraction:",trainingFraction))
    factorColumns = get_factor_features(homesite)
    factorColumnNames = names(homesite)[factorColumns]
    factorsDataTable = homesite[, c(factorColumnNames), with=FALSE]
    trainingTable = bernoulli_sampling(factorsDataTable, trainingFraction)
    formula = QuoteConversion_Flag ~ .
    chis = chi.squared(formula, trainingTable)
    # sort by importance
    sortingIndices = order(chis, decreasing = TRUE)
    chiSquaredSorted = data.frame("attr_importance"=chis[sortingIndices,])
    row.names(chiSquaredSorted) = row.names(chis)[sortingIndices]
    return(chiSquaredSorted)
}
