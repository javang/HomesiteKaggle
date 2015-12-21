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
require(caret)
require(yaml)
source("utility.R")
conf = yaml.load_file("project.conf")

data_preprocessing <- function(homesite) {
    ' Preprocess the training data by cleaning de data, asigning data types
    and fixing factor levels so they include the levels observed in the test dataset
    '
    homesite = transformAndClean(homesite)
    homesite = assignDataTypes(homesite)
    # Target variable
    homesite[,QuoteConversion_Flag:= as.factor(QuoteConversion_Flag)]
    homesite = removeNearZeroValueNumericColumns(homesite)
    homesite = preProcessNumericColumns(homesite) #BoxCox, Center, Scale. Saves the tranformations for reuse.
    homesite = fixFactorLevels(homesite)

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
    ############## FACTORS ##############
    # Set all the features that we want to use as factors
    factorFeatures = getSelectedFactorFeatures()
    homesite[,(factorFeatures):=lapply(.SD, as.factor),.SDcols=factorFeatures]            
    
    ############## NUMERIC ##############
    numericFeatures = c(
        "SalesField8" , "SalesField10", "SalesField11", "SalesField12", 
        "SalesField13", "SalesField14", "SalesField15",
        
        "PersonalField5", "PersonalField14", "PersonalField23", "PersonalField24",  
        "PersonalField25", "PersonalField26", "PersonalField27", "PersonalField30",
        "PersonalField31", "PersonalField32", "PersonalField33", "PersonalField44",
        "PersonalField45", "PersonalField46", "PersonalField47", "PersonalField49",
        "PersonalField50", "PersonalField51", "PersonalField52", "PersonalField54",
        "PersonalField55", "PersonalField56", "PersonalField57", 
        "PersonalField66", "PersonalField67", "PersonalField69", "PersonalField70",
        "PersonalField74", "PersonalField75", "PersonalField76", "PersonalField77",
        "PersonalField79", "PersonalField80", "PersonalField81",
        "PersonalField82"
        )
    homesite[,(numericFeatures):=lapply(.SD, as.numeric),.SDcols=numericFeatures]            
    return(homesite)
}

getSelectedFactorFeatures = function() {
    ' The names of the features that we have considered as factors
    '
    factorFeatures = c("Field6",  "Field7", "Field12",
                       "CoverageField1A", "CoverageField1B", "CoverageField2A", "CoverageField2B",
                       "CoverageField3A", "CoverageField3B", "CoverageField4A", "CoverageField4B",
                       "CoverageField5A", "CoverageField5B", "CoverageField6A", "CoverageField6B",
                       "CoverageField8", "CoverageField9", "CoverageField11A", "CoverageField11B",
                       
                       "SalesField1A", "SalesField1B", "SalesField2A", "SalesField2B",
                       "SalesField3" , "SalesField4" , "SalesField5" , "SalesField6" ,
                       "SalesField7" , "SalesField9",
                       
                       "PersonalField1" , "PersonalField2" , "PersonalField4A", "PersonalField4B",
                       "PersonalField6" , "PersonalField7" , "PersonalField8" , "PersonalField9",
                       "PersonalField10A", "PersonalField10B" , "PersonalField11" , "PersonalField12",
                       "PersonalField13", "PersonalField15", "PersonalField16", "PersonalField17", 
                       "PersonalField18", "PersonalField19", "PersonalField22", "PersonalField28",
                       "PersonalField29", "PersonalField34", "PersonalField35", "PersonalField36",
                       "PersonalField37", "PersonalField38", "PersonalField39", "PersonalField40",
                       "PersonalField41", "PersonalField42", "PersonalField43", "PersonalField48",
                       "PersonalField53", "PersonalField58", "PersonalField59", "PersonalField60",
                       "PersonalField61", "PersonalField62", "PersonalField63", "PersonalField64",
                       "PersonalField65", "PersonalField68", "PersonalField71", "PersonalField72",
                       "PersonalField73", "PersonalField78", "PersonalField83",
                       
                       "PropertyField1A", "PropertyField1B", "PropertyField2A", "PropertyField2B", 
                       "PropertyField3",  "PropertyField4", "PropertyField5", "PropertyField7",
                       "PropertyField8", "PropertyField9", "PropertyField10", "PropertyField11A",
                       "PropertyField11B", "PropertyField12", "PropertyField13",  "PropertyField14",
                       "PropertyField15", "PropertyField16A", "PropertyField16B", "PropertyField17",
                       "PropertyField18", "PropertyField19", "PropertyField20", "PropertyField21A",
                       "PropertyField21B", "PropertyField22",  "PropertyField23",  "PropertyField24A", "PropertyField24B",
                       "PropertyField26A","PropertyField26B", "PropertyField27",
                       "PropertyField28","PropertyField30", "PropertyField31",
                       "PropertyField32", "PropertyField33", "PropertyField34", "PropertyField35",
                       "PropertyField36", "PropertyField37", "PropertyField38", "PropertyField39A",
                       "PropertyField39B",
                       
                       "GeographicField1A", "GeographicField1B", "GeographicField2A", "GeographicField2B",
                       "GeographicField3A", "GeographicField3B", "GeographicField4A", "GeographicField4B",
                       "GeographicField5A", "GeographicField5B", "GeographicField6A", "GeographicField6B",
                       "GeographicField7A", "GeographicField7B", "GeographicField8A", "GeographicField8B",
                       "GeographicField9A", "GeographicField9B", "GeographicField10B", "GeographicField11A",
                       "GeographicField11B", "GeographicField12A", "GeographicField12B", "GeographicField13A", "GeographicField13B",
                       "GeographicField14A", "GeographicField14B", "GeographicField15A", "GeographicField15B",
                       "GeographicField16A", "GeographicField16B", "GeographicField17A", "GeographicField17B",
                       "GeographicField18A", "GeographicField18B", "GeographicField19A", "GeographicField19B", "GeographicField20A",
                       "GeographicField20B", "GeographicField21A", "GeographicField21B", "GeographicField22A", 
                       "GeographicField22B", "GeographicField23A", "GeographicField23B", "GeographicField24A", "GeographicField24B",
                       "GeographicField25A", "GeographicField25B", "GeographicField26A", "GeographicField26B", 
                       "GeographicField27A", "GeographicField27B",
                       "GeographicField28A", "GeographicField28B", "GeographicField29A", "GeographicField29B",
                       "GeographicField30A", "GeographicField30B", "GeographicField31A", "GeographicField31B", 
                       "GeographicField32A", "GeographicField32B",
                       "GeographicField33A", "GeographicField33B", "GeographicField34A", "GeographicField34B",
                       "GeographicField35A", "GeographicField35B", "GeographicField36A", "GeographicField36B",
                       "GeographicField37A", "GeographicField37B", "GeographicField38A", "GeographicField38B",
                       "GeographicField39A", "GeographicField39B", "GeographicField40A", "GeographicField40B",
                       "GeographicField41A", "GeographicField41B", "GeographicField42A", "GeographicField42B",
                       "GeographicField43A", "GeographicField43B", "GeographicField44A", "GeographicField44B",
                       "GeographicField45A", "GeographicField45B", "GeographicField46A", "GeographicField46B",
                       "GeographicField47A", "GeographicField47B", "GeographicField48A", "GeographicField48B",
                       "GeographicField49A", "GeographicField49B", "GeographicField50A", "GeographicField50B",
                       "GeographicField51A", "GeographicField51B", "GeographicField52A", "GeographicField52B",
                       "GeographicField53A", "GeographicField53B", "GeographicField54A", "GeographicField54B",
                       "GeographicField55A", "GeographicField55B", "GeographicField56A", "GeographicField56B",
                       "GeographicField57A", "GeographicField57B", "GeographicField58A", "GeographicField58B",
                       "GeographicField59A", "GeographicField59B", "GeographicField60A", "GeographicField60B",
                       "GeographicField61A", "GeographicField61B", "GeographicField62A", "GeographicField62B",
                       "GeographicField63", "GeographicField64"
    )
    return(factorFeatures)
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

removeNearZeroValueNumericColumns <- function(homesite){
    numFeatures <- get_numeric_features(homesite)
    nzv_result <- as.data.table(nearZeroVar(homesite, saveMetrics = TRUE),keep.rownames = TRUE)
    nzv_features <- nzv_result[nzv == TRUE & rn %in% names(numFeatures),rn]
    #The following fields shall be removed from the dataset as they have near zero variance:
    #     [1] "SalesField13"    "PersonalField23" "PersonalField24" "PersonalField25" "PersonalField26" "PersonalField49"
    #     [7] "PersonalField50" "PersonalField51" "PersonalField52" "PersonalField54" "PersonalField55" "PersonalField56"
    #     [13] "PersonalField57" "PersonalField66" "PersonalField67" "PersonalField69" "PersonalField70" "PersonalField79"
    #     [19] "PersonalField80" "PersonalField81" "PersonalField82"
    homesite[,(nzv_features):=NULL]
    return(homesite)
}

preProcessNumericColumns <- function(homesite){
    numFeatures <- get_numeric_features(homesite)
    preproc_result <- preProcess(homesite[,names(numFeatures),with=FALSE], method = c("BoxCox", "center", "scale"))
    newds <- as.data.table(predict(preproc_result, homesite[,names(numFeatures), with = FALSE]))
    homesite <- homesite[,(numFeatures):=NULL]
    homesite <- cbind(homesite$QuoteConversion_Flag, newds, homesite[,-1,with=FALSE])
    setnames(homesite, "V1", "QuoteConversion_Flag")
    preprocDir <- conf$preprocessing$directory
    fnPreProcResult <- file.path(preprocDir, conf$preprocessing$fn_results)
    save(preproc_result, file = fnPreProcResult)
    return(homesite)
}
