# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# data_preprocessor.R - Data preprocessing functions
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------

data_preprocessing <- function(homesite){
    ' Preprocess a dataset. This function:
    1) Assigns the proper types to the features,
    2) Cleans and transforms some of the features.
    '
    # Set numeric and factors
    homesite = assignDataTypes(homesite)
    homesite = transformAndClean(homesite)
    # Target variable
    homesite[,QuoteConversion_Flag:= as.factor(QuoteConversion_Flag)]
    # impute values    
    m = Mode(homesite[!is.na(PersonalField84), PersonalField84]) # impute the mode 
    homesite[is.na(PersonalField84), PersonalField84:=m] 
    m = Mode(homesite[!is.na(PropertyField29), PropertyField29]) # impute the mode 
    homesite[is.na(PropertyField29), PropertyField29:=m] 
    return(homesite)
}


transformAndClean = function(dataTable) {
    ' Remove useless features and transform the dates to numeric values
    '
    dataTable[,qt := as.Date(as.character(Original_Quote_Date, format = "%Y/%m%/%d"))]
    dataTable[,Original_Quote_Date_Day := as.numeric(format(qt,format="%d"))]
    dataTable[,Original_Quote_Date_Month := as.numeric(format(qt, format="%m"))]
    dataTable[,Original_Quote_Date_Year := as.numeric(format(qt, format="%Y"))]
    # Remove the original date fields after extracting the day, month and year
    dataTable[,Original_Quote_Date:=NULL]
    dataTable[,qt:=NULL]
    # Remove quote ID, it is an index variable
    dataTable[,QuoteNumber:=NULL]
    # Remove useless values
    dataTable[,PropertyField6:=NULL]
    dataTable[, GeographicField10A:=NULL]
    return(dataTable)
}

assignDataTypes = function(homesite) {
    ' Prepare the raw input dataframe with the proper types that we are going to use.
    We need this because R does not assign the types correctly for the variables
    that we consider as factors.
    '
    homesite[,Field10:= as.numeric(gsub(",", "", Field10))]
    
    ############## FACTORS ##############
    # Set all the features that we want to use as factors
    factorFeatures = getSelectedFactorFeatures()
    homesite[,(factorFeatures):=lapply(.SD, as.factor),.SDcols=factorFeatures]            
    
    # Add a level to GeographicField25A    
    levels(homesite$GeographicField25A) = append(levels(homesite$GeographicField25A), 3)
    # Ensure proper levels for GeographicField10B (Some splits of the data do not have both values)
    levels(homesite$GeographicField10B) = c("-1","25")
    
#     levels(homesite$PropertyField37) = append(levels(homesite$PropertyField37), " ")
#     levels(homesite$PropertyField2B) = append(levels(homesite$PropertyField2B), "-1")
#     levels(homesite$PropertyField7) = append(levels(homesite$PropertyField7), "T")
#     levels(homesite$PersonalField17) = c(levels(homesite$PersonalField17), c("XF", "XZ", "YO", "ZJ"))
#     levels(homesite$PersonalField16) = c(levels(homesite$PersonalField16), c("XG", "YG", "ZM"))
#     levels(homesite$PersonalField18) = c(levels(homesite$PersonalField18), c("XB"))
#     levels(homesite$PersonalField19) = c(levels(homesite$PersonalField19), c("ZS"))
#     
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
        "PersonalField82",
        
        "PropertyField23", "PropertyField27"
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
                       "PersonalField73", "PersonalField78", "PersonalField83","PersonalField84",
                       
                       "PropertyField1A", "PropertyField1B", "PropertyField2A", "PropertyField2B", 
                       "PropertyField3",  "PropertyField4", "PropertyField5", "PropertyField7",
                       "PropertyField8", "PropertyField9", "PropertyField10", "PropertyField11A",
                       "PropertyField11B", "PropertyField12", "PropertyField13",  "PropertyField14",
                       "PropertyField15", "PropertyField16A", "PropertyField16B", "PropertyField17",
                       "PropertyField18", "PropertyField19", "PropertyField20", "PropertyField21A",
                       "PropertyField21B", "PropertyField22", "PropertyField26A","PropertyField26B", 
                       "PropertyField28", "PropertyField29" ,"PropertyField30", "PropertyField31",
                       "PropertyField32", "PropertyField33", "PropertyField34", "PropertyField35",
                       "PropertyField36", "PropertyField37", "PropertyField38", "PropertyField39A",
                       "PropertyField39B",
                       
                       "GeographicField1A", "GeographicField1B", "GeographicField2A", "GeographicField2B",
                       "GeographicField3A", "GeographicField3B", "GeographicField4A", "GeographicField4B",
                       "GeographicField5A", "GeographicField5B", "GeographicField6A", "GeographicField6B",
                       "GeographicField7A", "GeographicField7B", "GeographicField8A", "GeographicField8B",
                       "GeographicField9A", "GeographicField9B", "GeographicField10B", "GeographicField11A",
                       "GeographicField11B", "GeographicField12A", "GeographicField12B", "GeographicField13A",
                       "GeographicField14A", "GeographicField14B", "GeographicField15A", "GeographicField15B",
                       "GeographicField16A", "GeographicField16B", "GeographicField17A", "GeographicField17B",
                       "GeographicField18B", "GeographicField19A", "GeographicField19B", "GeographicField20A",
                       "GeographicField20B", "GeographicField21A", "GeographicField21B", "GeographicField22A", "GeographicField25A",
                       "GeographicField25B", "GeographicField26A", "GeographicField26B", "GeographicField27B",
                       "GeographicField28A", "GeographicField28B", "GeographicField29A", "GeographicField29B",
                       "GeographicField30A", "GeographicField31A", "GeographicField32A", "GeographicField32B",
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


