# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# predictions_common.R - Common functions for creating predictions
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------

############
# Hacks to fix Values of variables 
fixVariables <- function(homesiteTestData){
    homesiteTestData[PropertyField37 == " ", PropertyField37:="N"]
    homesiteTestData$PropertyField37 = factor(homesiteTestData$PropertyField37)
    homesiteTestData[PropertyField2B == "-1", PropertyField2B:="2"]
    homesiteTestData$PropertyField2B = factor(homesiteTestData$PropertyField2B)
    homesiteTestData[PropertyField7 == "T", PropertyField7:="S"]
    homesiteTestData$PropertyField7 = factor(homesiteTestData$PropertyField7)
    # impute values    
    m = Mode(homesiteTestData[!is.na(PersonalField84), PersonalField84]) # impute the mode 
    homesiteTestData[is.na(PersonalField84), PersonalField84:=m] 
    
    homesiteTestData[PersonalField17 %in% c("XF", "XZ", "YO", "ZJ"), PersonalField17:="XB"]
    homesiteTestData$PersonalField17 = factor(homesiteTestData$PersonalField17)
    
    homesiteTestData[PersonalField16 %in% c("XG", "YG", "ZM"), PersonalField16:="XB"]
    homesiteTestData$PersonalField16 = factor(homesiteTestData$PersonalField16)
    
    homesiteTestData[PersonalField18 %in% c("XB"), PersonalField18:="XC"]
    homesiteTestData$PersonalField18 = factor(homesiteTestData$PersonalField18)
    
    homesiteTestData[PersonalField19 %in% c("ZS"), PersonalField19:="XB"]
    homesiteTestData$PersonalField19 = factor(homesiteTestData$PersonalField19)
    ############
    return(homesiteTestData)    
}
