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


