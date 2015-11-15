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
