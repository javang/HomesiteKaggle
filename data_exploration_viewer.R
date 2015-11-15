# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# data_exploration_viewer.R - Custom visualizations for data exploration
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------
sales_fields_visualizations <- function(){
  loginfo("Sales fields visualizations")
  pairs(homesite[,c("SalesField1A","SalesField2A")])
  pairs(homesite[,c("SalesField2A","SalesField2B")])
  pairs(homesite[,c("SalesField10","SalesField11", "SalesField12","SalesField13","SalesField14","SalesField15")])
  
  
  
}