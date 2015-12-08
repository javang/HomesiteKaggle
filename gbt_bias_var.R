# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# Notes: 
# THIS SCRIPT IS RUN IN STANDALONE MODE TO CREATE THE BIAS VARIANCE CURVES
# FOR THE GBT MODEL
# --------------------------------------------

source("data_processor2.R")
source("bias_variance.R")
require(yaml)
conf = yaml.load_file("project.conf")

dataDir = conf$general$data_directory
load(file.path(dataDir, conf$input$fn_reduced_training)) 
load(file.path(dataDir, conf$input$fn_reduced_testing)) 
createGbtBiasVariancePlot(modelTrainData, modelTestData, 
                                     data_point_fraction=0.50, 
                                     number_of_features_list=c(30, 50, 80,100,150,200,250,275)) 


