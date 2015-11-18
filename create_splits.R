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
# split_dataset.R - Split a dataset
# --------------------------------------------
require(yaml)
source("utility.R")

conf = yaml.load_file("project.conf")
dataDir = conf$general$data_directory
fnTrain = file.path(dataDir, conf$input$whole_train_data)
dt = load_data(fnTrain)
splitDataset(dt, 0.60, 0.20,
             file.path(dataDir, conf$input$fn_model_training),
             file.path(dataDir, conf$input$fn_model_testing),
             file.path(dataDir, conf$input$fn_model_cross_val))
             