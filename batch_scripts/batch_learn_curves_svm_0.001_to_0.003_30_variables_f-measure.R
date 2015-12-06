# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# batch_learn_curves_svm_0.001_to_0.003_30_variables_f-measure.R
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
#NOTE:  This is a really simple R script which will produce a learning curve. It is designed to 
#       be called in batch (RScript.exe, or similar), along other batches.
#       It will generate the learning curve plot in the visualizations_directory.
# --------------------------------------------

source("svm_model.R")
createLearningCurvesSVM(c(0.001, 0.0015, 0.002, 0.0025, 0.003),30, "F-measure")