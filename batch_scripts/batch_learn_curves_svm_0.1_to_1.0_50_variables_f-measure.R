# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# batch_learn_curves_svm_0.1_to_1.0_50_variables_f-measure.R
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
createLearningCurvesSVM(seq(from = 0.1, to = 1.0, by = 0.1),50, "F-measure")
