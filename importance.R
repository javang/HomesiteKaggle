# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# importance.R - Run this script to determine the importance of the categorical
# features.
# The script will:
#    - Read the preprocessed data after running preprocess.R
#    - Determine the importance of the variables for predicting the HomesiteQuote
#      with the following algorithms
#          *
#          *
#          *
#          *
#          *
#          *


# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# THIS SCRIPT IS RUN IN STANDALONE MODE TO INFER IMPORTANCE OF THE CATEGORICAL FEATRURES.

# --------------------------------------------

source("initializer.R") # standardInit
source("data_processor2.R") # chiSquareImportance
require(yaml)

conf = yaml.load_file("project.conf")

standardInit()
dataDir = conf$general$data_directory
fnPreprocessedTrain = file.path(dataDir, conf$input$preprocessed_train)  
loginfo(paste("Loading the preprocessed train dataset:",fnPreprocessedTrain))
load(fnPreprocessedTrain) # loads the data table originalTrainData, but IT IS THE preprocessed one

# Calculate importance according to the chi-squared test
chisqImportances = getChiSquaredImportance(originalTrainData, trainingFraction=0.01)



