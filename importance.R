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

require(yaml)
require(caret) # filterVarImp
require(CORElearn)
source("initializer.R") # standardInit
source("data_processor2.R") # chiSquareImportance


conf = yaml.load_file("project.conf")

standardInit()
dataDir = conf$general$data_directory
fnPreprocessedTrain = file.path(dataDir, conf$input$preprocessed_train)  
loginfo(paste("Loading the preprocessed train dataset:",fnPreprocessedTrain))
load(fnPreprocessedTrain) # loads the data table as the object originalTrainData, but IT IS THE already preprocessed one

# Calculate importance according to the chi-squared test
# chisqImportances = getChiSquaredImportance(originalTrainData, trainingFraction=0.4)
# df = data.frame(FeatureName=rownames(chisqImportances),Importance=chisqImportances$attr_importance)
# write.csv(df, file.path(dataDir, "ChiSquareImportance.csv"))

# Calculate importance of the categorical featuers according to the AUC for each of the features
# factorColumns = get_factor_features(originalTrainData[,-1, with=FALSE]) # -1 means that QuoteConversion_Flag is removed
# factorColumnNames = names(originalTrainData)[factorColumns]
# factorsDataTable = originalTrainData[, c(factorColumnNames), with=FALSE]
# outcomes = originalTrainData$QuoteConversion_Flag
# fvi = filterVarImp(factorsDataTable, outcomes)
# aucImportances = fvi[order(fvi$X0, decreasing = TRUE),]
# df = data.frame(FeatureName=rownames(aucImportances),Importance=aucImportances$X0)
# write.table(df, file.path(dataDir, "AucImportance.csv"), sep=',', row.names=FALSE)

# Calculate importance according to the Relief algorithm
reliefImportance = attrEval(QuoteConversion_Flag ~ ., originalTrainData,
                            estimator="Relief", ReliefIterations=5)
reliefImportance = reliefImportance[order(reliefImportance, decreasing = TRUE)]
df = data.frame(FeatureName=names(reliefImportance),Importance=reliefImportance)
write.table(df, file.path(dataDir, "reliefImportance.csv"), sep=',', row.names=FALSE)


# Calculate importance according to the GainRatio algorithm
gainRatioImportance = attrEval(QuoteConversion_Flag ~ ., originalTrainData, estimator="GainRatio")
gainRatioImportance = gainRatioImportance[order(gainRatioImportance, decreasing = TRUE)]
df = data.frame(FeatureName=names(gainRatioImportance),Importance=gainRatioImportance)
write.table(df, file.path(dataDir, "gainRatioImportance.csv"), sep=',', row.names=FALSE)


# Calculate importance according to the Gini-index measure
giniImportance = attrEval(QuoteConversion_Flag ~ ., originalTrainData, estimator="Gini")
giniImportance = giniImportance[order(giniImportance, decreasing = TRUE)]
df = data.frame(FeatureName=names(giniImportance),Importance=giniImportance)
write.table(df, file.path(dataDir, "giniImportance.csv"), sep=',', row.names=FALSE)


# Calculate importance according to the reliefF importance that decreases exponentially with rank (Manhattan distance)
ReliefFexpRankImportance = attrEval(QuoteConversion_Flag ~ ., originalTrainData, estimator="ReliefFexpRank")
ReliefFexpRankImportance = ReliefFexpRankImportance[order(ReliefFexpRankImportance, decreasing = TRUE)]
df = data.frame(FeatureName=names(ReliefFexpRankImportance),Importance=ReliefFexpRankImportance)
write.table(df, file.path(dataDir, "ReliefFexpRankImportance.csv"), sep=',', row.names=FALSE)
