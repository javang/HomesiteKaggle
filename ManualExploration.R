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

# --------------------------------------------


require(ggplot2)


plot(homesite[,.(CoverageField1A, CoverageField2A, CoverageField3A, CoverageField4A, CoverageField11A)])
plot(homesite[,.(CoverageField1B, CoverageField2B, CoverageField3B, CoverageField4B, CoverageField11B)])

plot(homesite[,.(QuoteConversion_Flag, Field6, Field7, Field8, Field9, Field10,Field11, Field12)])

ggplot(homesite) +
    geom_points(aes())