# --------------------------------------------
# University of Washington
# Data Science Certificate Program
# Deriving Knowledge from Data at Scale - DATASCI 450
#
# Final Project:
# Response to Homesite Quote Conversion Kaggle Challenge  
#
# initialization.R - Initialization Functions
#
# Project participants:
# Javier Velázquez
# Marciano Moreno
# 
# --------------------------------------------

# --------------------------------------------

require(yaml)
conf = yaml.load_file("project.conf")
# Environment variables:
interactive_session <- interactive()

# Required libraries:
if(!require("logging")){install.packages("logging")}
if(!require("FactoMineR")){install.packages("FactoMineR")}
if(!require("MASS")){install.packages("MASS")}
if(!require("FSelector")){install.packages("FSelector")}

#if(!require("gdata")){install.packages("gdata")}
#if(!require("Amelia")){install.packages("Amelia")}
#if(!require("plyr")){install.packages("plyr")}
#if(!require("dplyr")){install.packages("dplyr")}
#if(!require("maps")){install.packages("maps")}
#if(!require("RColorBrewer")){install.packages("RColorBrewer")}
#if(!require("ggplot2")){install.packages("ggplot2")}
#if(!require("data.table")){install.packages("data.table")}
#if(!require("Hmisc")){install.packages("Hmisc")}
#if(!(require("lattice"))){install.packages("lattice")}



# --------------------------------------------
# Function declarations [Initialization]:

initialize_program <- function(program_name, working_directory, log_file, output_logs_to_console, interactive_session)
{
    basicConfig(level = 'INFO')
    addHandler(writeToFile, level = "INFO", file = log_file)
    if (output_logs_to_console){
        addHandler(writeToConsole, level = "INFO")
    }
  
  loginfo(paste("*********************", program_name, "************************"))
  
  if (interactive_session){
    loginfo("Interactive session.")
  }else{
    loginfo("Batch session.")
  }
}


standardInit = function() {
    ' Perform the program inizialization. It mostly sets up the logging'
    program_name = conf$general$program_name
    working_directory = conf$general$data_directory
    log_file = conf$general$log_file
    output_logs_to_console = conf$general$output_logs_to_console
    DEBUG = conf$general$DEBUG
    initialize_program(program_name, working_directory, 
                       log_file, output_logs_to_console, interactive_session)
}
