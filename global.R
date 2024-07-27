library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(dplyr)
library(caret)
library(pROC) 
library(DT)
library(plotly)
library(markdown)
library(rsconnect)
rsconnect::setAccountInfo(name='07t44a-sean-berta',
			  token='2B04B0E0B3F06761281FBB1C59D698D9',
			  secret='MHdpNKngrsJdkBIEID+fO1gUidMxWshB0MMoI0GL')




# Source the create_dataset.R file
source("create_dataset.R")
# Call the create_dataset function
hitting_data_agg <- create_dataset()

rsconnect::deployApp("hof_predictor")