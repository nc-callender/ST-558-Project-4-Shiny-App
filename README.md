# ST558 Final Project: Application for Diagnosing Breast Cancer Tumors
## Description and Purpose of App
This applications analyzes data from breast masses and how they affect diagnosis.
The  dataset  is available on kaggle  and is commonly used for training machine learning algorithms.  The data set contains  data from digital images of 569 breast masses.  The tumor features are: 

Radius  
Area  
Perimeter   
Texture  
Smoothness  
Compactness  
Concavity  
Concave Points  
Symmetry  
Fractal Dimension  

For each feature, the dataset contains three dimensions:  

Mean  
Standard Deviation  
Worst  

And lastly, the dataset contains the classification for each mass, whether it was benign or malignant.

Link for dataset:
https://www.kaggle.com/datasets/utkarshx27/breast-cancer-wisconsin-diagnostic-dataset

This app allow the user to perform data exploration, making numerical and graphical summaries.  This allows the user to explore which parameter/dimension combination differs most between benign and malignant tumors.  It also allows the user to explore correlation between the parameters and between the dimensions for each parameter.
The app allows the user to model the data with a generalized  linear regression model or a random forest model.  It also allows the user to use than model to make predictions on a set of tumor characteristics.

##  R Packages Used

shiny  
shinydashboard  
shinyWidgets  
shinybusy  
DT  
plotly  
tidyverse  
corrplot  
ggcorrplot  
caret  
randomForest  


## Code to Install Packages
install.packages(c( “shiny”, “shinydashboard”, “shinyWidgets”, ”shinybusy”, ”DT”, ”plotly”, ”tidyverse”, ”corrplot”, ”ggcorrplot”, ”caret”, ”randomForest”)

## Code to load the packages

library(shiny)  
library(shinydashboard)  
library(shinyWidgets)  
library(shinybusy)  
library(DT)  
library(plotly)  
library(tidyverse)  
library(corrplot)  
library(ggcorrplot)  
library(caret)  
library(randomForest)  

## Directions for Running App
First make sure all packages are installed and loaded.  

Then use this code:
shiny::runGitHub(repo= "ST-558-Project4-Shiny-App", 
username = "nc-callender",
ref="main",
subdir = "BreastTumorApp")

