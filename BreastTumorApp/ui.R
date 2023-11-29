
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# Define UI for application that draws a histogram
ui<-dashboardPage(skin = "blue",
  
    # Application title
    dashboardHeader(title = "Breast Tumor Charateristics and Modeling of Malignancy",
                    titleWidth=600),

    # Sidebar 
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "about"),
            menuItem("Data Exploration", tabName = "data_exploration"),
            menuItem("Modeling", tabName = "modeling")
        )
    ),
    
    
    # Main Section 
    dashboardBody(
        tabItems(
          
#Tab 1- About
            tabItem("about",
                fluidPage(p("The purpose of this app is to allow the user to investigate the characteristics of cells from breast tumors. The user can investigate how the characteristics relate to each other and how they relate to whether the tumor is benign or malignant. "),
                  br(),
                  a(href = "https://www.kaggle.com/datasets/utkarshx27/breast-cancer-wisconsin-diagnostic-dataset","The dataset entitled  “Breast Cancer Wisconsin Diagnostic Dataset” was downloaded from here."),
                  br(),
                  p("The dataset contains columns related to the radius, perimeter, area, smoothness, compactness, concavity, number of concave point, symmetry and fractal dimension.  For each of these characteristics., the mean, standard error, and “worst” measurement are available.  The dataset also contains a factor denoting whether the mass was malignant or benign."),
                  br(),
                  
                  p("There are three main tabs available:"),
                  p("About:  Describes the characteristics of the data and app."),
                  p("Data Exploration:  Allows the user to make numerical summaries and graphs to investigate how the variables interact with each other and with whether the tumor was benign or malignant."),
                  p("Modelling:  Allows the user to model the data using generalize linear regression or random forest.  "),
                  br(),
                  tags$img(src = "pinkribbon.jpg", width = 160), 
                  tags$img(src = "pinkribbon.jpg", width = 160),
                  tags$img(src = "pinkribbon.jpg", width = 160)
                          ),
                
                ),
#Tab2 Data Exploration
            tabItem("data_exploration",
                tabsetPanel(
                    tabPanel("Numeric Summaries",
                        sidebarPanel(
                           selectInput ("char_of_interest_ns", 
                                        "Select a characteristic for summarization.",
                                        c("Radius", "Texture", "Perimeter", "Area", "Smoothness",
                                          "Compactness", "Concavity", "Concave Points", "Symmetry",
                                          "Fractal Dimension")),
                           selectInput ("dim_of_interest_ns", 
                                        "Select a parameter for summarization",
                                        c("Mean", "Std Error", "Worst")),
                           selectInput("type_of_summary",
                                       "Select a type of summary.", 
                                       c("Min, Median, Max", "Mean and Std Dev")),
                           selectInput("filtering_ns",
                                       "Select whether to filter the data.",
                                       c("Malignant and Benign", "Malignant", "Benign")),
                           conditionalPanel(condition="input.filtering_ns == 'Malignant and Benign'",
                              checkboxInput("grouping",
                                 "Do you want to group by malignant versus benign?")),
                           width = 5),
                        mainPanel(box(tableOutput("numeric_summary")), width=12)
                    ), #End of numeric summary panel
                    tabPanel("Grapical Summaries",fluidPage(h1("datagraph")))
                ),
            ),
            tabItem("modeling",
                tabsetPanel(
                    tabPanel("Modeling Info",fluidPage(h1("model info"))),
                    tabPanel("Model Fitting", fluidPage(h1("model fitting"))),
                    tabPanel("Prediction", fluidPage(h1("model predict")))
                )
                
            )
    )
    
)
)        
