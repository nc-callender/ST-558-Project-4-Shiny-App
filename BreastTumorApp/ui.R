
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(shinybusy)

# Define UI for application that draws a histogram
ui<-dashboardPage(skin = "blue",
  
  # Application title
  dashboardHeader(title = "Breast Tumor Charateristics and Modeling of Malignancy", titleWidth = 600),

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
      tabItem("about", fluidPage(
          p("The purpose of this app is to allow the user to investigate the characteristics of cells from breast tumors. The user can investigate how the characteristics relate to each other and how they relate to whether the tumor is benign or malignant. "),
          br(),
          a(href = "https://www.kaggle.com/datasets/utkarshx27/breast-cancer-wisconsin-diagnostic-dataset","The dataset entitled  “Breast Cancer Wisconsin Diagnostic Dataset” was downloaded from here."),
          br(),
          br(),
          p("The dataset contains columns related to the radius, perimeter, area, smoothness, compactness, concavity, number of concave point, symmetry and fractal dimension."),  
          p("For each of these characteristics., the mean, standard error, and “worst” measurement are available."), 
          p("The dataset also contains a factor denoting whether the mass was malignant or benign."),
          br(),
          p("There are three main tabs available:"),
          p("About:  Describes the characteristics of the data and app."),
          p("Data Exploration:  Allows the user to make numerical summaries and graphs to investigate how the variables interact with each other and with whether the tumor was benign or malignant."),
          p("Modelling:  Allows the user to model the data using generalize linear regression or random forest.  The user can then use the model to make predictions."),
          br(),
          tags$img(src = "pinkribbon.jpg", width = 160), 
          tags$img(src = "pinkribbon.jpg", width = 160),
          tags$img(src = "pinkribbon.jpg", width = 160)
          ) #end fluidpage,
      ), #end tabItem: About
      
#Tab2 Data Exploration
      tabItem("data_exploration",
        tabsetPanel(
          tabPanel("Numeric Summaries",
            sidebarPanel(width = 5, 
              selectInput ("char_of_interest_ns", 
                           "Select a characteristic for summarization.",
                            c("Radius", "Texture", "Perimeter", "Area", "Smoothness", "Compactness",
                              "Concavity", "Concave Points", "Symmetry", "Fractal Dimension")),
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
               checkboxInput("grouping", "Do you want to group by malignant versus benign?")),
              ), #end sidebar panel for numeric summary input
            mainPanel(boxwidth = 12, (tableOutput("numeric_summary")))
          ), #End of tab panel: numeric summary 
          
          tabPanel("Grapical Summaries",
            sidebarPanel(
              selectInput("type_of_graph", 
                          "Select a type of graph.",
                           c("Bar Plot", "Histogram", "Scatter Plot", "Box Plot", "Correlation Plot")),

              #Begin conditional panels for histograms
              conditionalPanel(condition = "input.type_of_graph == 'Histogram'",
                selectInput("char_of_interest_hist",
                            "Select a characteristic for graphing.",
                            c("Radius", "Texture", "Perimeter", "Area", "Smoothness", "Compactness",
                              "Concavity", "Concave Points", "Symmetry","Fractal Dimension"))),
              conditionalPanel(condition = "input.type_of_graph == 'Histogram'",
                selectInput("dim_of_interest_hist",
                            "Select a dimension for graphing.",
                            c("Mean", "Std Error", "Worst"))),
              conditionalPanel(condition = "input.type_of_graph == 'Histogram'",
                checkboxInput("grouping_hist",
                              "Do you want to color by malignant versus benign?")),
              conditionalPanel(condition = "input.type_of_graph == 'Histogram'",
                sliderInput("bins_hist",
                            "How  many bins do you want on the histogram?", 
                             min = 5, max = 25, value = 20, step = 1)), 

              #End for histogram conditionals; Begin box plot conditionals
              conditionalPanel(condition = "input.type_of_graph == 'Box Plot'",
                selectInput("char_of_interest_box",
                            "Select a characteristic for graphing.",
                            c("Radius", "Texture", "Perimeter", "Area", "Smoothness","Compactness",
                              "Concavity", "Concave Points", "Symmetry","Fractal Dimension"))),
              conditionalPanel(condition = "input.type_of_graph == 'Box Plot'",
                selectInput("dim_of_interest_box",
                            "Select a dimension for graphing.",
                             c("Mean", "Std Error", "Worst"))),
              conditionalPanel(condition = "input.type_of_graph == 'Box Plot'",
                checkboxInput("grouping_box", "Do you want to color by malignant versus benign?")),

              #End for box plot conditionals; Begin scatter plot conditionals                        
              conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                selectInput("char_of_interest_scatter_x",
                            "Select a characteristic for graphing on the x axis.",
                            c("Radius", "Texture", "Perimeter", "Area", "Smoothness",
                              "Compactness", "Concavity", "Concave Points", "Symmetry","Fractal Dimension"))),
              conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                selectInput("dim_of_interest_scatter_x",
                            "Select a dimension for graphing on the x axis.",
                            c("Mean", "Std Error", "Worst"))),
              conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                 selectInput("char_of_interest_scatter_y",
                             "Select a characteristic for graphing on the y axis.",
                             c("Radius", "Texture", "Perimeter", "Area", "Smoothness",
                             "Compactness", "Concavity", "Concave Points", "Symmetry","Fractal Dimension"),
                             selected = "Area")),
              conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                selectInput("dim_of_interest_scatter_y",
                                                         "Select a dimension for graphing on the y axis.",
                                                         c("Mean", "Std Error", "Worst"))),
              conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                checkboxInput("grouping_scatter", "Do you want to color by malignant versus benign?")),
              conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                checkboxInput("trendline_scatter", "Do you want to a linear regression line?")),

             #End for scatter plot conditionals; Begin corrplot plot conditionals
              conditionalPanel(condition = "input.type_of_graph == 'Correlation Plot'",
                selectInput("filtering_corrplot",
                            "Select whether to filter the data.",
                            c("Malignant and Benign", "Malignant", "Benign"))),
              conditionalPanel(condition = "input.type_of_graph == 'Correlation Plot'",
                selectInput("dim_or_parameter_corrplot",
                            "Compare across parameter or dimension?",
                            c("Parameter", "Dimension"))),
              conditionalPanel(condition = "(input.dim_or_parameter_corrplot == 'Parameter') 
                                          & (input.type_of_graph == 'Correlation Plot')",
                selectInput("dim_corrplot",
                            "Dimension to be used",
                             c("Mean", "Std Error", "Worst"))),
              conditionalPanel(condition = "(input.dim_or_parameter_corrplot == 'Dimension')&
                                                      (input.type_of_graph == 'Correlation Plot')",
                selectInput("parameter_corrplot",
                            "Parameter to be used",
                            c("Radius", "Texture", "Perimeter", "Area", "Smoothness",
                              "Compactness", "Concavity", "Concave Points", "Symmetry","Fractal Dimension"))),
            ), #end sidebarPanel: Graphical summaries tab
            
            mainPanel(box(width = 12, plotlyOutput("graphical_summary")))
          )#end tabPanel:graphical summary
        ), #end tabsetPanel:data exploration
      ), #end tabItem: data exploration

#Tab3 Modelling
      tabItem("modeling",
        tabsetPanel(
          tabPanel("Modeling Info", 
            box(width=12, h1("Generalized Linear Regression"),
              p("Generalized Linear Regression  is a technique that can be used to perform Logistic Regression a on dataset where the response (dependent) variable is binary. The response variable is fit as a logistic sigmoid function of independent variables which can be continuous or binary. The general form of the equation is"),
              withMathJax(helpText("$$ y = \\frac{1}{1+e^{-X}}$$")),
              p("where X is a vector containing all the predictor variables. The range of this function is 0-1, which works well with a binary dependent variable."),
              p("The logistic function is linked to the X vector with the logit function."), br(),
              withMathJax(helpText("$$log\\frac{p}{1-p}=\\beta_0 + \\beta_1x_1 + \\beta_2x_2+ ...+ \\beta_px_p$$")),
            ), #end top box in modelling info tab
            box(width=12, h1("Random Forest"), 
                p("Random forest is a tree-based method of modeling.  It uses bootstrap/ aggregating (bagging) and feature randomness to create an uncorrelated forest of decision trees. It is the feature randomness that distinguishes the random forest model from other types of classification tree."), 
                p("While classification trees are prone to overfitting the data (making it conform too tightly to the training set), that risk is reduced in a random forest model due to the averaging of results from uncorrelated trees. Another advantage of random forest over classification trees is that it is easier to evaluate the importance of different predictor variables.  Disadvantages of random forests are that they models produced are difficult to interpret and computationally expensive."),
                p("The fitting here is performed using the caret package.  The effectiveness of the random forest model relates to the number of random variables that are chosen at each iteration.  Cross validation will be used to optimize that number.  The user can specify the number of data partitions to use in the cross validation and a maximum number of random variables chosen at each iteration. ")
            ) #end bottom box in modelling info tab
          ),#end tabPanel:Modeling info
                    
          #begin tabPanel: Model Fitting
          tabPanel("Model Fitting", fluidRow(column(6,

            #Model Fitting: Generalized Linear Regression Inputs
            box(width = 12, strong("Generalized Linear Regression Input"),
              p("Modelling works best when there is not a large amount of correlation between predictors. For that reason, the user can pick only one of the three size related characteristics: radius, perimeter, and area. For each tumor characteristic below, select the dimension to use in the model."), 
              fluidRow(column(5, 
                pickerInput("size_glm", "Size Characteristic", 
                            c("Radius" = "x.radius", "Area" = "x.area", "Perimeter" = "x.perimeter"), 
                            width = "fit", selected = "x.area"),
                pickerInput("size_dim_glm", "Size", 
                            c("None", "Mean" = "_mean", "Std Error" = "_se","Worst" = "_worst"),
                            width = "fit", selected = "_mean"), 
                pickerInput("texture_dim_glm", "Texture", 
                            c("None" = "+0 ", "Mean" = "+x.texture_mean", 
                              "Std Error" = "+x.texture_se", "Worst" = "+x.texture_worst"),
                            width = "fit", selected = "+x.texture_worst"), 
                pickerInput("smooth_dim_glm", "Smoothness", 
                            c("None" = "+0", "Mean" = "+x.smoothness_mean", 
                              "Std Error" = "+x.smoothness_se","Worst" = "+x.smoothness_worst"),
                            width = "fit", selected = "+x.smoothness_mean"),
                pickerInput("cp_dim_glm", "Concave Points", 
                            c("None" = "+0", "Mean" = "+x.concave_pts_mean", 
                              "Std Error" = "+x.concave_pts_se","Worst" = "+x.concave_pts_worst"),
                            width = "fit", selected = "+x.concave_pts_worst")
                ), #end left column
                column(5,offset = 1, 
                pickerInput("compact_dim_glm", "Compactness", 
                            c("None" = "+0", "Mean" = "+x.compactness_mean", 
                              "Std Error" = "+x.compactness_se", "Worst" = "+x.compactness_worst"), 
                            width = "fit", selected = "+x.compactness_se"),                       
                pickerInput("concave_dim_glm", "Concavity", 
                            c("None" = "+0", "Mean" = "+x.concavity_mean", 
                              "Std Error" = "+x.concavity_se", "Worst" = "+x.concavity_worst"),
                            width = "fit", selected = "+x.concavity_mean"), 
                pickerInput("symm_dim_glm", "Symmetry", 
                            c("None" = "+0", "Mean" = "+x.symmetry_mean", "Std Error" = "+x.symmetry_se", 
                              "Worst" = "+x.symmetry_worst"),
                              width  = "fit", selected = "+x.symmetry_worst"), 
                pickerInput("fd_dim_glm", "Fractal Dimension", 
                            c("None" = "+0", "Mean" = "+x.fractal_dim_mean", 
                              "Std Error" = "+x.fractal_dim_se", "Worst" = "+x.fractal_dim_worst"),
                            width  = "fit", selected = "+x.fractal_dim_mean") 
              ))#end rt column and fluidRow
            ), #end box for generalized linear regression input

            #begin box for random forest inputs
            box(width=12,   strong("Random Forest Input"),
              p("Modelling works best when there is not a large amount of correlation between predictors. For that reason, some characteristics are not being used for modelling. For size, the user can pick between the three related characteristics: radius, perimeter, and area."),
              p("For each tumor characteristic below, select the dimension to use in the model. Also select the number of partitions for cross validation and the number of randomly selected variables to include at each branching of the random forest."),
              fluidRow(column(5,          
                pickerInput("size_rf", "Size Characteristic", 
                            c("Radius" = "x.radius", "Area" = "x.area", "Perimeter" = "x.perimeter"), 
                            width = "fit", selected ="x.area"),
                pickerInput("size_dim_rf", "Size", 
                            c("None", "Mean"=  "_mean", "Std Error" = "_se","Worst" = "_worst"),
                            width = "fit", selected = "_worst"), 
                pickerInput("texture_dim_rf", "Texture", 
                            c("None" = "+0 ", "Mean" = "+x.texture_mean", 
                            "Std Error" = "+x.texture_se", "Worst" = "+x.texture_worst"),
                            width = "fit", selected = "+x.texture_worst"), 
                pickerInput("smooth_dim_rf", "Smoothness", 
                            c("None" = "+0", "Mean" = "+x.smoothness_mean", 
                            "Std Error" = "+x.smoothness_se","Worst" = "+x.smoothness_worst"),
                            width = "fit", selected = "+x.smoothness_mean"),
                pickerInput("cp_dim_rf", "Concave Points", 
                            c("None" = "+0", "Mean" = "+x.concave_pts_mean", 
                            "Std Error" = "+x.concave_pts_se","Worst" = "+x.concave_pts_worst"),
                            width = "fit", selected = "+x.concave_pts_worst")
                ), #end left column
                column(5,offset = 1, 
                pickerInput("compact_dim_rf", "Compactness", 
                            c("None" = "+0", "Mean" = "+x.compactness_mean", 
                            "Std Error" = "+x.compactness_se", "Worst" = "+x.compactness_worst"), 
                            width = "fit", selected = "+x.compactness_se"),                       
                pickerInput("concave_dim_rf", "Concavity", 
                            c("None" = "+0", "Mean" = "+x.concavity_mean", 
                            "Std Error" = "+x.concavity_se", "Worst" = "+x.concavity_worst"),
                            width = "fit", selected = "+x.concavity_mean"), 
                pickerInput("symm_dim_rf", "Symmetry", 
                            c("None" = "+0", "Mean" = "+x.symmetry_mean", 
                            "Std Error" = "+x.symmetry_se", "Worst" = "+x.symmetry_worst"),
                            width = "fit", selected = "+x.symmetry_worst"), 
                pickerInput("fd_dim_rf", "Fractal Dimension", 
                            c("None" = "+0", "Mean" = "+x.fractal_dim_mean", "Std Error" = "+x.fractal_dim_se", 
                            "Worst "= "+x.fractal_dim_worst"),
                            width = "fit", selected = "+x.fractal_dim_mean")#end rt column
              )),#end rt column end row 
              fluidRow(column(5,
                sliderInput("cv_number", "How many partitions for cross validation?", 
                            min = 2, max = 10, value = 5)), # end lt column
                column(5, offset = 1,
                sliderInput("mtry", "How many randomly selected variables introduced at each iteration?", 
                            min = 1, max = 4, value = 3)))#end rt column, end fluid row
            ), #end box for RF input,
            ),#end lt column (containing the input boxes for glm and rf)
            
            #begin rt column containing model outputs
            column(6,add_busy_spinner(spin = "fading-circle"),box(width = 12,
            box(width=12, strong("Split the Data Set"),
              p("The data set needs splitting into two parts, one for training and one for testing."),
              sliderInput("data_split", "What percent of the data do you want to use for training?", 
                          min = 50, max = 95, value = 70, step = 5),
              strong("Compare Model Input"),
              p("Select which model (or both) to use."),
              pickerInput("model_to_use", "Model to Use", 
                          c("Generalized Linear Regression", "Random Forest", "Both"),
                          width = "fit", selected = "Generalized Linear Regression"),
              actionButton("model_now", "Perform Modelling Now!", class= "btn-success")
            ),#end top box
            br(),
            box(width = 12, 

              h3("Generalized Linear Regression Model"),
              verbatimTextOutput("glm_summary"),  
              tags$span(style = "color:blue;", strong(textOutput("glm_test_output"))),
              tags$span(style = "color:purple;", strong(textOutput("glm_test_acc"))),

              h3("Random Forest Model"),
              tags$span(style = "color:blue;", strong(textOutput("rf_not_performed"))),
              plotOutput("rf_var_imp"), 
              tags$span(style = "color:blue;", strong(textOutput("rf_mtry"))), 
              tags$span(style = "color:blue;", strong(textOutput("rf_test_output"))),      
              tags$span(style = "color:purple;", strong(textOutput("rf_test_acc"))))
            ), #end box for modelling output
            )#end rt column
          )#end fluid row
          ), #end tabPanel Modelling
          
          ##begin tabPanel Prediction
          tabPanel("Prediction",fluidRow(
            column(12, box(width=12, h4("Selecting Values for Predictors"), 
              fluidRow(
                column(2, numericInput("radius_mean", "Radius: mean? (5-30)", 
                                       min = 5, max = 30, value = 14)),
                column(2, numericInput("radius_se", "Radius: std error? (0-3)", 
                                       min = 0, max = 3, value = 0.4)),
                column(2, numericInput("radius_worst", "Radius: worst? (5-40)",
                                       min = 5, max = 40, value = 17)),
                column(2, numericInput("texture_mean", "Texture: mean? (10-40)", 
                                       min = 10, max = 40, value = 19)),
                column(2, numericInput("texture_se", "Texture: std error? (0-5)", 
                                       min = 0, max = 5, value = 1.2)),
                column(2, numericInput("texture_worst", "Texture: worst? (10-50)", 
                                       min = 10, max = 50, value = 26))),
            fluidRow(
                column(2, numericInput("perimeter_mean", "Perimeter: mean? (40-200)", 
                                       min = 40, max = 200, value = 92)),
                column(2, numericInput("perimeter_se", "Perimeter: std error? (0-30)", 
                                      min = 0, max = 30, value = 3)),
                column(2, numericInput("perimeter_worst", "Perimeter: worst? (40-300)", 
                                      min = 40, max = 300, value = 110)),
                column(2, numericInput("area_mean", "Area: mean? (100-2600)", 
                                       min = 100, max = 2600, value = 655)),
                column(2, numericInput("area_se", "Area: std error? (0-600)", 
                                       min = 0, max = 600, value = 40)),
                column(2, numericInput("area_worst", "Area: worst? (150-5000)", 
                                       min = 150, max = 5000, value = 890))),
            fluidRow(
                column(2, numericInput("smoothness_mean", "Smoothness: mean? (0-0.5)", 
                                       min = 0, max = 0.5, value = 0.096)),
                column(2, numericInput("smoothness_se", "Smoothness: std error? (0-0.05)", 
                                       min = 0, max = 0.05, value = 0.007)),
                column(2, numericInput("smoothness_worst", "Smoothness: worst? (0-1.5)", 
                                       min = 0, max = 1.5, value = 0.13)),
                column(2, numericInput("compactness_mean", "Compactness: mean? (0-0.5)", 
                                       min = 0, max = 0.5, value = 0.104)),
                column(2, numericInput("compactness_se", "Compactness: std error? (0-0.15)", 
                                       min = 0, max = 0.15, value = 0.025)),
                column(2, numericInput("compactness_worst", "Compactness: worst? (0-1.5)", 
                                       min = 0, max = 1.5, value = 0.250))),
              fluidRow(
                column(2, numericInput("concavity_mean", "Concavity: mean? (0-0.5)", 
                                       min = 0, max = 0.5, value = 0.089)),
                column(2, numericInput("concavity_se", "Concavity: std error? (0-0.4)",
                                       min = 0, max = 0.4, value = 0.032)),
                column(2, numericInput("concavity_worst", "Concavity: worst? (0-1.3)", 
                                       min = 0, max = 1.3, value = 0.27)),
                column(2, numericInput("symmetry_mean", "Symmetry: mean? (0-0.5)", 
                                       min = 0, max = 0.5, value = 0.181)),
                column(2, numericInput("symmetry_se", "Symmetry: std error? (0-0.1)", 
                                       min = 0, max = 0.1, value = 0.021)),
                column(2, numericInput("symmetry_worst", "Symmetry: worst? (0-0.8)", 
                                       min = 0, max = 0.8, value = 0.29))),
              fluidRow(
                column(2, numericInput("cp_mean", HTML("Concave Points: mean? <br/>(0-0.3)"), 
                                       min = 0, max = 0.3, value = 0.049)),
                column(2, numericInput("cp_se", HTML("Concave Points: std error?<br/> (0-0.1)"), 
                                       min = 0, max = 0.1, value = 0.012)),
                column(2, numericInput("cp_worst", HTML("Concave Points: worst? <br/>(0-0.3)"), 
                                       min = 0, max = 0.3, value = 0.115)),
                column(2, numericInput("fd_mean", HTML("Fractal Dimension: mean? <br/>(0-0.1)"), 
                                       min = 0, max = 0.1, value = 0.063)),
                column(2, numericInput("fd_se", HTML("Fractal Dimension: std error? <br/>(0-0.1)"), 
                                       min = 0, max = 0.1, value = 0.004)),
                column(2, numericInput("fd_worst", HTML("Fractal Dimension: worst? <br/>(0-0.3)"), 
                                       min = 0, max = 0.3, value = 0.084))
              ), #end fluidRow
                actionButton("predict_now", "Obtain Prediction Now!", class= "btn-success")
            )# end box (top box on predictions page)
            )),#end column, end fluidRow

            # Box with information about models available for prediction
            box(width = 12,background = "light-blue",
              h4("Model Information"),
              "Current model(s) is/are:",
              textOutput("methods_for_prediction"), 
              br(), 
              textOutput("glm_variables1"),
              strong(textOutput("glm_variables2")),
              br(), 
              textOutput("rf_variables1"),
              strong(textOutput("rf_variables2"))
            ), #end box for model information

            # Box for prediction ouput with color coding
            box(width = 12, background = "navy", 
              h4("Prediction"), 
              verbatimTextOutput(("test_output1")),
              verbatimTextOutput(("test_output2")),
              verbatimTextOutput(("test_output")),
              tags$span(style="color:green;", strong(textOutput("glm_predict_benign"))),
              tags$span(style="color:red;", strong(textOutput("glm_predict_malignant"))),
              tags$span(style="color:green;", strong(textOutput("rf_predict_benign"))),
              tags$span(style="color:red;", strong(textOutput("rf_predict_malignant")))
              ),#end box for prediction output
          )#end tabPanel for Prediction
        )#end tabsetpanel for modelling
      )#end tabItem-modelling
    ) #end tabitems
  ) #end dashboard body
) #end dashboard page
