
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(shinybusy)

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
                    tabPanel("Grapical Summaries",
                        sidebarPanel(
                            selectInput("type_of_graph", 
                                "Select a type of graph.",
                                 c("Bar Plot", "Histogram", "Scatter Plot", "Box Plot", "Correlation Plot"),
                                 ),
                            conditionalPanel(condition = "input.type_of_graph == 'Histogram'",
                                selectInput("char_of_interest_hist",
                                "Select a characteristic for graphing.",
                                c("Radius", "Texture", "Perimeter", "Area", "Smoothness",
                                  "Compactness", "Concavity", "Concave Points", "Symmetry",
                                  "Fractal Dimension"))),
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
                                                         c("Radius", "Texture", "Perimeter", "Area", "Smoothness",
                                                           "Compactness", "Concavity", "Concave Points", "Symmetry",
                                                           "Fractal Dimension"))),
                            conditionalPanel(condition = "input.type_of_graph == 'Box Plot'",
                                             selectInput("dim_of_interest_box",
                                                         "Select a dimension for graphing.",
                                                         c("Mean", "Std Error", "Worst"))),
                            conditionalPanel(condition = "input.type_of_graph == 'Box Plot'",
                                             checkboxInput("grouping_box",
                                                           "Do you want to color by malignant versus benign?")),
                        #End for box plot conditionals; Begin scatter plot conditionals                        
                            conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                                             selectInput("char_of_interest_scatter_x",
                                                         "Select a characteristic for graphing on the x axis.",
                                                         c("Radius", "Texture", "Perimeter", "Area", "Smoothness",
                                                           "Compactness", "Concavity", "Concave Points", "Symmetry",
                                                           "Fractal Dimension"))),
                            conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                                             selectInput("dim_of_interest_scatter_x",
                                                         "Select a dimension for graphing on the x axis.",
                                                     c("Mean", "Std Error", "Worst"))),
                            conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                                             selectInput("char_of_interest_scatter_y",
                                                         "Select a characteristic for graphing on the y axis.",
                                                         c("Radius", "Texture", "Perimeter", "Area", "Smoothness",
                                                           "Compactness", "Concavity", "Concave Points", "Symmetry",
                                                           "Fractal Dimension"),
                                                         selected= "Area")),
                            conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                                             selectInput("dim_of_interest_scatter_y",
                                                         "Select a dimension for graphing on the y axis.",
                                                         c("Mean", "Std Error", "Worst"))),
                            conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                                             checkboxInput("grouping_scatter",
                                                           "Do you want to color by malignant versus benign?")),
                            conditionalPanel(condition = "input.type_of_graph == 'Scatter Plot'",
                                             checkboxInput("trendline_scatter",
                                                           "Do you want to a linear regression line?")),
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
                                                       "Compactness", "Concavity", "Concave Points", "Symmetry",
                                                       "Fractal Dimension"))),
), #end tabPanel Graphical summaries
                        mainPanel(box(width = 12, plotlyOutput("graphical_summary"))))
                ), #end tabset panel for data exploration
            ), #end tabItem- data exploration
            tabItem("modeling",
                tabsetPanel(
                    tabPanel("Modeling Info",box(width=12, h1("Generalized Linear Regression"),
                            p("Generalized Linear Regression  is a technique that can be used to perform Logistic Regression a on dataset where the response (dependent) variable is binary. The response variable is fit as a logistic sigmoid function of independent variables which can be continuous or binary. The general form of the equation is"),
                            withMathJax(helpText("$$ y = \\frac{1}{1+e^{-X}}$$")),
                            p("where X is a vector containing all the predictor variables. The range of this function is 0-1, which works well with a binary dependent variable."),
p("The logistic function is linked to the X vector with the logit function."), br(),
                            withMathJax(helpText("$$log\\frac{p}{1-p}=\\beta_0 + \\beta_1x_1 + \\beta_2x_2+ ...+ \\beta_px_p$$")),
), #end box1
                             box(width=12, h1("Random Forest"), p("Random forest is a tree-based method of modeling.  It uses bootstrap/ aggregating (bagging) and feature randomness to create an uncorrelated forest of decision trees. It is the feature randomness that distinguishes the random forest model from other types of classification tree."), 
                               p("While classification trees are prone to overfitting the data (making it conform too tightly to the training set), that risk is reduced in a random forest model due to the averaging of results from uncorrelated trees. Another advantage of random forest over classification trees is that it is easier to evaluate the importance of different predictor variables.  Disadvantages of random forests are that they models produced are difficult to interpret and computationally expensive."),
                               p("The fitting here is performed using the caret package.  The effectiveness of the random forest model relates to the number of random variables that are chosen at each iteration.  Cross validation will be used to optimize that number.  The user can specify the number of data partitions to use in the cross validation and a maximum number of random variables chosen at each iteration. ")
                               
                                                                  ) #end box2
                             )#end tabPanel-modeling info
                    ,
                    tabPanel("Model Fitting", fluidRow(column(6,                         box(width=12, strong("Generalized Linear Regression Input"),
                                                                                             p("Modelling works best when there is not a large amount of correlation between predictors. For that reason, the user can pick only one of the three related characteristics: radius, perimeter, and area. For each tumor characteristic below, select the dimension to use in the model."),
fluidRow(
    column(5, 
         pickerInput("size_glm", "Size Characteristic", 
                     c("Radius" ="x.radius", "Area"="x.area", "Perimeter"="x.perimeter"), 
                     width = "fit", selected ="x.area"),
         pickerInput("size_dim_glm", "Size", c("None", "Mean"="_mean", "Std Error"="_se","Worst"="_worst"),
                    width = "fit", selected = "_worst"), 
         pickerInput("texture_dim_glm", "Texture", c("None"="+0 ", "Mean"="+x.texture_mean", 
                                                    "Std Error" = "+x.texture_se", "Worst" = "+x.texture_worst"),
                   width = "fit", selected = "+x.texture_worst"), 
        pickerInput("smooth_dim_glm", "Smoothness", c("None"="+0", "Mean"="+x.smoothness_mean", 
                                                      "Std Error"="+x.smoothness_se","Worst"="+x.smoothness_worst"),
                    width = "fit", selected = "+x.smoothness_mean"),
        pickerInput("cp_dim_glm", "Concave Points", c("None"="+0", "Mean"="+x.concave_pts_mean", 
                                                      "Std Error"="+x.concave_pts_se","Worst"="+x.concave_pts_worst"),
                    width = "fit", selected = "+x.concave_pts_worst")
        ), #end left column
    column(5,offset = 1, 
           pickerInput("compact_dim_glm", "Compactness", c("None"="+0", "Mean"="+x.compactness_mean", 
                                                           "Std Error"="+x.compactness_se", 
                                                           "Worst"="+x.compactness_worst"), 
                    width = "fit", selected = "+x.compactness_worst"),                       
           pickerInput("concave_dim_glm", "Concavity", c("None"="+0", "Mean"="+x.concavity_mean", 
                                                        "Std Error"="+x.concavity_se", "Worst"="+x.concavity_worst"),
                     width = "fit", selected = "+x.concavity_mean"), 
                                                                                                             pickerInput("symm_dim_glm", "Symmetry", c("None"="+0", "Mean"="+x.symmetry_mean", "Std Error"="+x.symmetry_se", "Worst"="+x.symmetry_worst"),
                                                                                                                         width  ="fit", selected = "+x.symmetry_worst"), 
      pickerInput("fd_dim_glm", "Fractal Dimension", c("None"="+0", "Mean"="+x.fractal_dim_mean", "Std Error"="+x.fractal_dim_se", "Worst"="+x.fractal_dim_worst"),
                       width  ="fit", selected = "+x.fractal_dim_mean") 
           )#end rt column
                                                                                             ),#end row 
                    ), #end box for GLM input
                    box(width=12, strong("Random Forest Input"),
                        p("Modelling works best when there is not a large amount of correlation between predictors. For that reason, some characteristics are not being used for modelling. For size, the user can pick between the three related characteristics: radius, perimeter, and area."),
                        p("For each tumor characteristic below, select the dimension to use in the model. Also select the number of partitions for cross validation and the number of randomly selected variables to include at each branching of the random forest."),
                        fluidRow(column(5,          pickerInput("size_rf", "Size Characteristic", 
                                                                c("Radius" ="x.radius", "Area"="x.area", "Perimeter"="x.perimeter"), 
                                                                width = "fit", selected ="x.area"),
                                        pickerInput("size_dim_rf", "Size", c("None", "Mean"="_mean", "Std Error"="_se","Worst"="_worst"),
                                                    width = "fit", selected = "_worst"), 
                                        pickerInput("texture_dim_rf", "Texture", c("None"="+0 ", "Mean"="+x.texture_mean", 
                                                                                    "Std Error" = "+x.texture_se", "Worst" = "+x.texture_worst"),
                                                    width = "fit", selected = "+x.texture_worst"), 
                                        pickerInput("smooth_dim_rf", "Smoothness", c("None"="+0", "Mean"="+x.smoothness_mean", 
                                                                                      "Std Error"="+x.smoothness_se","Worst"="+x.smoothness_worst"),
                                                    width = "fit", selected = "+x.smoothness_mean"),
                                        pickerInput("cp_dim_rf", "Concave Points", c("None"="+0", "Mean"="+x.concave_pts_mean", 
                                                                                      "Std Error"="+x.concave_pts_se","Worst"="+x.concave_pts_worst"),
                                                    width = "fit", selected = "+x.concave_pts_worst")
                        ), #end left column
                        column(5,offset = 1, 
                               pickerInput("compact_dim_rf", "Compactness", c("None"="+0", "Mean"="+x.compactness_mean", 
                                                                               "Std Error"="+x.compactness_se", 
                                                                               "Worst"="+x.compactness_worst"), 
                                           width = "fit", selected = "+x.compactness_worst"),                       
                               pickerInput("concave_dim_rf", "Concavity", c("None"="+0", "Mean"="+x.concavity_mean", 
                                                                             "Std Error"="+x.concavity_se", "Worst"="+x.concavity_worst"),
                                           width = "fit", selected = "+x.concavity_mean"), 
                               pickerInput("symm_dim_rf", "Symmetry", c("None"="+0", "Mean"="+x.symmetry_mean", "Std Error"="+x.symmetry_se", "Worst"="+x.symmetry_worst"),
                                           width  ="fit", selected = "+x.symmetry_worst"), 
                               pickerInput("fd_dim_rf", "Fractal Dimension", c("None"="+0", "Mean"="+x.fractal_dim_mean", "Std Error"="+x.fractal_dim_se", "Worst"="+x.fractal_dim_worst"),
                                           width  ="fit", selected = "+x.fractal_dim_mean"))#end rt column
                        ),#end row 
                        fluidRow(column(5,sliderInput("cv_number", "How many partitions for cross validation?", min = 2, max = 10, value = 5)),column(5, offset= 1,
                                                                                                                                                      sliderInput("mtry", "How many randomly selected variable introduced at each iteration?", min = 1, max = 4, value = 3)))
                    ), #end box for RF input,
                    
                    ),#end lt column
                                                       column(6,add_busy_spinner(spin="fading-circle"),box(width=12, strong("Split the Data Set"),
                                                                    p("The data set needs splitting into two parts, one for training and one for testing."),
                                                                    sliderInput("data_split", "What percent of the data do you want to use for training?", min = 50, max = 95, value = 80, step = 5),
                                                                    strong("Compare Model Input"),
                                                                    p("Select which model (or both) to use. If both are selected then accuracy results will be reported."
                                                                    ),
                                                                    pickerInput("model_to_use", "Model to Use", c("Generalized Linear Regression", "Random Forest", "Both"),
                                                                                width  ="fit", selected = "Generalized Linear Regression"),
                                                                    actionButton("model_now", "Perform Modelling Now!", class= "btn-success"),br(),
                                                                    
                                                                    
                                                                    
                                                                    h3("Generalized Linear Regression Model"),verbatimTextOutput("glm_summary"),  strong(textOutput("glm_test_output")),
                                                                    #textOutput("glm_not_run"),
                                                                    h3("Random Forest Model"),#textOutput("rf_not_run"), 
                                                                    textOutput("rf_mtry"), textOutput("rf_test_output"), plotOutput("rf_var_imp"), verbatimTextOutput("test_text2")
                                                                    
                                                       ), #end box for glm output
                                                       )#end rt column
                                                       )#end fluid row
                             ), #end tabPanel Modelling
                    tabPanel("Prediction", fluidPage(h1("model predict")))
                )#end tabsetpanel for modelling
                
            )#end tabItem-modelling
    ) #end tabitems
    
) #end dashboard body
) #end dashboard page
