
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# Define UI for application that draws a histogram
ui<-dashboardPage(skin = "blue",
  
    # Application title
    dashboardHeader(title = "Breast Tumor Charateristics and Modeling of Malignancy"),

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
            tabItem("about",
                fluidPage(h1("Header"))),
            tabItem("data_exploration",
                fluidPage(h1("data explore"))),
            tabItem("modeling",
                fluidPage(h1 ("model")))
        )
        
    
    )
    
)
        
