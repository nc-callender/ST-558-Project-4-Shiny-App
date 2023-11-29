#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)

#Import Data Set
tumor_data <- read.csv("brca.csv")

#Make a lookup table for variable names relating to characteristic and dimension
variable_names <- as_tibble(colnames(tumor_data)) 
colnames(variable_names) <- c("original_name")
variable_names <- variable_names %>% filter ((original_name != "X" & original_name != "y"))

characteristic_base <- c("Radius", "Texture", "Perimeter", "Area", "Smoothness", "Compactness", "Concavity", "Concave Points", "Symmetry", "Fractal Dimension")
characteristic <- rep(characteristic_base,times  =3)

dimension_base <-c ("Mean", "Std Error", "Worst")
dimension <- rep(dimension_base, times = 10)

variable_table <- data.frame(variable_names, characteristic, dimension )

# Begin server code
shinyServer(function(input, output, session) {

# Output for numeric summary tab
    output$numeric_summary <- renderTable({
        #get variable name
        variable_for_summ <- variable_table %>% 
            filter(characteristic == input$char_of_interest_ns) %>%
            filter (dimension == input$dim_of_interest_ns) %>%
            select (original_name) %>%
            mutate (y = "y")

        #Select variable name and y, label y for plotting
        table_ns <- tumor_data %>% 
            select_({{variable_for_summ[1,1]}},{{variable_for_summ[1,2]}}) 
            colnames(table_ns) <- c("x","Diagnosis")
        table_ns$Diagnosis <-factor(table_ns$Diagnosis, levels=c("B", "M"), 
                            labels = c("Benign", "Malignant"))
        #Make table according to if filtering desired, grouping desired and what summary is requested.
        if(input$filtering_ns=="Malignant and Benign"){
            if(input$grouping){
                if(input$type_of_summary == "Min, Median, Max"){
                    table_ns <- table_ns %>% 
                        group_by(Diagnosis) %>%
                        summarize (Min = min(x), Median = median(x), Max = max(x))
                }
                else{
                    table_ns <- table_ns %>% 
                        group_by(Diagnosis) %>%
                        summarize (Mean = round(mean(x),3), "Std. Dev." = round(sd(x),3))
                }
            } #end for no filtering but with grouping desired
            else{
               if(input$type_of_summary == "Min, Median, Max"){
                   table_ns <- table_ns %>% 
                   summarize (Min = min(x), Median = median(x), Max = max(x))}
                else{
                    table_ns <- table_ns %>% 
                    summarize (Mean = round(mean(x),3), "Std. Dev." = round(sd(x),3))
                }
            }#end for no filter no group
        }#end for no filter
        if(input$filtering_ns=="Malignant" | input$filtering_ns=="Benign"){
            if(input$type_of_summary == "Min, Median, Max"){
                table_ns <- table_ns %>% 
                    filter(Diagnosis == input$filtering_ns) %>%
                    summarize (Min = min(x), Median = median(x), Max = max(x))
            }
            else{
              table_ns <- table_ns %>% 
                filter(Diagnosis == input$filtering_ns) %>%
                summarize (Mean = round(mean(x),3), "Std. Dev." = round(sd(x),3))
            }
          } #end for filtering 

table_ns 
})  # end of numeric summary section
  
      output$test_text <- renderText({
        #get variable name
        variable_for_summ <- variable_table %>% 
            filter(characteristic == input$char_of_interest_ns) %>%
            filter (dimension == input$dim_of_interest_ns) %>%
            select (original_name)
#        
        paste("The variable is", variable_for_summ)
#          variable_for_summ
#        table <- summary(variable_for_summ)
#        table
      
    })


})
