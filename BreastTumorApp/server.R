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
tumor_data <- read.csv("brca.csv") %>% rename ("Diagnosis" = y)
tumor_data $Diagnosis <-factor(tumor_data $Diagnosis, levels=c("B", "M"), 
                            labels = c("Benign", "Malignant"))

#Make a lookup table for variable names relating to characteristic and dimension
variable_names <- as_tibble(colnames(tumor_data)) 
colnames(variable_names) <- c("original_name")
variable_names <- variable_names %>% filter ((original_name != "X" & original_name != "Diagnosis"))

characteristic_base <- c("Radius", "Texture", "Perimeter", "Area", "Smoothness", "Compactness", "Concavity", "Concave Points", "Symmetry", "Fractal Dimension")
characteristic <- rep(characteristic_base,times  =3)

dimension_base <-c ("Mean", "Std Error", "Worst")
dimension <- rep(dimension_base, times = 10)

variable_table <- data.frame(variable_names, characteristic, dimension )

#Function for accessing variable name
get_variable_name <- function (characteristic_of_interest = "Area", dimension_of_interest = "Mean"){
    variable_name_desired<- variable_table %>% 
    filter(characteristic == characteristic_of_interest) %>%
    filter (dimension == dimension_of_interest) %>%
    select (original_name) %>%
    mutate (Diagnosis = "Diagnosis")
}

# Begin server code
shinyServer(function(input, output, session) {

# Output for numeric summary tab
    output$numeric_summary <- renderTable({
        #get variable name
        variable_for_summ <-get_variable_name (characteristic_of_interest = input$char_of_interest_ns,
                                               dimension_of_interest = input$dim_of_interest_ns)

        #Select variable name and y, label y for plotting
        table_ns <- tumor_data %>% 
            select_({{variable_for_summ[1,1]}},{{variable_for_summ[1,2]}}) 
            colnames(table_ns) <- c("x","Diagnosis")

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
  

# Output for graphical summary tab
    output$graphical_summary <- renderPlotly({
        #bar plot
        if (input$type_of_graph == "Bar Plot"){
            graph1 <- ggplot(data = tumor_data, aes (x = Diagnosis)) +
                geom_bar(aes(fill=Diagnosis))
        graph <- ggplotly(graph1) %>% layout(legend = list(x = 0.865, y = 1.0))
        }
        #histogram
        #determine variable name using lookup table
        variable_for_hist <- get_variable_name (characteristic_of_interest = input$char_of_interest_hist,
                                                 dimension_of_interest = input$dim_of_interest_hist)

      #Select variable name and y, label y for plotting
        table_hist <- tumor_data %>% 
            select_({{variable_for_hist[1,1]}},{{variable_for_hist[1,2]}}) 
        colnames(table_hist) <- c("x","Diagnosis")
      
      #make label for x
      x_label_hist <- paste0 (input$char_of_interest_hist, ": ", input$dim_of_interest_hist)

      # make graph (histogram)
        if (input$type_of_graph == "Histogram"){
            if  (input$grouping_hist){
              graph1 <- ggplot(data = table_hist, aes (x = x)) +
                geom_histogram(bins=input$bins_hist, aes(fill = Diagnosis), position = "dodge") + 
                labs(x = x_label_hist) +
                theme(legend.position = "bottom")
            }
            else{
            graph1 <- ggplot(data = table_hist, aes (x = x)) +
                geom_histogram(bins=input$bins_hist, fill="cyan4") +
                labs(x = x_label_hist)
            }
        graph <- ggplotly(graph1) %>% layout(legend = list(x = 0.865, y = 1.0))
        } #end histogram

        #box
        #determine variable name using lookup table
        variable_for_box <- get_variable_name (characteristic_of_interest = input$char_of_interest_box,
                                                  dimension_of_interest = input$dim_of_interest_box)

        #Select variable name and y, label y for plotting
        table_box<- tumor_data %>% 
            select_({{variable_for_box[1,1]}},{{variable_for_box[1,2]}}) 
        colnames(table_box) <- c("y","Diagnosis")
      
        #make label for x
        y_label_box <- paste0 (input$char_of_interest_box, ": ", input$dim_of_interest_box)
      
        # make graph (box plot)
        if (input$type_of_graph == "Box Plot"){
            if  (input$grouping_box){
                graph1 <- ggplot(data = table_box, aes (x= Diagnosis, y = y)) +
                geom_boxplot(aes(fill = Diagnosis), position = "dodge") + 
                labs(y = y_label_box, x="Diagnosis")
            }
            else{
                graph1 <- ggplot(data = table_box, aes (y = y)) +
                geom_boxplot(fill="cyan4") +
                labs(y = y_label_box) +
                theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
            }
        } #end box
        
        #scatter plot
        #determine variable names using lookup table
        variable_for_scatter_x <- get_variable_name (
            characteristic_of_interest = input$char_of_interest_scatter_x,
            dimension_of_interest = input$dim_of_interest_scatter_x)
        variable_for_scatter_y <- get_variable_name (
          characteristic_of_interest = input$char_of_interest_scatter_y,
          dimension_of_interest = input$dim_of_interest_scatter_y)

        #Select variable names and diagnosis and make axis labels
        table_scatter <- tumor_data %>% 
          select_({{variable_for_scatter_x[1,1]}},
                  {{variable_for_scatter_y[1,1]}},
                  {{variable_for_scatter_y[1,2]}}) 
        colnames(table_scatter) <- c("x", "y", "Diagnosis")

                #make label for x
        x_label_scatter <- paste0 (input$char_of_interest_scatter_x, ": ", input$dim_of_interest_scatter_x)
        y_label_scatter <- paste0 (input$char_of_interest_scatter_y, ": ", input$dim_of_interest_scatter_y)
      
        # make graph (scatter plot)
        if (input$type_of_graph == "Scatter Plot"){
            if  (input$grouping_scatter){
                graph1 <- ggplot(data = table_scatter, aes (x = x, y = y)) +
                          geom_point(aes(color = Diagnosis)) + 
                          labs(x = x_label_scatter,y = y_label_scatter ) +
                          theme(legend.position = "bottom")

                if (input$trendline_scatter){
                    graph1 <- graph1 + geom_smooth (method = lm, 
                                                    aes(group = Diagnosis, color = Diagnosis), 
                                                    se = FALSE)
                }
            }
            else{
                graph1 <- ggplot(data = table_scatter, aes (x = x, y = y)) +
                    geom_point() +
                    labs(x = x_label_scatter,y = y_label_scatter )

                if (input$trendline_scatter){
                      graph1 <- graph1 + geom_smooth (method = lm, se = FALSE)
                }
            } 
           graph <- ggplotly(graph1) %>% layout(legend = list(orientation = "h", x = 0, y = 1.0))
        } #end scatter
      graph 
    })  # end of graphical summary section
    
    
    
    
    
    
    
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
