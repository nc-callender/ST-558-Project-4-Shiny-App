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
library(corrplot)
library(caret)
library(ggcorrplot)
library(randomForest)

#Import Data Set
tumor_data <- read.csv("brca.csv") %>% rename ("Diagnosis" = y)
tumor_data $Diagnosis <-factor(tumor_data $Diagnosis, levels = c("B", "M"), 
                            labels = c("Benign", "Malignant"))

#Make a lookup table for variable names relating to characteristic and dimension
variable_names <- as_tibble(colnames(tumor_data)) 
colnames(variable_names) <- c("original_name")
variable_names <- variable_names %>% filter ((original_name != "X" & original_name != "Diagnosis"))

characteristic_base <- c("Radius", "Texture", "Perimeter", "Area", "Smoothness", "Compactness", "Concavity",
                         "Concave Points", "Symmetry", "Fractal Dimension")
characteristic <- rep(characteristic_base,times = 3)

dimension <-c(rep("Mean", times = 10), rep("Std Error", times = 10), rep("Worst", times = 10))

variable_table <- data.frame(variable_names, characteristic, dimension )
variable_table <- variable_table %>% unite(char_dim,characteristic:dimension, sep = "_", remove = FALSE)

#Function for accessing variable name
get_variable_name <- function (characteristic_of_interest = "Area", dimension_of_interest = "Mean"){
    variable_name_desired <- variable_table %>% 
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
        if(input$filtering_ns == "Malignant and Benign"){
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
                      geom_bar(aes(fill = Diagnosis))
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
                          geom_histogram(bins = input$bins_hist, aes(fill = Diagnosis), position = "dodge") + 
                          labs(x = x_label_hist) +
                          theme(legend.position = "bottom")
            }
            else{
                graph1 <- ggplot(data = table_hist, aes (x = x)) +
                          geom_histogram(bins = input$bins_hist, fill = "cyan4") +
                          labs(x = x_label_hist)
            }
        graph <- ggplotly(graph1) %>% layout(legend = list(x = 0.865, y = 1.0))
        } #end histogram

        #box plot
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
            if(input$grouping_box){
                graph1 <- ggplot(data = table_box, aes (x = Diagnosis, y = y)) +
                          geom_boxplot(aes(fill = Diagnosis), position = "dodge") + 
                          labs(y = y_label_box, x = "Diagnosis")
            }
            else{
                graph1 <- ggplot(data = table_box, aes (y = y)) +
                          geom_boxplot(fill="cyan4") +
                          labs(y = y_label_box) +
                          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
            }
        graph <- ggplotly(graph1) %>% layout(legend = list(x = 0.865, y = 1.0))
        } #end boxplot
        
        #scatter plot
        #determine variable names using lookup table
        variable_for_scatter_x <- get_variable_name (
            characteristic_of_interest = input$char_of_interest_scatter_x,
            dimension_of_interest = input$dim_of_interest_scatter_x)
        variable_for_scatter_y <- get_variable_name (
          characteristic_of_interest = input$char_of_interest_scatter_y,
          dimension_of_interest = input$dim_of_interest_scatter_y)

        #Select variable names and diagnosis 
        table_scatter <- tumor_data %>% 
          select_({{variable_for_scatter_x[1,1]}},
                  {{variable_for_scatter_y[1,1]}},
                  {{variable_for_scatter_y[1,2]}}) 
        colnames(table_scatter) <- c("x", "y", "Diagnosis")

        #make label for x and y
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
                if (input$trendline_scatter){ graph1 <- graph1 + geom_smooth (method = lm, se = FALSE)}
            } 
        graph <- ggplotly(graph1) %>% layout(legend = list(orientation = "h", x = 0, y = 1.0))
        } #end scatter
        
        #begin corrplot
        #select data based on filtering input
        if (input$filtering_corrplot == "Malignant and Benign") {
            table_corrplot <- tumor_data %>% select (2:31)
        }
        else {
            table_corrplot <-tumor_data %>% filter(Diagnosis == input$filtering_corrplot) %>% select (2:31)
        }
        
        #select data based on dimension or parameter of interest
        if (input$dim_or_parameter_corrplot == "Parameter"){
            if(input$dim_corrplot == "Mean") {table_corrplot <-table_corrplot %>% select(1:10)}
            else if(input$dim_corrplot == "Std Error") {table_corrplot <-table_corrplot %>% select(11:20)}
            else  {table_corrplot <-table_corrplot %>% select(21:30)}

        colnames(table_corrplot) <- c("Radius", "Texture", "Perimeter", "Area", "Smoothness",
                                      "Compactness", "Concavity", "Concave Points", "Symmetry",
                                      "Frac. Dim.")
        }
        
        else if (input$dim_or_parameter_corrplot == "Dimension"){
            if(input$parameter_corrplot == "Radius") {table_corrplot <- table_corrplot %>% select(1,11,21)}
            else if(input$parameter_corrplot == "Texture") {table_corrplot <- table_corrplot %>% select(2,12,22)}
            else if(input$parameter_corrplot == "Area") {table_corrplot <- table_corrplot %>% select(3,13,23)}
            else if(input$parameter_corrplot == "Perimeter") {table_corrplot <- table_corrplot %>% select(4,14,24)}
            else if(input$parameter_corrplot == "Smoothness") {table_corrplot <- table_corrplot %>% select(5,15,25)}
            else if(input$parameter_corrplot == "Compactness") {table_corrplot <- table_corrplot %>% select(6,16,26)}
            else if(input$parameter_corrplot == "Concavity") {table_corrplot <- table_corrplot %>% select(7,17,27)}
            else if(input$parameter_corrplot == "Concave Points") {table_corrplot <- table_corrplot %>% select(8,18,28)}
            else if(input$parameter_corrplot == "Symmetry") {table_corrplot <- table_corrplot %>% select(9,19,29)}
            else  {table_corrplot <-table_corrplot %>% select(10,20,30)}

            colnames(table_corrplot) <- c("Mean", "Std Error", "Worst")          
        }
        
        if (input$type_of_graph == "Correlation Plot"){
          #make the plot
            corr <- cor(table_corrplot, method = "spearman")
            graph1 <-ggcorrplot(corr, lab = TRUE, tl.cex = 8, lab_size = 2,  show.legend = FALSE)
            graph <- ggplotly(graph1) %>% layout(legend = list(orientation = "h", x = 0, y = 1.0))
        }

    graph  #ouput the graph
    })  # end of graphical summary section

#Begin Modelling and Prediction
    observe({
        observeEvent(input$model_now,{
            #Data splitting using caret
            split <- input$data_split/100

                        #split dataset
            set.seed(14641) #reproducibility of splitting
            train_index <-createDataPartition(tumor_data$Diagnosis, p = split, list = TRUE)
            tumor_data_train <- tumor_data[train_index[[1]],]
            tumor_data_test <- tumor_data[-train_index[[1]],]

            #Reset ouputs when modelling begins (wipe out ghosts from previous model/predictions).
            output$glm_predict_benign <- renderText(" ")
            output$glm_predict_malignant <- renderText(" ")
            output$rf_predict_benign <- renderText(" ")
            output$rf_predict_malignant <- renderText(" ")
            output$glm_variables1 <- renderText(" ")
            output$glm_variables2 <- renderText(" ")
            output$rf_variables1 <- renderText(" ")
            output$rf_variables2 <- renderText(" ")
          
            #Input model to use and make corresponding statement for predicion tab
            methods_used <-input$model_to_use
            output$methods_for_prediction <- renderText(methods_used)

            if(input$model_to_use=="Generalized Linear Regression" | input$model_to_use=="Both"){
                #Create equation for use with glm
                if (input$size_dim_glm != "None"){size_var_glm <- paste0(input$size_glm, input$size_dim_glm)}
                else {size_var_glm <- ("+")}
                outcome <- "Diagnosis"
                variables_glm <- c(size_var_glm, input$texture_dim_glm, input$smooth_dim_glm,
                                 input$cp_dim_glm, input$compact_dim_glm, input$concave_dim_glm,
                                 input$symm_dim_glm, input$fd_dim_glm)

                formula_glm <- as.formula(paste (outcome, 
                                          paste(variables_glm, collapse = " "), 
                                          sep = " ~ "))

               #glm model       
                set.seed(121) #reproducibility for cross validation
                fit_glm <- train(formula_glm, 
                                 data = tumor_data_train, 
                                 method = "glm", 
                                 family = "binomial", 
                                 preProcess = c("center", "scale"),
                                 trControl = trainControl(method = "cv", number = 5)
                ) #end fit

                #Determination of accuracy using model on test set
                pred_glm_acc <- predict(fit_glm, newdata = tumor_data_test)
                pred_glm_acc_results <- confusionMatrix(data = tumor_data_test$Diagnosis,
                                                        reference = pred_glm_acc)

                #outputs for glm
                output$glm_summary <- renderPrint({summary(fit_glm)})
        
                output$glm_test_output <- renderText({
                    paste0("The accuracy of the GLM model in predicting the training set is ",
                            round(fit_glm[[4]][2]*100,1) , "%.")
                 })
        
                output$glm_test_acc <- renderText({
                    paste0("The accuracy of the GLM model in predicting the test set is ",
                            round(pred_glm_acc_results$overall[1]*100,1) , "%.")})

                #Reset random forest outputs when GLM only selected (get rid of ghosts)
                if (input$model_to_use=="Generalized Linear Regression"){
                    output$rf_var_imp <- renderPlot({})
                    output$rf_mtry <- renderText({" "})
                    output$rf_not_performed <- renderText({("Random Forest was not selected.")})
                    output$rf_test_output <- renderText({("")})
                    output$rf_test_acc <- renderText({" "})
                } #end of reset random forest outputs

                #Prepare list of variables used in fitting to display in prediction tab
                var_list_glm <- fit_glm[[23]]   #retrieve variables from fit 
                var_list_display <- c()         #initialize vector for variables
                for (i in 1:length(var_list_glm)){
                    var_list_display[i] <- variable_table[which(variable_table$original_name == var_list_glm[i]),2]
                }

                var_list_collapsed <- paste(var_list_display, collapse = " + ")
                output$glm_variables1 <- renderText("The variables used in the generalized linear regression model were: ") 
                output$glm_variables2 <- renderText(paste(var_list_collapsed))
        
                #Prediction
                observeEvent(input$predict_now,{

                    #Blank out results from previous predictions
                    output$rf_predict_benign <- renderText("")
                    output$rf_predict_malignant <- renderText("")
                    output$glm_predict_benign <- renderText("")
                    output$glm_predict_malignant <- renderText("")
                    
                    #Gather inputs for glr
                    if(methods_used== "Generalized Linear Regression" | methods_used == "Both"){
                    values_for_pred <- data_frame(x.radius_mean = input$radius_mean, 
                                                  x.texture_mean = input$texture_mean, 
                                                  x.perimeter_mean = input$perimeter_mean, 
                                                  x.area_mean = input$area_mean,
                                                  x.smoothness_mean = input$smoothness_mean, 
                                                  x.compactness_mean = input$compactness_mean,
                                                  x.concavity_mean = input$concavity_mean,
                                                  x.concave_pts_mean = input$cp_mean,
                                                  x.symmetry_mean = input$symmetry_mean,
                                                  x.fractal_dim_mean = input$fd_mean,
                                                  x.radius_se = input$radius_se, 
                                                  x.texture_se = input$texture_se, 
                                                  x.perimeter_se = input$perimeter_se,
                                                  x.area_se = input$area_se,
                                                  x.smoothness_se = input$smoothness_se,
                                                  x.compactness_se = input$compactness_se,
                                                  x.concavity_se = input$concavity_se,
                                                  x.concave_pts_se = input$cp_se,
                                                  x.symmetry_se = input$symmetry_se,
                                                  x.fractal_dim_se = input$fd_se,
                                                  x.radius_worst = input$radius_worst, 
                                                  x.texture_worst = input$texture_worst, 
                                                  x.perimeter_worst = input$perimeter_worst,
                                                  x.area_worst = input$area_worst,
                                                  x.smoothness_worst = input$smoothness_worst,
                                                  x.compactness_worst = input$compactness_worst,
                                                  x.concavity_worst = input$concavity_worst,
                                                  x.concave_pts_worst = input$cp_worst,
                                                  x.symmetry_worst = input$symmetry_worst,
                                                  x.fractal_dim_worst = input$fd_worst)

                    #Do probability calculation and convert to diagnosis (GLM)
                    predict_glm_prob <- predict(fit_glm, newdata = values_for_pred, type="prob")
                    predict_glm_diagnosis <- if_else (predict_glm_prob$Benign> 0.5, "Benign", "Malignant")

                    #Output statements for diagnosis (to be color coded.)
                    if (predict_glm_diagnosis == "Benign"){
                        output$glm_predict_benign<- renderText(paste0(
                            "For the given values for predictors, the fit from the generalized linear model predicts ",
                            predict_glm_diagnosis, "."))
                        output$glm_predict_malignant <- renderText(paste0(""))
                    }

                    if (predict_glm_diagnosis == "Malignant"){
                        output$glm_predict_malignant <- renderText(paste0(
                            "For the given values for predictors, the fit from the generalized linear model predicts ",
                             predict_glm_diagnosis, "."))
                    output$glm_predict_benign <- renderText(paste0(""))
                    }
                    
                    } #end of predict if glm or both
                }) # end of observe event for predict
            } #end of glm or both

            
            if(input$model_to_use=="Random Forest" | input$model_to_use=="Both"){

                #clear rf not performed statement
                output$rf_not_performed <- renderText({(" ")})
        
                #Equation formula random forest
                #Create equation for use with glm
                if (input$size_dim_rf != "None"){size_var_rf <- paste0(input$size_rf, input$size_dim_rf)}
                else {size_var_rf <- ("+")}
          
                outcome <- "Diagnosis"
                variables_rf <- c(size_var_rf, input$texture_dim_rf, input$smooth_dim_rf, input$cp_dim_rf,
                                  input$compact_dim_rf, input$concave_dim_rf, input$symm_dim_rf, input$fd_dim_rf)
                formula_rf <- as.formula(paste (outcome, 
                                         paste(variables_rf, collapse = " "), 
                                         sep = " ~ "))

                #Tuning grid for random forest
                mtrys <- seq(1, input$mtry, by = 1)
        
                #Random Forest Model
                set.seed(121) #reproducibility for cross validation
                fit_rf <- train(formula_rf,
                          data = tumor_data_train,
                          method = "rf",
                          family = "binomial",
                          preProcess = c("center", "scale"),
                          trControl= trainControl(method = "cv", number = input$cv_number),
                          tuneGrid = expand.grid (mtry = mtrys)
                )
                
                #output mtry statement for random forest
                output$rf_mtry <- renderText({
                paste0("The optimized value for the number of variables randomly chosen at each branching (mtry) was ",
                        fit_rf$bestTune$mtry , ".")
                })
                
                #Prediction for use with accuracy on test set
                pred_rf_acc <- predict(fit_rf, newdata = tumor_data_test)
                pred_rf_acc_results <- confusionMatrix(data = tumor_data_test$Diagnosis,
                                                       reference = pred_rf_acc)

                #output accuracy statement for random forest training-training data
                output$rf_test_output<- renderText({
                #Pull accuracy from fit results
                accuracy_rf <- fit_rf$results %>% select(mtry, Accuracy) %>%
                filter(mtry == fit_rf$bestTune$mtry) %>% select (Accuracy)
                paste0("The accuracy of the random forest model on the training set ",
                       "using the optimized mtry was ", round((accuracy_rf*100),1), "%.")
                })
                #output accuracy statement for random forest testing-test data
                output$rf_test_acc <- renderText({
                paste0("The accuracy of the random forest model in predicting the test set is ",
                       round(pred_rf_acc_results$overall[1]*100,1) , "%.")})
        
               #Reset outputs when random forest only is run. (Get rid of ghosts from previous models)
                if (input$model_to_use=="Random Forest"){
                    output$glm_test_output <- renderText({("The generalized linear model was not selected.")})
                    output$glm_summary <- renderPrint({invisible()})
                    output$glm_test_acc <- renderText({" "})
                    output$rf_not_performed <- renderText({(" ")})
                }
        
                #Generate variable importance plot. Diagnosis will need converting to factor.
                tumor_data_train_2 <- tumor_data_train
                tumor_data_train_2$Diagnosis <- as.factor(tumor_data_train_2$Diagnosis)
                outcome2 <- "Diagnosis"

                formula_rf_2 <- as.formula(paste (outcome2,
                                           paste(variables_rf, collapse = " + "),
                                           sep = " ~ "))

                #Output variable importance plot
                 output$rf_var_imp <- renderPlot({
                 tumor_rf<-randomForest(formula_rf_2, data=tumor_data_train_2, 
                                        mtry=fit_rf$bestTune$mtry, importance =TRUE)
                 varImpPlot(tumor_rf, main="Variable Importance Plot")
                }) #end of variable importance plot
        
                #Prepare list of variables used in fitting to display in prediction tab
                var_list_rf <- fit_rf[[23]]    #get varaibles used in the fit
                var_list_display_rf <- c()     #initialize a vector
                for (i in 1:length(var_list_rf)){
                    var_list_display_rf[i] <- variable_table[which(variable_table$original_name == var_list_rf[i]),2]
                }
        
                var_list_rf_collapsed <- paste(var_list_display_rf, collapse = " + ")
                output$rf_variables1 <- renderText("The variables used in the random forest model were: ") 
                output$rf_variables2 <- renderText(paste(var_list_rf_collapsed))
        
        
                #Perform prediction
                observeEvent(input$predict_now,{
                    output$rf_predict_benign <- renderText("")
                    output$rf_predict_malignant <- renderText("")
                    if (methods_used != "Both") {
                        output$glm_predict_benign <- renderText("")
                        output$glm_predict_malignant <- renderText("")}
                    if(methods_used== "Random Forest" | methods_used == "Both"){
                        #Harvest inputs
                        values_for_pred <- data_frame(x.radius_mean = input$radius_mean, 
                                           x.texture_mean = input$texture_mean, 
                                           x.perimeter_mean = input$perimeter_mean, 
                                           x.area_mean = input$area_mean,
                                           x.smoothness_mean = input$smoothness_mean, 
                                           x.compactness_mean = input$compactness_mean,
                                           x.concavity_mean = input$concavity_mean,
                                           x.concave_pts_mean = input$cp_mean,
                                           x.symmetry_mean = input$symmetry_mean,
                                           x.fractal_dim_mean = input$fd_mean,
                                           x.radius_se = input$radius_se, 
                                           x.texture_se = input$texture_se, 
                                           x.perimeter_se = input$perimeter_se,
                                           x.area_se = input$area_se,
                                           x.smoothness_se = input$smoothness_se,
                                           x.compactness_se = input$compactness_se,
                                           x.concavity_se = input$concavity_se,
                                           x.concave_pts_se = input$cp_se,
                                           x.symmetry_se = input$symmetry_se,
                                           x.fractal_dim_se = input$fd_se,
                                           x.radius_worst = input$radius_worst, 
                                           x.texture_worst = input$texture_worst, 
                                           x.perimeter_worst = input$perimeter_worst,
                                           x.area_worst = input$area_worst,
                                           x.smoothness_worst = input$smoothness_worst,
                                           x.compactness_worst = input$compactness_worst,
                                           x.concavity_worst = input$concavity_worst,
                                           x.concave_pts_worst = input$cp_worst,
                                           x.symmetry_worst = input$symmetry_worst,
                                           x.fractal_dim_worst = input$fd_worst)

                        #Obtain prediction as probability and convert to diagnosis
                        predict_rf_prob <- predict(fit_rf, newdata = values_for_pred, type = "prob")
                        predict_rf_diagnosis <- if_else (predict_rf_prob$Benign > 0.5, "Benign", "Malignant")
          
                        #Output diagnosis- to be color coded by user interface.
                        if (predict_rf_diagnosis == "Benign"){
                        output$rf_predict_benign <- renderText(paste0("For the given values for predictors, 
                            the fit from the random forest model predicts ",
                            predict_rf_diagnosis, "."))
                        output$rf_predict_malignant <- renderText(paste0(""))
                        }
          
                        if (predict_rf_diagnosis == "Malignant"){
                        output$rf_predict_malignant <- renderText(paste0("For the given values for predictors, 
                            the fit from the random forest model predicts ",
                            predict_rf_diagnosis, "."))
                        output$rf_predict_benign <- renderText(paste0(""))
                        }

                    }#end of predict- if method is random forest or both 
                }) #End observe event predict_now
            } # end of model if random forest or both
        })# end observe event model_now
    })#end observe (modelling observing data split)
})#end server
