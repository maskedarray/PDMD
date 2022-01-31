{
    options(scipen = 9999999)
    library(plotly)
    library(GGally)
    library(tidyverse)
    library(lubridate)  #used for handling date and time while reading file
    library(dplyr) #used for data manipulation
    library(reshape2)
    library(ggplot2) #visualization
    library(caTools) #for train/test split
    library(corrgram) # for making neat correlation matrix plots
    library(olsrr)
    library(e1071)
    library(neuralnet)
    library(forecast)
    library(xtable)
    library(formattable)
    source("PDMD_functions.R")
}

#### Reading and cleaning metadata ####
{
    metadata = read.csv("Metadata.csv")
    metadata$Ceiling_Insulation = ifelse(metadata$Ceiling_Insulation == "No", 0, 1)
    metadata$Connection_Type[metadata$Connection_Type == "3 single phase meters"] = "1 phase"
    metadata$Connection_Type = as.numeric(gsub("[^0-9.-]", "", metadata$Connection_Type))
    #removing unwanted data parameters from metadata
    metadata = subset(metadata, select = -c(match(c("Building_Year", "Livingrooms", "Bedrooms", "Drawingrooms", "Kitchen",
                                                    "Ceiling_Height_ft", "Ceiling_Insulation", "Connection_Type", "No_of_WashingMachines", 
                                                    "No_of_Water_Dispensers", "No_of._Water_Pumps", "No_of_Irons", "No_of_UPS", "Seniors",
                                                    "Children_0_to_13", "No_of_Electric_Heaters", "Temporary_Residents",
                                                    "Adults_14_to_60", "No_of_Floors", "Permanent_Residents",
                                                    "No_of_Electronic_Devices", "No_of_Lighting_Devices"), names(metadata))))
}

#finding correlation in metadata
metadata_correlations(0.4)

#aggregate monthly consumption from PRECON data
usage_df = validate_aggregate_monthly_consumption()

#dataframe for looping through whole data
saved_params = data.frame(
  Names = colnames(subset(metadata, select = -c(Website.Name))),
  #           01,02,03,04,08,10,11,12,13,15  
  Seeds =   c(43,03,28,10,06,43),
  Reps =    c(10,10,10,10,10,10),
  Hidden1 = c(24,06,03,12,06,14),
  Hidden2 = c(12,03,06,06,03,28),
  prem = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
  MAPE_nn = NA,
  MAPE_lm = NA,
  MAPE_svm = NA
)
lm_coeffs = array(dim = c(13, 6))
#number of floors: 02,10,02,04, 0.2
#permanent residents: 28,10,06,03, 0.1
iteration_count = 3
# for(ii in 1:50){
#   print(ii)
#   saved_params$Seeds[iteration_count] = ii
  
  
  for(iteration_count in 1:length(saved_params$Names)){
  #for(iteration_count in 4:4)
  #{
      training_dataset = data.frame()
      scaled = data.frame()
      generate_training_dataset(usage_df)
      
  
      #### Neural Net ####
      
      #fixed training data
      set.seed(34)
      train_sample_index = sample(1:nrow(scaled), 30)
      trainSet = scaled[train_sample_index,]
      testSet = scaled[-train_sample_index,]
      #computing the model
      set.seed(saved_params$Seeds[iteration_count])
      model_nn = neuralnet(formula=paste0(saved_params$Names[iteration_count], "  ~ ."), data=trainSet,
                           hidden = c(saved_params$Hidden1[iteration_count], saved_params$Hidden2[iteration_count]),
                           stepmax = 100000, rep = 1)

      preds <- predict(model_nn, testSet)
      print(saved_params$Names[iteration_count])

      predict_param =
        (preds[,1] * (max(training_dataset[,c(saved_params$Names[iteration_count])],
              na.rm = T) - min(training_dataset[,c(saved_params$Names[iteration_count])],
                    na.rm = T))) + min(training_dataset[,c(saved_params$Names[iteration_count])], na.rm = T)


      print(accuracy(round(predict_param) ,training_dataset[-train_sample_index,c(saved_params$Names[iteration_count])]))
      acc = accuracy(round(predict_param) ,training_dataset[-train_sample_index,c(saved_params$Names[iteration_count])])
      saved_params$MAPE_nn[iteration_count] = acc[5]
  
      
      #### Linear Model ####
      #linear model. Result = 40% MAPE
      
      #fixed training data
      set.seed(34)
      train_sample_index = sample(1:nrow(training_dataset), 30)
      trainSet = training_dataset[train_sample_index,]
      testSet = training_dataset[-train_sample_index,]
      #computing linear model
      set.seed(saved_params$Seeds[iteration_count])
      model <- lm(formula=paste0(saved_params$Names[iteration_count], "  ~ ."), data=trainSet)
      lm_coeffs[,iteration_count] = model$coefficients
      both_output = ols_step_both_p(model, details = TRUE,  prem = 0.1)
      summary(both_output$model)
      preds <- predict(both_output$model, testSet)
      print(accuracy(round(preds), training_dataset[-train_sample_index,c(saved_params$Names[iteration_count])]))
      acc = accuracy(round(preds), training_dataset[-train_sample_index,c(saved_params$Names[iteration_count])])
      saved_params$MAPE_lm[iteration_count] = acc[5]
      rm(acc)
      
      #### SVM Model ####
      set.seed(saved_params$Seeds[iteration_count])
      model_svm = svm(formula=formula(paste0(saved_params$Names[iteration_count], "  ~ .")),
                      data=trainSet, type = "eps-regression")
      preds <- predict(model_svm, testSet)
      print(accuracy(round(preds), training_dataset[-train_sample_index,c(saved_params$Names[iteration_count])]))
      acc = accuracy(round(preds), training_dataset[-train_sample_index,c(saved_params$Names[iteration_count])])
      saved_params$MAPE_svm[iteration_count] = acc[5]
      rm(acc)
      
      rm(trainSet, testSet, train_sample_index, both_output, model, model_nn, model_svm, scaled, training_dataset, iteration_count)
  }

 #}

#printing the table of mape to latex file
{
  out_table = saved_params[,c(1,7,8,9)]
  
  print(xtable(out_table, type = "latex"), file = "output_table.tex")
}

 #### SVR Model ####

# {
# 
#     model_svm = svm(formula=No_of_People ~ ., data=trainSet, type = "eps-regression")
#     
#     summary(model_svm)
#     preds <- predict(model_svm, testSet)
#     
#     
#     modelEval <- cbind(testSet$No_of_People, preds)
#     colnames(modelEval) <- c('Actual', 'Predicted')
#     modelEval <- as.data.frame(modelEval)
#     
#     accuracy(testSet$No_of_People, preds)
#       
# }

#figure of monthly consumption of all houses lines plot (not needed)
# training_dataset = data.frame(t(training_dataset))
# 
# 
# fig = plot_ly(mode = 'lines', type = "scatter")
# 
# for(i in 1: 34){
#    fig = fig %>% add_trace( y = training_dataset[,i], name = paste0("House", i, sep = "", collapse = NULL))
# }

#box plot for diversity in metadata
fig
rm(fig)
metadata = metadata[-c(1,3,6,14,24,30,35,38),]
{
  fig1 = plot_ly(type = 'box', showlegend = FALSE, color = I("gray40"))
  fig1 = fig1 %>% add_trace(y = metadata$No_of_People, name = "No. of people")
  fig1 = fig1 %>% layout(yaxis = list(showline = TRUE, tickfont = list( size=16)), xaxis = list(showline = TRUE, tickfont = list( size=16)))
  
  fig2 = plot_ly(type = 'box', showlegend = FALSE, color = I("gray40"))
  fig2 = fig2 %>% add_trace(y = metadata$No_of_Fans, name = "No. of fans")
  fig2 = fig2 %>% layout(yaxis = list(showline = TRUE, tickfont = list( size=16)), xaxis = list(showline = TRUE, tickfont = list( size=16)))
  
  fig3 = plot_ly(type = 'box', showlegend = FALSE, color = I("gray40"))
  fig3 = fig3 %>% add_trace(y = metadata$No_of_ACs, name = "No. of air conditioners")
  fig3 = fig3 %>% layout(yaxis = list(showline = TRUE, tickfont = list( size=16)), xaxis = list(showline = TRUE, tickfont = list( size=16)))
  
  fig4 = plot_ly(type = 'box', showlegend = FALSE, color = I("gray40"))
  fig4 = fig4 %>% add_trace(y = metadata$Total_No_of_Rooms, name = "No. of rooms")
  fig4 = fig4 %>% layout(yaxis = list(showline = TRUE, tickfont = list( size=16)), xaxis = list(showline = TRUE, tickfont = list( size=16)))
  
  
  fig6 = plot_ly(type = 'box', showlegend = FALSE, color = I("gray40"))
  fig6 = fig6 %>% add_trace(y = metadata$No_of_Refrigerators, name = "No. of refrigerators")
  fig6 = fig6 %>% layout(yaxis = list(showline = TRUE, tickfont = list( size=16)), xaxis = list(showline = TRUE, tickfont = list( size=16)))
  
  
  fig8 = plot_ly(type = 'box', showlegend = FALSE, color = I("gray40"))
  fig8 = fig8 %>% add_trace(y = metadata$Property_Area_sqft, name = "Property area (sqft)")
  fig8 = fig8 %>% layout(yaxis = list(showline = TRUE, tickfont = list( size=16)), xaxis = list(showline = TRUE, tickfont = list( size=16)))
  
  fig = subplot(fig1, fig2, fig3, fig4, fig6, fig8, nrows = 2, margin = 0.05)
  
  
  rm(fig1, fig2, fig3, fig4, fig6, fig8) 
}
fig
rm(fig)


#box plot for diversity in consumption pattern for each months
#takes training data till line 133

#### Creating dataset for training ####
#unmelt usage_df and rearrange columns
training_dataset = dcast(usage_df[, c(1,3,2)], house~month )
#arranging data by house number
#change column name
colnames(training_dataset)[1] = "House_number"
#remove letters from house column
training_dataset$House_number = as.numeric( gsub("House ", "", 
                                                 training_dataset$House_number))
#order the dataset
training_dataset = 
  training_dataset[order(training_dataset$House_number),]
#adjust names of rows
row.names(training_dataset) = 1:nrow(training_dataset)
fig = plot_ly(type = "box", color = I("gray40"), showlegend = FALSE)

for(i in 2: 13){
  fig = fig %>% add_trace( y = training_dataset[,i], name = usage_df$month[i-1])
}
fig = fig %>% layout(yaxis = list(title = list(text="Energy Consumption (kWh)", font = list(size=16)), 
                                  showline = TRUE, tickfont = list( size=18), range = list(0,2500)), 
                     xaxis = list(showline = TRUE, tickfont = list( size=18)))
fig
rm(fig, training_dataset)


#table for beta coefficients

print(xtable(formattable(lm_coeffs, digit = 4), type = "latex"), file = "output_coeffs.tex")





