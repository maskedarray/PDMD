
validate_aggregate_monthly_consumption <- function() {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  if (file.exists("saved_data.Rda")){
    load("saved_data.Rda")
    return(usage_df)
  } else {
    usage_df = data.frame()
    usage_df1 = data.frame()
    for (x in 1:42){
      print(x)
      filename = paste("House",x, sep = "", collapse = NULL)
      filedata = read.csv(paste0("PRECON/",filename,".csv"))
      filedata$Date_Time = as.POSIXct(filedata$Date_Time)
      filedata = merge(filedata, data.frame(Date_Time = seq.POSIXt(from = as.POSIXct("2018-06-01"), 
                                                                   to = as.POSIXct("2019-05-31 23:59:59"), 
                                                                   by = "min" )), all.y = TRUE)
      if((length(which(is.na(filedata$Usage_kW)))/1440) < 3 ) {
        
        usage_df1 <- aggregate(x = filedata$Usage_kW, 
                               by = list(month = substr(filedata$Date_Time, 1, 7)), 
                               FUN = sum, na.rm = TRUE)
        filename1 = paste("House ",x, sep = "", collapse = NULL)
        usage_df1$house = filename1
        names(usage_df1)[names(usage_df1) == "x"] <- "units"
        if(dim(table(usage_df1$units < 10)) == 1) {
          usage_df = rbind(usage_df, usage_df1)  
        } else {
          print(filename)
          print(table(usage_df1$units < 10))
          print("incorrect data")
        }
        
      } else {
        print(filename)
        print(length(which(is.na(filedata$Usage_kW)))/1440)
        print("incorrect data")
      }
    }
    usage_df$units = usage_df$units/60.0
    rm(usage_df1, filename, filedata, x, filename1)
    save(usage_df, file = "saved_data.Rda")
    return(usage_df)
  }
}

metadata_correlations <- function(corr_threshold) {
  corr_pairs = cor(subset(metadata, select = -c(Website.Name)))
  corr_pairs[lower.tri(corr_pairs, diag = FALSE)] = 0
  idx <- which((corr_pairs>corr_threshold) & corr_pairs != 1, arr.ind = TRUE)
  corr_pairs_names = cbind(rownames(corr_pairs)[idx[,"row"]], colnames(corr_pairs)[idx[,"col"]], corr_pairs[idx])
  rm(idx, corr_threshold)
  #printing the correlation pairs
  print(corr_pairs_names[order(corr_pairs_names[,3], decreasing = TRUE),])  
  rm(corr_pairs, corr_pairs_names)
}

ameera_plots <- function() {
  #plot_ly(x = usage_df$month[usage_df$house == "House1"], y = usage_df$units[usage_df$house == "House1"], type = "bar")
  
  #making plots
  
  # fig7 <- plot_ly(y=usage_df$units[usage_df$month == '2018-07'], type = "box", name = "18-07")
  # fig7 <- fig7 %>% add_trace(y =usage_df$units[usage_df$month == '2018-08'], type = "box", name = "18-08")
  # fig7 <- fig7 %>% add_trace(y =usage_df$units[usage_df$month == '2018-09'], type = "box", name = "18-09")
  # fig7 <- fig7 %>% add_trace(y =usage_df$units[usage_df$month == '2018-10'], type = "box", name = "18-10")
  # fig7 <- fig7 %>% add_trace(y =usage_df$units[usage_df$month == '2018-11'], type = "box", name = "18-11")
  # fig7 <- fig7 %>% add_trace(y =usage_df$units[usage_df$month == '2018-12'], type = "box", name = "18-12")
  # fig7 <- fig7 %>% add_trace(y =usage_df$units[usage_df$month == '2019-01'], type = "box", name = "19-01")
  # fig7 <- fig7 %>% add_trace(y =usage_df$units[usage_df$month == '2019-02'], type = "box", name = "19-02")
  # fig7 <- fig7 %>% add_trace(y =usage_df$units[usage_df$month == '2019-03'], type = "box", name = "19-03")
  # fig7 <- fig7 %>% add_trace(y =usage_df$units[usage_df$month == '2019-04'], type = "box", name = "19-04")
  # fig7 <- fig7 %>% add_trace(y =usage_df$units[usage_df$month == '2019-05'], type = "box", name = "19-05")
  # fig7 <- fig7 %>% layout(title = 'consumption within a month')
  # fig7
  # 
  # 
  # 
  # fig6 <- plot_ly(y=usage_df$units[usage_df$season == 'Summer'], type = "box", name = "Summer")
  # fig6 <- fig6 %>% add_trace(y =usage_df$units[usage_df$season == 'Autumn'], type = "box", name = "Autumn")
  # fig6 <- fig6 %>% add_trace(y =usage_df$units[usage_df$season == 'Winter'], type = "box", name = "Winter")
  # fig6 <- fig6 %>% add_trace(y =usage_df$units[usage_df$season == 'Spring'], type = "box", name = "Spring")
  # fig6 <- fig6 %>% layout(title = 'Box plot for seasons')
  # 
  # fig6
  
  
  # fig8 <- plot_ly(data = usage_df, x = ~season, y = ~units, type ='scatter', mode = 'markers')
  # fig8 <- fig8 %>% layout(title = 'Scatter plot for seasons')
  # fig8
  
  #fig9 <- plot_ly(data = monthly_aggregated_consumption_demographics, x = ~monthly_aggregated_consumption_demographics[,c(3)], y = ~Permanent_Residents, type ='scatter', mode = 'markers')
  #fig9 <- fig9 %>% layout(title = 'no of people vs consumption')
  #fig9
  
  
  
  ##High degree: If the coefficient value lies between ± 0.50 and ± 1, 
  ##then it is said to be a strong correlation.
  
  #corr_residents_2018_06 = cor.test(monthly_aggregated_consumption_demographics$Permanent_Residents, 
  #                                  monthly_aggregated_consumption_demographics$`2018-06`)
  #corr_residents_2018_06
  
  
  
  
  # correlation check
  #corr values on top with residents
  #plot12 = corrgram(monthly_aggregated_consumption_demographics, 
  #                  lower.panel=panel.shade, upper.panel=panel.cor)
}


generate_training_dataset <- function(usage_df){
  
  #### Creating dataset for training ####
  #unmelt usage_df and rearrange columns
  training_dataset = dcast(usage_df[, c(1,3,2)], house~month )
  #adding the column to be trained and predicted for
  training_dataset = merge(metadata[,c(1, iteration_count + 1)],
                           training_dataset, by.x = "Website.Name", by.y ="house", all.y = TRUE)
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
  
  #delete the house number column 
  training_dataset = training_dataset[,c(-1)]
  #renaming cols according to month
  colnames(training_dataset)[-1] = c(paste0(month.abb[as.numeric(substr(colnames(training_dataset)[-1], 6,7))],
                                            substr(colnames(training_dataset)[-1], 1 , 4)))
  training_dataset <<- training_dataset
  #Scaling
  scaled = as.data.frame(training_dataset)
  
  cols <- colnames(training_dataset) 
  
  for ( i in 1:length(cols) ){
    
    max_dst = max(training_dataset[,cols[i]] )
    min_dst = min(training_dataset[,cols[i]] )
    
    scaled[, cols[i]] <- scale(training_dataset[,cols[i]], 
                               center = min_dst,#mean, std dev 
                               scale = max_dst - min_dst)               
  }
  scaled <<- scaled
  #scaled = training_dataset
}


