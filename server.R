function(input,output,session){
  
  output$selecteddatemap <- renderText({
    (paste("As of", format(input$dates, format = "%d %b %Y"), 
          ", where can heartworm be transmitted?"))
  })
  
  
  output$binaryoutput <- renderPlot({
    chdu <- paste("C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/hdumaps/", 
                  "chdu", format(input$dates, format = "%Y%m%d"), 
                  ".tif", sep="")
    
    chdu.r <- rast(chdu)
    
    bbox_new <- st_bbox(chdu.r)
    xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
    yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
    
    # bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
    bbox_new[3] <- bbox_new[3] + (0.15 * xrange) # xmax - right
    # bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
    bbox_new[4] <- bbox_new[4] + (0.15 * yrange) # ymax - top
    bbox_new <- bbox_new %>%  # take the bounding box ...
      st_as_sfc() #and make it a sf polygon
    
    # subs.mat <- matrix(data=c(0, 120, 0, 120, 130, 1, 130, 1000, 2),
    #                    ncol=3, byrow=TRUE)
    # Rn <- classify(chdu.r, subs.mat, include.lowest=TRUE, right=FALSE)
    pal <-c("royalblue3", "goldenrod2","firebrick3")
    
    tmap_mode("plot")
    tm_shape(chdu.r, bbox=bbox_new)+
      tm_raster(n=3,
                palette=pal,
                breaks = c(0, 120, 130, Inf))+
      tm_layout(legend.position = c("right", "top"), 
                title= paste('30-day cumulative HDUs on', input$dates, sep=" "), 
                title.position = c('right', 'top'))
  })
  
  output$summaryImage <- renderImage({
    
    filename <- normalizePath(file.path('./www',
                                        paste(input$summaryselection)))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", input$summaryselection),
         #height=400,
         width="100%")
    
  }, deleteFile = FALSE)
  

  
  output$location <- renderText({
    paste("Your postcode:", input$postcode)
  })
  
  output$capital.cities <- renderTable({
    capital.df <- data.frame(capital.names, capital.chdu)
    colnames(capital.df) <- c("Capital city", (paste(max(dseq), "'s", " status", sep="")))
    capital.df
    })
  
  output$locationplot <- renderPlot(locationplotdata())
  
  locationplotdata <- reactive({
    req(input$postcode != "")
    postcode <- input$postcode
    z <- which(list==postcode)
    
    trial <- data.frame(dseq, {if(length(all_of(z))!=0) dplyr::select(postcodes.all, (all_of(z)))
      else return(NULL)})
    
    trial[,3] <- ifelse(trial[,2] > 130, 1, 0)
    trial[,4] <- as.numeric(format(trial[,1], format="%Y"))
    trial[,5] <- format(as.POSIXct(trial[,1]), "%m-%d")
    trial[,6] <- cut(trial[,2],
                     breaks=c(0,120,130,1000),
                     labels=c("Transmission unlikely", 
                              "Shoulder", "Transmission season"))
    trial[,7] <- str_c(get_fy(trial[,1], offset_period = -1),"/",get_fy(trial[,1]))
    trial[,8] <- day_of_year(trial[,1], type = "financial")
    
    yearbreaks <- seq((as.numeric(format(min(dseq), format="%Y"))+0.5), 
                      (as.numeric(format(max(dseq), format="%Y"))+0.5), by=1)
    
    
    f <- seq.Date(min(dseq), max(dseq), by="month")
    f <- as.Date(format(f, format="%m-%d"), format="%m-%d")
    
    g <- (seq.Date(min(dseq), max(dseq), by="month"))+14
    g <- as.Date(format(g, format="%m-%d"), format="%m-%d")
    
    fnx = function(x) {
      unlist(strsplit(as.character(x), '[19|20][0-9]{2}-', fixed=FALSE))[2]
    }
    
    dm1 = sapply(f, fnx)
    dm2 = sapply(g, fnx)
    
    new_col = c(as.factor(dm1), as.factor(dm2))
    
    colours <- c("Transmission season" = "firebrick3", "Transmission unlikely" = "royalblue3", 
                 "Shoulder" = "goldenrod2")
    
    years <- seq(as.numeric(format(min(dseq), format="%Y")), as.numeric(format(max(dseq), format="%Y")), by=1)
    
    #calendar year
    ggplot(trial, aes(trial[,5], y=trial[,4]))+
      geom_tile(aes(fill=trial[,6]))+
      scale_fill_manual(values=colours)+
      geom_hline(yintercept=yearbreaks)+
      scale_y_reverse(breaks=years)+
      scale_x_discrete(breaks=new_col)+
      labs(title=postcode, x="Date", y="Year", fill="Status")+
      theme(plot.title= element_text(face="bold", size=20),
            axis.title.x = element_text(face="bold", size=16),
            axis.text.x = element_text(size=14),
            axis.title.y = element_text(face="bold", size=16),
            axis.text.y = element_text(size=14),
            legend.title = element_text(face="bold", size=16),
            legend.position = "bottom",
            legend.text = element_text(size=14))
    
    #financial year
    # ggplot(trial, aes(trial[,8], y=trial[,7]))+
    #   geom_tile(aes(fill=trial[,6]))+
    #   scale_fill_manual(values=colours)+
    #   geom_hline(yintercept=yearbreaks)+
    #   scale_y_reverse(breaks=years)+
    #   scale_x_discrete(breaks=new_col)+
    #   labs(title=postcode, x="Date", y="Year", fill="Status")+
    #   theme(plot.title= element_text(face="bold", size=20),
    #         axis.title.x = element_text(face="bold", size=16),
    #         axis.text.x = element_text(size=14),
    #         axis.title.y = element_text(face="bold", size=16),
    #         axis.text.y = element_text(size=14),
    #         legend.title = element_text(face="bold", size=16),
    #         legend.position = "bottom",
    #         legend.text = element_text(size=14))
    
  })
  
  output$dailystatus <- renderText(statusdata())
  
  statusdata <- reactive({
    req(input$postcode != "")
    postcode <- input$postcode
    z <- which(list==postcode)
    
    todaystatus.df <- data.frame(dseq, {if(length(all_of(z))!=0) dplyr::select(postcodes.all, (all_of(z)))
      else return(NULL)})
    todaystatus.df[,2] <- round(todaystatus.df[,2], digits=2)
    todaystatus.df[,3] <- cut(todaystatus.df[,2],
                              breaks=c(0,120,130,1000),
                              labels=c(", transmission is unlikely",
                                       ", the transmission season may be starting soon if there is more warm weather", ", transmission is possible, consult your veterinarian"))
    t <- paste((todaystatus.df[nrow(todaystatus.df),2]), "HDUs", todaystatus.df[nrow(todaystatus.df),3], sep="")
    t <- as.character(t)
    t
    
   })
  
  output$cutofftable <- renderDataTable(cutoffdata())
  
  cutoffdata <- reactive({
    req(input$postcode != "")
    postcode <- input$postcode
    z <- which(list==postcode)
    
    status.df <- data.frame(dseq, {if(length(all_of(z))!=0) dplyr::select(postcodes.all, (all_of(z)))
      else return(NULL)})
    
    status.df[,3] <- ifelse(status.df[,2] > 130, 1, 0)
    status.df[,4] <- NA
   
    for (n in 1:nrow(status.df)){
      status.df[n,4] <- ifelse(status.df[(n+1),2] < 130 & status.df[n,2]>130, 1, 0)
    
    }
    
    a <- which(status.df[,4]==1)
    
    cutoffdata <- data.frame(status.df[a,1], "season stops")
    
    status.df[,5] <- NA
    status.df[,5] <- ifelse(status.df[,2] < 130, 1, 0)
    status.df[,6] <- NA
    
    for (n in 1:nrow(status.df)){
      status.df[n,6] <- ifelse(status.df[(n+1),2] > 130 & status.df[n,2]<130, 1, 0)
      
    }
    
    b <- which(status.df[,6]==1)
    
    df1 <- data.frame(status.df[b,1], "season starts")
    df2 <- data.frame(status.df[a,1], "season stops")
    
    #how many days since transmission was possible?
    for (k in 1:(nrow(df1))){
      df1[k,3] <- day_of_year(df1[k,1]) - day_of_year(df2[k,1])
      
    }
    
    df2[,3] <- c("NA")
    
    colnames(df1) <- c("Date", paste("Status for ", input$postcode, sep=""), "Days since last transmission")
    colnames(df2) <- c("Date", paste("Status for ", input$postcode, sep=""), "Days since last transmission")
    
    cutoff.df <- rbind(df1, df2)
    cutoff.df <- data.frame(cutoff.df[order(as.Date(cutoff.df[,1], format="%Y-%m-%d")),])
    #cutoff.df <- ifelse ((length(a)*length(b) != 0), cutoff.df, data.frame(c("Not applicable for this postcode")))
    
    colnames(cutoff.df) <- c("Date", paste("Status for ", input$postcode, sep=""), "Days since last transmission")
    cutoff.df
    
    
    
  })
  outputOptions(output, 'cutofftable', suspendWhenHidden=TRUE)
  
  output$percentagetable <- renderDataTable(percentagetabledata())
    
  percentagetabledata <- reactive({
    req(input$postcode != "")
    postcode <- input$postcode
    z <- which(list==postcode)
    
    perc.df <- data.frame(dseq, {if(length(all_of(z))!=0) dplyr::select(postcodes.all, (all_of(z)))
      else return(NULL)})
    
    perc.df[,3] <- ifelse(perc.df[,2] > 130, 1, 0)
    perc.df[,4] <- NA
    
    for (n in 1:nrow(perc.df)){
      perc.df[n,4] <- ifelse(perc.df[(n+1),2] < 130 & perc.df[n,2]>130, 1, 0)
      
    }
    
    a <- which(perc.df[,4]==1)
    
    percdata <- data.frame(perc.df[a,1], "season stops")
    
    perc.df[,5] <- NA
    perc.df[,5] <- ifelse(perc.df[,2] < 130, 1, 0)
    perc.df[,6] <- NA
    
    for (n in 1:nrow(perc.df)){
      perc.df[n,6] <- ifelse(perc.df[(n+1),2] > 130 & perc.df[n,2]<130, 1, 0)
      
    }
    
    b <- which(perc.df[,6]==1)
    
    df1 <- data.frame(perc.df[b,1], "season starts")
    df2 <- data.frame(perc.df[a,1], "season stops")
    
    
    for (k in 1:(nrow(df1))){
      c <- day_of_year(df1[k,1]) - day_of_year(df2[k,1])
      df1[k,3] <- ifelse(c>=0, c, NA)
      #df1[k,3] <- day_of_year(df1[k,1]) - day_of_year(df2[k,1])
      
    }
    
    df2[,3] <- c(NA)

    percentagetabledata <- data.frame(yseq.df[,1], NA)
      for (i in 1:length(yseq.df[,1])){
        
        tdf <- subset(df1, format(as.Date(df1[,1]), "%Y")==yseq.df[i,1])
        percentagetabledata[i,2] <- (yseq.df[i,2] - sum(tdf[,3]))/yseq.df[i,2] * 100

      }
    
   percentagetabledata[,2] <- as.numeric(percentagetabledata[,2])
   percentagetabledata[,2] <- round(percentagetabledata[,2], digits=2)
   
   percentagetabledata[nrow(percentagetabledata),2] <- ifelse(yseq.df[nrow(yseq.df),2] >= 365, percentagetabledata[nrow(percentagetabledata),2], NA)
   
    colnames(percentagetabledata) <- c("Year", "Percentage of the year at risk")
    percentagetabledata
    
  })
    outputOptions(output, 'percentagetable', suspendWhenHidden=TRUE)
    
}




