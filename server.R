function(input,output,session){
  
  output$binaryoutput <- renderPlot({
    chdu <- paste("C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/hdumaps/", "chdu", (format((Sys.Date()-2), format = "%Y%m%d")), ".tif", sep="")
    chdu.r <- rast(chdu)
    myFun <- function(x) {if_else(x >= 130, 1, 0)}
    Rn <- app(chdu.r, fun=myFun)
    tmap_mode("plot")
    tm_shape(Rn)+
      tm_raster(n=2,
                palette=get_brewer_pal(palette="Paired", plot=FALSE))
  })
  
  output$location <- renderText({
    paste("Your postcode:", input$postcode)
  })
  
  output$capital.cities <- renderTable({
    capital.df <- data.frame(capital.names, capital.chdu, preventatives)
    colnames(capital.df) <- c("Capital city", (paste(max(dseq), "'s", " status", sep="")), "Preventatives required?")
    capital.df
    })
  
  #capitalcitydata <- (capitalcities.df)
  
  output$locationplot <- renderPlot(locationplotdata())
  
  locationplotdata <- reactive({
    req(input$postcode != "")
    #postcode <- as.character(ifelse(input$postcode < 1000, paste0("0", input$postcode), input$postcode))
    postcode <- input$postcode
    z <- which(list==postcode)
    
    trial <- data.frame(dseq, {if(length(all_of(z))!=0) dplyr::select(postcodes.all, (all_of(z)))
      else return(NULL)})
    
    trial[,3] <- ifelse(trial[,2] > 130, 1, 0)
    trial[,4] <- as.numeric(format(trial[,1], format="%Y"))
    trial[,5] <- format(as.POSIXct(trial[,1]), "%m-%d")
    trial[,6] <- cut(trial[,2],
                     breaks=c(0,100,130,1000),
                     labels=c("Transmission unlikely", 
                              "Shoulder", "Transmission season"))
    
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
    
    colours <- c("Transmission season" = "brown3", "Transmission unlikely" = "blue3", 
                 "Shoulder" = "darkorange2")
    
    years <- seq(as.numeric(format(min(dseq), format="%Y")), as.numeric(format(max(dseq), format="%Y")), by=1)
    
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
    #status.df[,4] <- cut(status.df[,2],
                         #breaks = c(0, 120, 130, 1000),
                         #labels = c("Transmission unlikely", "Shoulder", "Transmission possible"))
    
    
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
    colnames(df1) <- c("Date", paste("Status for ", input$postcode, sep=""))
    colnames(df2) <- c("Date", paste("Status for ", input$postcode, sep=""))
    
    cutoff.df <- rbind(df1, df2)
    cutoff.df <- data.frame(cutoff.df[order(as.Date(cutoff.df[,1], format="%Y-%m-%d")),])
    #cutoff.df <- ifelse ((length(a)*length(b) != 0), cutoff.df, data.frame(c("Not applicable for this postcode")))
    
    colnames(cutoff.df) <- c("Date", paste("Status for ", input$postcode, sep=""))
    cutoff.df
    
  })
  outputOptions(output, 'cutofftable', suspendWhenHidden=TRUE)
  
}




