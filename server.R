function(input,output,session){
  
  output$location <- renderText({
    paste("Your postcode:", input$postcode)
  })
  
  output$location.table <- renderTable(locationdata())
  
  output$capital_cities <- renderTable(capitalcitiesdata())
  
  #output$cutoff.table <- renderTable(cutofftabledata())

  # cutofftabledata <- reactive({cutoff.df %>% filter(dseq >= as.Date(input$dates[1]), 
  #                                                   dseq <= as.Date(input$dates[2]))
  # 
  # })
  
  
  locationdata <- reactive({
    req(input$postcode != "")
    postcode <- ifelse(input$postcode < 1000, paste0("0", input$postcode), input$postcode)
    z <- which(list==postcode)
    
    #pauses the application - always remove when done
    #browser()
    
    #output data
    data.frame(dseq, {if(length(z)!=0) select(postcodes.all, (z+1))
      else return(NULL)})
    
    # if(length(z)!=0) select(postcodes.all, z)
    # else return(NULL)
      #(postcodes %>% select(z))


  })
  
  #help! Cannot get table to display in ui
  capitalcitiesdata <- reactive({
    capitalcities.df})
  
    
  
  
  #   filter(postcodes, between(date,
  #                             as.Date(input$dates[1]),
  #                             as.Date(input$dates[2]))
  # )

  }

  


# 
# cutofftabledata <- reactive({
#   postcode <- ifelse(postcode < 1000, paste0("0", postcode), postcode)
#   z <- which(list==postcode)
#   data.frame(select(postcodes, z))