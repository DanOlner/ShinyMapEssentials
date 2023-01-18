#Based on https://shiny.rstudio.com/articles/tabsets.html


function(input, output) {
  
  #User can choose which data column will be shown
  #Subset LA data to the appropriate column
  map_df = reactive({
    
    #Select just the one column to display
    x <- la %>% select(input$la_varname_to_display_on_map)
    
    #rename to displaycolumn so it's the same each time when updated
    #(May be a better way to do this)
    names(x)[names(x)==input$la_varname_to_display_on_map] <- 'displaycolumn'
    
    return(x)
    
  })
  
  #Check what map_df is producing
  observeEvent(map_df(), {
    
    #print(paste0(map_df() %>% st_crs()))
    # print(map_df() %>% select('displaycolumn'))
    
    #"input$MAPID_zoom is an integer that indicates the zoom level"
    #Is empty on initialisation
    #https://rstudio.github.io/leaflet/shiny.html
    print(cat("Map zoom: ",input$map_zoom,"\n"))
    
    
    #Length arg cos NULL evaluates as logical(0) and throws an error. Duh.
    #https://stackoverflow.com/a/27351392
    # if(!is.null(input$map_zoom) & length(input$map_zoom) > 0){
    #   
    #   print("ping!")
    #   
    #   if(input$map_zoom == 7) print("7 integer")
    #   
    # }
    
  })
  
  observeEvent(input$map_zoom, {
    
    print("ping!")
    
  })
  
  #Initial map output creation (static elements only, dynamic changes in observes / leafletproxy)
  #See https://rstudio.github.io/leaflet/shiny.html
  output$map <- renderLeaflet({
    
    #Only static elements, observe below will do the dynamics
    leaflet() %>%
      addTiles() %>%
      setView(lng = -2, lat = 53, zoom = 6) 
    
  })
  
  
  
  #
  observe({
    
    #Change map when variable changed
    #See https://rstudio.github.io/leaflet/shiny.html - 
    #Avoids redrawing whole map after each change
   
    # Create a color palette for the map
    #https://rstudio.github.io/leaflet/choropleths.html
    #https://r-graph-gallery.com/183-choropleth-map-with-leaflet.html
    mypalette <- colorNumeric(palette="YlOrRd", domain=map_df()$displaycolumn, na.color="transparent")
    #mypalette(c(45,43))
    
    leafletProxy("map", data = map_df()) %>%
      clearShapes() %>% 
      addPolygons(
        fillColor = ~mypalette(displaycolumn)
      )
     
  })
  
  
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    d()
  })
  
}