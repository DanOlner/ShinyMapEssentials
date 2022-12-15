#Based on https://shiny.rstudio.com/articles/tabsets.html


function(input, output) {
  
  #User can choose which data column will be shown
  #Subset LA data to the appropriate column
  map_df = reactive({
    
    #Select just the one column to display
    x <- la %>% select(input$la_varname_to_display_on_map)
    
    #rename to displaycol so it's the same each time when updated
    #(May be a better way to do this)
    names(x)[names(x)==input$la_varname_to_display_on_map] <- 'displaycol'
    
    x
    
  })
  
  #Check what map_df is producing
  observeEvent(map_df(), {
    #print(paste0(map_df() %>% st_crs()))
    print(map_df() %>% select('displaycol'))
  })
  
  output$map <- renderLeaflet({
    
    # Create a color palette for the map
    #https://rstudio.github.io/leaflet/choropleths.html
    #https://r-graph-gallery.com/183-choropleth-map-with-leaflet.html
    mypalette <- colorNumeric(palette="YlOrRd", domain=map_df()$displaycol, na.color="transparent")
    #mypalette(c(45,43))
    
    leaflet(map_df()) %>%
      addTiles() %>%
      setView(lng = -2, lat = 53, zoom = 6) %>% 
      addPolygons(
        fillColor = ~mypalette(displaycol)
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