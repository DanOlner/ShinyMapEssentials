
# inputs ------------------------------------------------------------------


# #Based on https://shiny.rstudio.com/articles/tabsets.html
lsoa <- readRDS('data/LSOAs_plus_IMD2015_19_plusLAlookup.rds')
#load local authority level summary map data
la <- readRDS('data/localauthoritymap_w_IMDsummarydata.rds')

#Separate zoom value - copy to when changes from input$map_zoom
#Why? because input$map_zoom is NULL on first loading
#And this avoids having to do double null / length test
zoomvalue = 6



# frontiers_data <- 
#   readRDS('data/frontier borders layer.rds')
# 
# lsoa_data_2 <- 
#   readRDS('data/lsoa layer.rds')
# ttwa_data <-
#   readRDS('data/ttwa 2011 layer.rds')
# 

# fake data for app -------------------------------------------------------

la <-
  la %>%
  mutate(
    IMD_rank = sample.int(length(NAME), length(NAME)),
    Dissimilarity_index = sample.int(length(NAME), length(NAME)),
    Other_index = sample.int(length(NAME), length(NAME))
    )


# no geom la --------------------------------------------------------------

areas_no_geom <-
  la
st_geometry(areas_no_geom) <- NULL

# server.R ----------------------------------------------------------------


function(input, output) {
  
  ## Data -- map_df() is a function which returns data to be used elsewhere
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
    
    #Can I set manually? (Is NULL on first load and length for if test is zero, see below)
    #Newp, is read-only
    #input$map_zoom = 6
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
  
  # observeEvent(input$map_zoom, {
  #   
  #   zoomvalue = input$map_zoom
  #   print(cat("Map zoom: ",zoomvalue,"\n"))
  #   
  #   #Hide based on zoom
  #   #This code runs also in main map observe; must be way to avoid duplication
  #   if(zoomvalue <= 9){
  #     
  #     leafletProxy("map") %>% hideGroup("lsoas")
  #     leafletProxy("map") %>% showGroup("local authorities")
  #     
  #   } else {
  #     
  #     leafletProxy("map") %>% hideGroup("local authorities")
  #     leafletProxy("map") %>% showGroup("lsoas")
  #     
  #   }
  #   
  # })
  
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
    
    lsoapalette <- colorNumeric(palette="RdYlBu", domain=lsoa$Rank2019, na.color="transparent")
    
    
    #Add local authorities AND lsoas and then selectively hide based on zoom (faster than loading each time?)
    leafletProxy("map") %>%
      clearShapes() %>% 
      addPolygons(
        data = map_df(),
        fillColor = ~mypalette(displaycolumn),
        color = 'grey',
        weight = 3,
        opacity = 0.7,
        group = "local authorities"
      ) #%>% 
      # addPolygons(
      #   data = lsoa,
      #   fillColor = ~lsoapalette(Rank2019),
      #   color = 'black',
      #   weight = 1,
      #   opacity = 0.7,
      #   group = "lsoas"
      # )
    
    
    #Initial zoom of 6, just show local authorities  
    #leafletProxy("map") %>% hideGroup("lsoas")
      
     
  })
  
  
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(areas_no_geom)
  })
  
  ## generate plot -----
  source('plot_widgets.R')
  output$plot <-
    renderPlotly({
      density_widget(data = areas_no_geom, xVar = input$la_varname_to_display_on_map)
    })
  
  # Generate an HTML table view of the data ----
  source('table_widget.R')
  output$table <- DT::renderDataTable({
    table_widget(
      areas_no_geom
      )
  })
  
}
