
# inputs ------------------------------------------------------------------


# #Based on https://shiny.rstudio.com/articles/tabsets.html
lsoa <- readRDS('data/lsoa_layer_w_ttwalookup.rds')
#load local authority level summary map data
la <- readRDS('data/localauthoritymap_w_IMDsummarydata.rds')
#
ttwa <- readRDS('data/ttwa_engwales.rds')

#Separate zoom value - copy to when changes from input$map_zoom
#Why? because input$map_zoom is NULL on first loading
#And this avoids having to do double null / length test
zoomvalue = 6

#Flag for when at zoom level for drawing LSOAs
#(So as to only draw once)
LSOAzoomlevel = FALSE

#Update underlying LSOAs if centred top level geog changes
#(And if at zoom level at which LSOAs are being shown)
#Give a name  so if test doesn't complain about zero length
lastTopLevelGeography = "none"


#Palettes for LSOAs and top level geographies
#(Other style elements done in leaflet code below)
#Top level palette needs doing reactively, as domain will change when variable changes
#(Will be true of LSOA palette too if/when changing to more than one selectable variable)
lsoapalette <- colorNumeric(palette="RdYlBu", domain=lsoa$UKborn_percent, na.color="transparent")


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

ttwa <- ttwa %>% 
  mutate(
    IMD_rank = sample.int(length(ttwa11nm), length(ttwa11nm)),
    Dissimilarity_index = sample.int(length(ttwa11nm), length(ttwa11nm)),
    Other_index = sample.int(length(ttwa11nm), length(ttwa11nm))
  )

#UI WILL HAVE WIDGET TO SELECT TYPE OF TOP LEVEL DATA, SWAP BETWEEN LA AND TTWA
#SETTING TO TTWA FOR NOW (IN SERVER)
toplevelgeog <- ttwa


# server.R ----------------------------------------------------------------


function(input, output) {
  
  ## Data -- map_df() is a function which returns data to be used elsewhere
  #User can choose which data column will be shown
  #Subset LA data to the appropriate column
  map_df = reactive({
    
    #Select just the one column to display
    x <- toplevelgeog %>% select(input$toplevel_varname_to_display_on_map)
    
    #rename to displaycolumn so it's the same each time when updated
    #(May be a better way to do this)
    names(x)[names(x)==input$toplevel_varname_to_display_on_map] <- 'displaycolumn'
    
    return(x)
    
  })
  
  ## map_no_geom 
  map_df_no_geom = reactive({
    
    x <- map_df()
    
    st_geometry(x) <- NULL

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
  
  observeEvent(input$map_zoom, {
    
    zoomvalue = input$map_zoom
    cat("Map zoom: ",zoomvalue,"\n")
    
    #Hide based on zoom
    #This code runs also in main map observe; must be way to avoid duplication
    if(zoomvalue <= 9){
      
      LSOAzoomlevel <<- FALSE
      
      leafletProxy("map") %>% showGroup("top level geography")
      leafletProxy("map") %>% clearGroup("lsoas")
      
      lastzoomvalue = zoomvalue
      
    } else if(!LSOAzoomlevel)  {
      
      #Set outside for loop scope
      LSOAzoomlevel <<- TRUE
      
      leafletProxy("map") %>% hideGroup("top level geography")
      
      leafletProxy('map')%>% 
        addPolygons(
          # data = lsoa %>% filter(ttwa=='Sheffield & Rotherham'),
          data = lsoa %>% filter(ttwa=='London'),
          fillColor = ~lsoapalette(UKborn_percent),
          color = 'black',
          weight = 0.2,
          opacity = 1,
          fillOpacity = 0.5,
          group = "lsoas"
        )
      
    }
    
  })
  
  
  #Dragend event not implemented in r/leaflet
  #But MAPID_bounds and MAPID_center is triggered at the end of a drag, phew
  observeEvent(input$map_center, {
    
    #https://rstudio.github.io/leaflet/shiny.html
    #lat and lon in a list
    cat(input$map_center[[1]],",",input$map_center[[2]],"\n")
    
    
    #If we're at LSOA zoom level
    #We want to check we're still displaying the same LSOA set
    #Or whether we need to change
    if(LSOAzoomlevel){
    
      #Find top level geography underneath this point
      #It's not necessarily shown, so use the sf itself
      centerpoint = st_sfc(x = st_point(c(input$map_center[[1]],input$map_center[[2]])), crs = st_crs(toplevelgeog))
      
      #This works. Huh.
      #Without it, we get the error described here
      #(Only need to set once but keeping here for now for clarity)
      #https://stackoverflow.com/a/68481205/5023561
      sf::sf_use_s2(FALSE)
      
      #TTWA under the central point
      toplevelgeog_underpoint <- st_intersection(toplevelgeog, centerpoint)
      
      print(toplevelgeog_underpoint)
      
      #If top level geography if different from last drag
      #Update the LSOAs underneath
      if(lastTopLevelGeography != toplevelgeog_underpoint$ttwa11nm){
        
        cat("Updating geography\n")
        
        #Set outside if scope
        lastTopLevelGeography <<- toplevelgeog_underpoint$ttwa11nm
      
        leafletProxy("map") %>% clearGroup("lsoas")
        
        leafletProxy('map') %>% 
          addPolygons(
            data = lsoa %>% filter(ttwa==toplevelgeog_underpoint$ttwa11nm),
            fillColor = ~lsoapalette(UKborn_percent),
            color = 'black',
            weight = 0.2,
            opacity = 1,
            fillOpacity = 0.5,
            group = "lsoas"
          )
      
      }#end if lastTopLevelGeography
      
    }#end if LSOA zoom level
    
    
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
    
    #Reactively change palette if change of top level variable
    #https://rstudio.github.io/leaflet/choropleths.html
    #https://r-graph-gallery.com/183-choropleth-map-with-leaflet.html
    toplevelgeog_palette <- colorNumeric(palette="YlOrRd", domain=map_df()$displaycolumn, na.color="transparent")
   
    #Add local authorities AND lsoas and then selectively hide based on zoom (faster than loading each time?)
    leafletProxy("map") %>%
      clearShapes() %>% 
      addPolygons(
        data = map_df(),
        fillColor = ~toplevelgeog_palette(displaycolumn),
        color = 'grey',
        weight = 3,
        opacity = 0.7,
        fillOpacity = 0.5,
        group = "top level geography"
      ) 
      
     
  })
  
  
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(map_df_no_geom())
  })
  
  ## generate plot -----
  source('plot_widgets.R')
  output$plot <-
    renderPlotly({
      density_widget(data = la, xVar = input$la_varname_to_display_on_map)
    })
  
  # Generate an HTML table view of the data ----
  source('table_widget.R')
  output$table <- DT::renderDataTable({
    table_widget(
      map_df_no_geom()
      )
  })
  
}
