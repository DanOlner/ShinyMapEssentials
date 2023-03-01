#Installed check for package "reactlog", for visualising reactive graph
#Base "R package installed" check: https://stackoverflow.com/a/38082613/5023561
is_inst <- function(pkg) {
  nzchar(system.file(package = pkg))
}

if(is_inst("reactlog")){
  cat('Reactlog installed, enabling. Use CTRL/CMD + F3 to open reactive graph plot.\n')
  options(shiny.reactlog = TRUE)
}

# inputs ------------------------------------------------------------------


# #Based on https://shiny.rstudio.com/articles/tabsets.html
lsoa <- readRDS('data/lsoa.rds')
#load local authority level summary map data
la <- readRDS('data/localauthoritymap_w_IMDsummarydata.rds')
#
ttwa <- readRDS('data/ttwa.rds')


#FRONTIERS IN LIST FORM, EACH ELEMENT A NAMED TTWA MATCHING NAMES IN TTWA AND LSOA ABOVE
frontiers.original.list <- readRDS('data/frontiers_list.rds')

## postcode lookup 
postcode_lookup <- readRDS('data/postcode lookup table.rds')

postcode_options <- postcode_lookup$pcd_area

#Filter phi, keep values above cutoff (in function so can be set by user)
#Function loaded in global.R
x <- proc.time()
frontiers.live.list <- filter.frontiers.by.phi(frontiers.original.list, 1.96)
cat('Time to filter frontiers list: ', proc.time() - x,'\n')

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

#This fixes st_intersection not working
#Without it, we get the error described here
#(Only need to set once but keeping here for now for clarity)
#https://stackoverflow.com/a/68481205/5023561
sf::sf_use_s2(FALSE)


# fake data for app -------------------------------------------------------

la <-
  la %>%
  mutate(
    frontier_rank = sample.int(length(NAME), length(NAME)),
    IMD_rank = sample.int(length(NAME), length(NAME)),
    Dissimilarity_index = sample.int(length(NAME), length(NAME)),
    Other_index = sample.int(length(NAME), length(NAME))
    )

ttwa <- ttwa %>% 
  mutate(
    IMD_rank = sample.int(length(ttwa11nm), length(ttwa11nm)),
    Dissimilarity_index = sample.int(length(ttwa11nm), length(ttwa11nm)),
    Other_index = sample.int(length(ttwa11nm), length(ttwa11nm)),
    
    frontier_stat = sample.int(length(ttwa11nm), length(ttwa11nm)), ## high = more
    frontier_rank = (frontier_stat * -1) %>% rank(ties.method = 'first') %>% toOrdinal(),
    di_rank = (di *-1) %>% rank(ties.method = 'first') %>% toOrdinal() ## low rank = hi segregation
  )


#UI WILL HAVE WIDGET TO SELECT TYPE OF TOP LEVEL DATA, SWAP BETWEEN LA AND TTWA
#SETTING TO TTWA FOR NOW (IN SERVER)
toplevelgeog <- ttwa


# no geom la --------------------------------------------------------------

areas_no_geom <-
  lsoa
st_geometry(areas_no_geom) <- NULL



## Assign reactive value that will be used throughout
reactive_values <- 
  reactiveValues(
    area_chosen = NULL,
    most_segregated = (ttwa %>% filter(di_rank == '1st'))$ttwa[1],
    least_segregated = (ttwa %>% filter(di == min(di)))$ttwa[1],
    most_frontier = (ttwa %>% filter(frontier_rank == '1st'))$ttwa[1],
    least_frontier = (ttwa %>% filter(frontier_stat == min(frontier_stat)))$ttwa[1]
    
  )


# server.R ----------------------------------------------------------------

function(input, output, session) {
  
  
  ## reactive to update area chosen 
  ## This will set the TTWA first, from what the default in the input$area_chosen is
  observeEvent(input$area_chosen,{
    
    reactive_values$area_chosen <- input$area_chosen

    cat('area chosen observe triggered.\n')
    
    }
  )
  
  observeEvent(input$postcode_chosen,{
    
    data_chosen <- 
      (postcode_lookup %>%
      filter(pcd_area == input$postcode_chosen)
      )
    reactive_values$area_chosen <- data_chosen$ttwa[1]
    
    cat('ttwa chosen observe triggered.\n')
    
  }
  )
  
  
  ## Serverside postcode select 
  updateSelectizeInput(inputId = 'postcode_chosen',
                       choices = postcode_options,
                       selected = '',
                       server = T)
  

  ## Example write up 
  
  get_area_stats <-
    reactive({
      lsoa %>% filter(ttwa == reactive_values$area_chosen)
    })
  
  get_ttwa_tab <-
    reactive({
      ttwa %>% filter(ttwa11nm == reactive_values$area_chosen)
    })
  
  
  output$frontier_summary <-
    renderText({
      paste(
        'A plot of regions by frontier concentration is plotted opposite. ',
        reactive_values$most_frontier,
        ' has the largest concentration of social frontiers whilst ',
        reactive_values$least_frontier,
        ' has the lowest concentration of social frontiers. ',
        'Across all areas in the England and Wales, the most segregated area is ',
        reactive_values$most_segregated,
        ' (as measured by the dissimilarity index) whilst the least segregated area is ',
        reactive_values$least_segregated,
        '. '
      )
    })
  

  
  output$relationship_summary <-
    renderText({
      paste(
        'placehoder text for ',
        reactive_values$area_chosen,
        '. '
      )
    })
  
  
  
  output$write1 <-
    renderText({
      paste(
        'This document is a worked example of the social frontier analysis used in Dean et al. and the Czech paper as applied to',
        reactive_values$area_chosen,
        '. ',
        'The goal is to give a quick summary of the method and present interactive results for',
        reactive_values$area_chosen,
        '. ',
        'The latter is important for judging how accurate the routine is at guessing what where we intuitively imagine frontiers to be.',
        
        'We use data on the number of foreign-born residents in each LSOA.', 
        'There are',
        'XXX',
        'LSOAs'
        )
    })
    
  output$ttwa_writeup <-
    renderText({
      ifelse(
        is.na(reactive_values$area_chosen), 
        'No area selected or region not found',
        paste(
          reactive_values$area_chosen,
          'is the ',
          get_ttwa_tab()$di_rank[1],
          ' most segregated region (out of 173) in England and Wales according to the 2011 census. ',
          reactive_values$area_chosen,
          ' is also ranked ',
          get_ttwa_tab()$frontier_rank[1],
          '  (out of 173) for frontier density.'
          
        )
      )
    })
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  #MAP CODE------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##MAP FUNCTIONS----
  
  #Draw TTWAs; function because when zooming in or changing TTWA in other ways
  #needs redrawing without TTWA where LSOAS/frontiers are being shown
  #(And the reverse when zooming out)
  drawttwas <- function(mapdata, clearall=T){
  
    if(clearall){
      leafletProxy("map") %>%
        clearShapes()    
    } else {
      leafletProxy("map") %>% clearGroup("top level geography")
    }
    
  #Add TTWAs, if needed remove one TTWA if zoomed in to see LSOAS/frontiers
  leafletProxy("map") %>% 
    addPolygons(
      data = mapdata,
      layerId = ~ttwa11nm,
      label = ~ttwa11nm,
      fillColor = ~toplevelgeog_palette(displaycolumn),
      color = 'grey',
      weight = 3,
      opacity = 0.7,
      fillOpacity = 0.5,
      group = "top level geography",
      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
    ) 
    
  }
  
  #Draw LSOAS on leaflet for currently selected TTWA
  #Include surrounding TTWA zones (but not the one focused on)
  drawLSOAs <- function(){
    
    leafletProxy("map") %>% clearGroup("lsoas")
    leafletProxy("map") %>% clearGroup("frontiers")
    leafletProxy("map") %>% clearGroup("toplevelgeog_outline")
    leafletProxy("map") %>% clearGroup("top level geography")
    
    
    #Get TTWA map data minus one being focused on
    #Draw first, so overlaid
    mapdata <- map_df()
    
    #Remove currently selected TTWA
    mapdata <- mapdata %>% filter(ttwa11nm!=reactive_values$area_chosen)
    
    #Set scope higher so drawttwas function can use without passing
    toplevelgeog_palette <<- colorNumeric(palette="YlOrRd", domain=mapdata$displaycolumn, na.color="transparent")
    
    drawttwas(mapdata, clearall = F)
    
    
    
    leafletProxy('map') %>% 
      addPolygons(
        data = lsoa %>% filter(ttwa==reactive_values$area_chosen),
        fillColor = ~lsoapalette(UKborn_percent),
        color = 'black',
        weight = 0.2,
        opacity = 1,
        fillOpacity = 0.5,
        group = "lsoas"
      ) %>% 
      addPolylines(
        data = frontiers.live.list[[reactive_values$area_chosen]],
        color = 'black',
        weight = 3,
        opacity = 1,
        group = "frontiers"
      ) %>%
      addPolygons(
        data = toplevelgeog %>% filter(ttwa11nm == reactive_values$area_chosen),
        fill = F,
        color = 'white',
        weight = 8,
        opacity = 1,
        group = "toplevelgeog_outline"
      ) 
    
  }
  
  
  #Find TTWA under map's current centre point
  findTTWAunderMapCentrePoint <- function(){
    
    #Find TTWA under centre point
    centerpoint = st_sfc(x = st_point(c(input$map_center[[1]],input$map_center[[2]])), crs = st_crs(toplevelgeog))
    
    #TTWA under the central point
    toplevelgeog_underpoint <- st_intersection(toplevelgeog, centerpoint)
    
    print(toplevelgeog_underpoint$ttwa11nm)
    
    reactive_values$area_chosen <- toplevelgeog_underpoint$ttwa11nm
    
  }
  
  
  
  
  

  #User can choose which data column will be shown in the top level geography
  #Subset TTWA data to the appropriate column (from the sf dataframe columns)
  map_df = reactive({
    
    #Select just the one column to display, plus the top level geog name for the layer ID etc
    #CURRENTLY FIXED TO THIS TTWA NAME, not ideal
    x <- toplevelgeog %>% select(input$toplevel_varname_to_display_on_map,ttwa11nm)
    
    #rename to displaycolumn so it's the same each time when updated
    #(May be a better way to do this)
    names(x)[names(x)==input$toplevel_varname_to_display_on_map] <- 'displaycolumn'
    
    return(x)
    
  })
  
  
  ##LEAFLET REACTIVES----
  
  #Initial map output creation (static elements only, dynamic changes in observes / leafletproxy)
  #See https://rstudio.github.io/leaflet/shiny.html
  output$map <- renderLeaflet({
    
    #Only static elements, observe below will do the dynamics
    #Set zoom fractional jumps for a bit more zoom control
    #https://stackoverflow.com/a/62863122/5023561
    leaflet(options = leafletOptions(zoomSnap = 0.1, zoomDelta=0.1)) %>%
      addTiles() %>%
      setView(lng = -2, lat = 53, zoom = 7.2)
    
  })
  
  #https://stackoverflow.com/a/62701468/5023561
  #For making sure data loads to map on initial load
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  
  
  
  #Add initial dynamic elements, the TTWA choropleth overlay
  observe({
    
    cat("Leaflet proxy call.")
    
    #Change map when variable changed
    #See https://rstudio.github.io/leaflet/shiny.html -
    #Avoids redrawing whole map after each change
    
    #Only call back to map_df reactive once (though it's cached unless input changes, so shouldn't matter...)
    mapdata <- map_df()
    
    #Reactively change palette if change of top level variable
    #https://rstudio.github.io/leaflet/choropleths.html
    #https://r-graph-gallery.com/183-choropleth-map-with-leaflet.html
    
    #Set scope higher so drawttwas function can use without passing
    toplevelgeog_palette <<- colorNumeric(palette="YlOrRd", domain=mapdata$displaycolumn, na.color="transparent")
    
    drawttwas(mapdata)
    
  })
  
  #Click on top level geography sets area_chosen input in summary tab
  #https://stackoverflow.com/a/54433520/5023561
  observe({
    
    event <- input$map_shape_click
    print( event )
    updateSelectInput(session, inputId = "area_chosen", selected = event$id
    )
    
  }) 

  
  observeEvent(input$map_zoom, {
    
    zoomvalue <<- input$map_zoom
    cat("Observe zoom, map zoom: ",zoomvalue,"\n")
    
    #Change view based on zoom
    #Zoom out is purely TTWA
    #Zoom in puts LSOAs / frontiers at centre of map but keeps surrounding TTWAs
    if(zoomvalue <= 9){
      
      LSOAzoomlevel <<- FALSE
      
      leafletProxy("map") %>% clearGroup("lsoas")
      leafletProxy("map") %>% clearGroup("frontiers")
      leafletProxy("map") %>% clearGroup("toplevelgeog_outline")
      
      mapdata <- map_df()
      
      #Reactively change palette if change of top level variable
      #https://rstudio.github.io/leaflet/choropleths.html
      #https://r-graph-gallery.com/183-choropleth-map-with-leaflet.html
      
      #Set scope higher so drawttwas function can use without passing
      toplevelgeog_palette <<- colorNumeric(palette="YlOrRd", domain=mapdata$displaycolumn, na.color="transparent")
      
      cat('Attempting map zoom out TTWA re-draw\n')
      drawttwas(mapdata)
      
      lastzoomvalue = zoomvalue
      
    } else if(!LSOAzoomlevel)  {
      
      #Set outside for loop scope
      LSOAzoomlevel <<- TRUE
      
      findTTWAunderMapCentrePoint()
      
      drawLSOAs()
      
      # leafletProxy('map')%>% 
      #   addPolygons(
      #     data = lsoa %>% filter(ttwa==reactive_values$area_chosen),
      #     fillColor = ~lsoapalette(UKborn_percent),
      #     color = 'black',
      #     weight = 0.2,
      #     opacity = 1,
      #     fillOpacity = 0.5,
      #     group = "lsoas"
      #   ) %>% 
      #   addPolylines(
      #     data = frontiers.live.list[[reactive_values$area_chosen]],
      #     color = 'black',
      #     weight = 3,
      #     opacity = 1,
      #     group = "frontiers"
      #   ) %>%
      #   addPolygons(
      #     data = toplevelgeog %>% filter(ttwa11nm == reactive_values$area_chosen),
      #     fill = F,
      #     color = 'white',
      #     weight = 8,
      #     opacity = 1,
      #     group = "toplevelgeog_outline"
      #   ) 
      
    }
    
  })
  
  
  #Dragend event not implemented in r/leaflet
  #But MAPID_bounds and MAPID_center is triggered at the end of a drag, phew
  observeEvent(input$map_center, {
    
    cat("Observe center map point: ")
    
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
      
      #TTWA under the central point
      toplevelgeog_underpoint <- st_intersection(toplevelgeog, centerpoint)
      
      print(toplevelgeog_underpoint$ttwa11nm)
      
      #Chance the centerpoint check may not have picked up a higher level geog
      #If so, keep the older one so app doesn't break
      #Older one will be kept by default, just need to not act
      # cat("length of top level geography filter: ", length(toplevelgeog_underpoint$ttwa11nm))
      
      #If top level geography if different from last drag
      #Update the LSOAs underneath
      #Note, needs length check first cos if conditional can't cope with length zero, like a numpty
      if(length(toplevelgeog_underpoint$ttwa11nm) > 0){
      
        if(lastTopLevelGeography != toplevelgeog_underpoint$ttwa11nm){
          
          cat("Updating geography\n")
          
          #Set outside of scope
          lastTopLevelGeography <<- toplevelgeog_underpoint$ttwa11nm
        
          reactive_values$area_chosen <- toplevelgeog_underpoint$ttwa11nm
          
          drawLSOAs()
          
          # leafletProxy("map") %>% clearGroup("lsoas")
          # leafletProxy("map") %>% clearGroup("frontiers")
          # leafletProxy("map") %>% clearGroup("toplevelgeog_outline")
          # 
          # leafletProxy('map') %>% 
          #   addPolygons(
          #     data = lsoa %>% filter(ttwa==toplevelgeog_underpoint$ttwa11nm),
          #     fillColor = ~lsoapalette(UKborn_percent),
          #     color = 'black',
          #     weight = 0.2,
          #     opacity = 1,
          #     fillOpacity = 0.5,
          #     group = "lsoas"
          #   ) %>% 
          #   addPolylines(
          #     data = frontiers.live.list[[toplevelgeog_underpoint$ttwa11nm]],
          #     color = 'black',
          #     weight = 3,
          #     opacity = 1,
          #     group = "frontiers"
          #   ) %>%
          #   addPolygons(
          #     data = toplevelgeog %>% filter(ttwa11nm == toplevelgeog_underpoint$ttwa11nm),
          #     fill = F,
          #     color = 'white',
          #     weight = 8,
          #     opacity = 1,
          #     group = "toplevelgeog_outline"
          #   ) 

          
        
        }#end if lastTopLevelGeography
        
      }#end if length
      
    }#end if LSOA zoom level
    
    
  })
  
  
  
  # Download handlers -----------------
  output$download_app_data <-
    downloadHandler(
      filename = function(){
        paste('data.zip', sep = '')
      },
      content = function(con){
        file.copy('data/data-frontiers.zip', con)
      },
      contentType = 'application/zip'
    )
  
  ## Generaate reactive inputs ----
  
  output$inputs <- renderPrint({
    print(input)
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(
      get_area_stats()
    )
  })
  
  ## generate plots -----
  source('plot_widgets.R')
  output$plot <-
    renderPlotly({
      scatter_widget(data = la)
    })
  
  # Generate an HTML table view of the data ----
  source('table_widget.R')
  output$table <- DT::renderDataTable({
    table_widget(
      areas_no_geom
      )
  })
  
}
