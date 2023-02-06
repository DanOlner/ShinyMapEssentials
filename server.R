
# inputs ------------------------------------------------------------------


# #Based on https://shiny.rstudio.com/articles/tabsets.html
lsoa <- readRDS('data/lsoa.rds')
#load local authority level summary map data
la <- readRDS('data/localauthoritymap_w_IMDsummarydata.rds')
#
ttwa <- readRDS('data/ttwa.rds')

#FRONTIERS IN LIST FORM, EACH ELEMENT A NAMED TTWA MATCHING NAMES IN TTWA AND LSOA ABOVE
frontiers.original.list <- readRDS('data/frontiers_list.rds')

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
    Other_index = sample.int(length(ttwa11nm), length(ttwa11nm))
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
    area_chosen = NULL
  )

# server.R ----------------------------------------------------------------


function(input, output) {
  
  
  ## reactive to update area chosen 
  observeEvent(input$area_chosen,{reactive_values$area_chosen <- input$area_chosen}
  )
  
  ## Example write up 
  
  get_area_stats <-
    reactive({
      lsoa %>% filter(ttwa == reactive_values$area_chosen)
    })
  
  output$write1 <-
    renderText({
      paste(
        'This document is a worked example of the social frontier analysis used in Dean et al. and the Czech paper as applied to',
        reactive_values$area_chosen,
        'The goal is to give a quick summary of the method and present interactive results for',
        reactive_values$area_chosen,
        'The latter is important for judging how accurate the routine is at guessing what where we intuitively imagine frontiers to be.',
        
        'We use data on the number of foreign-born residents in each LSOA.', 
        'There are',
        'XXX',
        'LSOAs'
        )
    })
    


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
  

  
  observeEvent(input$map_zoom, {
    
    zoomvalue <<- input$map_zoom
    cat("Observe zoom, map zoom: ",zoomvalue,"\n")
    
    #Hide based on zoom
    #This code runs also in main map observe; must be way to avoid duplication
    if(zoomvalue <= 9){
      
      LSOAzoomlevel <<- FALSE
      
      leafletProxy("map") %>% showGroup("top level geography")
      leafletProxy("map") %>% clearGroup("lsoas")
      leafletProxy("map") %>% clearGroup("frontiers")
      leafletProxy("map") %>% clearGroup("toplevelgeog_outline")
      
      lastzoomvalue = zoomvalue
      
    } else if(!LSOAzoomlevel)  {
      
      #Set outside for loop scope
      LSOAzoomlevel <<- TRUE
      
      leafletProxy("map") %>% hideGroup("top level geography")
      
      #Find LSOA under centre point
      centerpoint = st_sfc(x = st_point(c(input$map_center[[1]],input$map_center[[2]])), crs = st_crs(toplevelgeog))
      
      #TTWA under the central point
      toplevelgeog_underpoint <- st_intersection(toplevelgeog, centerpoint)
      
      print(toplevelgeog_underpoint$ttwa11nm)
      
      leafletProxy('map')%>% 
        addPolygons(
          data = lsoa %>% filter(ttwa==toplevelgeog_underpoint$ttwa11nm),
          fillColor = ~lsoapalette(UKborn_percent),
          color = 'black',
          weight = 0.2,
          opacity = 1,
          fillOpacity = 0.5,
          group = "lsoas"
        ) %>% 
        addPolylines(
          data = frontiers.live.list[[toplevelgeog_underpoint$ttwa11nm]],
          color = 'black',
          weight = 3,
          opacity = 1,
          group = "frontiers"
        ) %>%
        addPolygons(
          data = toplevelgeog %>% filter(ttwa11nm == toplevelgeog_underpoint$ttwa11nm),
          fill = F,
          color = 'white',
          weight = 8,
          opacity = 1,
          group = "toplevelgeog_outline"
        ) 
      
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
          
          #Set outside if scope
          lastTopLevelGeography <<- toplevelgeog_underpoint$ttwa11nm
        
          leafletProxy("map") %>% clearGroup("lsoas")
          leafletProxy("map") %>% clearGroup("frontiers")
          leafletProxy("map") %>% clearGroup("toplevelgeog_outline")
          
          leafletProxy('map') %>% 
            addPolygons(
              data = lsoa %>% filter(ttwa==toplevelgeog_underpoint$ttwa11nm),
              fillColor = ~lsoapalette(UKborn_percent),
              color = 'black',
              weight = 0.2,
              opacity = 1,
              fillOpacity = 0.5,
              group = "lsoas"
            ) %>% 
            addPolylines(
              data = frontiers.live.list[[toplevelgeog_underpoint$ttwa11nm]],
              color = 'black',
              weight = 3,
              opacity = 1,
              group = "frontiers"
            ) %>%
            addPolygons(
              data = toplevelgeog %>% filter(ttwa11nm == toplevelgeog_underpoint$ttwa11nm),
              fill = F,
              color = 'white',
              weight = 8,
              opacity = 1,
              group = "toplevelgeog_outline"
            ) 

          
        
        }#end if lastTopLevelGeography
        
      }#end if length
      
    }#end if LSOA zoom level
    
    
  })
  
  
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

  
  

  observe({
    
    cat("Leaflet proxy call.")

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
  
  ## generate plot -----
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
