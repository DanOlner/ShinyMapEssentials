#Based on https://shiny.rstudio.com/articles/tabsets.html

#Load data in UI so we can use the LA column names to set an option to select which to view on map

#RDS for these two is ~0.2 seconds vs ~10 seconds if loading from geojson directly

#load local authority level summary map data - we need here to get list of LA variable names to be able to choose from
ttwa <- readRDS('data/ttwa.rds')

#UI WILL HAVE WIDGET TO SELECT TYPE OF TOP LEVEL DATA, SWAP BETWEEN LA AND TTWA
#SETTING TO TTWA FOR NOW (IN SERVER)


#LA column names to be able to choose from (can later add lookups for better names, explanations etc)
# toplevel_colname_options <- names(la)[3:20]

# fake data ---------------------------------------------------------------

toplevel_colname_options <-
  c('IMD_rank',
    'Dissimilarity_index',
    'Other_index'
  )

area_options <- 
  ttwa$ttwa11nm %>% unique

# ui elements  --------------------------------------------------------------

download_data <-
  function(){
    downloadLink('download_app_data', 'Download dashboard data')
  }


map_input_panel <-
  function(){
     selectInput(
        inputId = 'toplevel_varname_to_display_on_map',
        label = 'top level geog variable to display on map',
        choices = toplevel_colname_options,
        selected = 'IMD_rank',
        selectize = T
        )
  }


summary_input_panel <-
  function(){
    selectInput(
      inputId = 'area_chosen',
      label = 'Area to summarise',
      choices = area_options,
     selected = 'Sheffield',
      selectize = T
    )
  }

about_tab_panel <- 
  function(title){
    tabPanel(title,  
             fluidRow(
               column(width = 11, includeMarkdown("./assets/about.md"), offset = 1)
               ),
             fluidRow(
               column(width = 11, download_data(), offset = 1)
             ),
             fluidRow(
               column(width = 11, offset = 1,
                      span(
                        img(
                          src = 'esrc logo.JPG',
                          width = "40%",
                          inline = T
                        ),
                        img(
                          src = 'nordf logo.png',
                          width = "40%",
                          inline = T
                        )
                      )
               )
             )
             
    )
  }

  

summary_panel <-
  function(title){
    tabPanel(
      title,
      fluidRow(width = 12, summary_input_panel()), 
      
      fluidRow(
        column(5, plotlyOutput("plot")),
        column(width = 6, textOutput('write1'), offset = 1)
      ),
      fluidRow(
        column(width = 6, 
               h4('Overall relationship between frontiers and segregation'),
               p('Here is the relationship')),
        column(width = 6, verbatimTextOutput("summary"))
      )
      
    )
  }

diag_panel <-
  function(title){
    tabPanel(title,
             fluidRow(
               p('some text')
             )
             )
  }


# Define UI for random distribution app ----
fluidPage(
  
  # App title ----
  titlePanel(title = 
               span("Life at the Frontiers", img(src = 'latf logo.PNG', height = 35))
             ),
  
  ## theme to look diff
  theme = bs_theme(version = 5, bootswatch = 'lux'), 
  

    
    #Floating panel above - currently not showing above the map, will have to fiddle with order
    #https://stackoverflow.com/a/42292858
    # absolutePanel(
    #   id = "PANEL_la_varname_to_display_on_map",
    #   class = "panel panel-default",
    #   fixed = TRUE,
    #   draggable = TRUE,
    #   top = 100,
    #   left = "auto",
    #   right = 20,
    #   bottom = "auto",
    #   width = 330,
    #   height = "auto",
    #   selectInput(
    #     inputId = 'la_varname_to_display_on_map',
    #     label = 'local authority variable to display on map',
    #     choices = la_colname_options,
    #     selected = 'mean2019',
    #     selectize = T
    #   ),
    #   actionButton("zoomer", "reset zoom")
    # ),
    


      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  about_tab_panel('About'),
                  
                  tabPanel("Map", 
                           sidebarLayout(
                             sidebarPanel(
                               h4(strong("Explore Frontiers")),
                               p(
                                 'Colours represent wider areas with high numbers of frontiers. Drag the map and zoom in to see the location of frontiers'
                               ),
                               
                               map_input_panel(),
                               plotlyOutput("3Dmap"),
                               textOutput('ttwa_writeup')

                               ),
                             mainPanel(
                               leafletOutput("map", height = 1000))
                           ) 
                           ),
                            
                          
                  summary_panel('Summary and plots'),
    diag_panel('diagnostics')
                  )                
      )
      
    
