#Based on https://shiny.rstudio.com/articles/tabsets.html

#Load data in UI so we can use the LA column names to set an option to select which to view on map

#RDS for these two is ~0.2 seconds vs ~10 seconds if loading from geojson directly

#load local authority level summary map data - we need here to get list of LA variable names to be able to choose from
la <- readRDS('data/localauthoritymap_w_IMDsummarydata.rds')
ttwa <- readRDS('data/ttwa_engwales.rds')


# fake data ---------------------------------------------------------------

toplevel_colname_options <-
  c('IMD_rank',
    'Dissimilarity_index',
    'Other_index'
  )


area_options <- 
  ttwa$ttwa11nm %>% unique

# ui elements  --------------------------------------------------------------

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
      selectize = T
    )
  }

## This widget chooses between LA / TTWA / whatever geographies
upper_area_input <-
  function(){
    switchInput(
      inputId = 'chose_ttwa',
      label = 'View mode',
      onLabel = 'TTWA',
      offLabel = 'LA',
      value = T
    )
  }

    
## Panels -------------------------------------------

about_tab_panel <- 
  function(title){
    tabPanel(title,  
             fluidRow(
               column(width = 11, includeMarkdown("./assets/about.md"), offset = 1)
               )
    )
  }


summary_panel <-
  function(title){
    tabPanel(
      title,
      fluidRow(
#        column(width = 4, upper_area_input()),
        column(width = 4, summary_input_panel())
               ), 
      
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

map_panel <-
  function(title){
    tabPanel(title = title, 
             sidebarLayout(
               sidebarPanel(
                 fluidRow(upper_area_input() ),
                 fluidRow(map_input_panel() )
               ),
               mainPanel(leafletOutput("map", height = 1000)))
    )
    
  }

diag_panel <-
  function(title){
    tabPanel(title,
             fluidRow(
               verbatimTextOutput('chose_ttwa')
             )
             )
  }


# Define UI for random distribution app ----
fluidPage(
  
  # App title ----
  titlePanel("Life at the Frontiers"),
  
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
                  map_panel("Maps"),
                  # tabPanel("mapTab", div(class="outer", leafletOutput("map", height = 1000))),
  #                tabPanel("plotTab", plotlyOutput("plot")),
                  summary_panel('Summary and plots'),
  diag_panel('diagnostics')
                  )                
      )
      
    
