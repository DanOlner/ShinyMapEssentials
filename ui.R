#Based on https://shiny.rstudio.com/articles/tabsets.html

#Load data in UI so we can use the LA column names to set an option to select which to view on map

#RDS for these two is ~0.2 seconds vs ~10 seconds if loading from geojson directly

#load local authority level summary map data - we need here to get list of LA variable names to be able to choose from
la <- readRDS('data/localauthoritymap_w_IMDsummarydata.rds')


#LA column names to be able to choose from (can later add lookups for better names, explanations etc)
la_colname_options <- names(la)[3:20]

# fake data ---------------------------------------------------------------

la_colname_options <-
  c(la_colname_options,
    'IMD_rank',
    'Dissimilarity_index',
    'Other_index'
  )

area_options <- 
  la$NAME %>% unique

# ui elements  --------------------------------------------------------------

map_input_panel <-
  function(){
  selectInput(
    inputId = 'la_varname_to_display_on_map',
    label = 'local authority variable to display on map',
    choices = la_colname_options,
    selected = 'mean2019',
    selectize = T
  )
  }


summary_input_panel <-
  function(){
    selectInput(
      inputId = 'area_chosen',
      label = 'Area to summarise',
      choices = area_options,
#      selected = 'Bedford',
      selectize = T
    )
  }

summary_input_panel()
about_tab_panel <- 
  function(title){
    tabPanel(title,  
             fluidRow(
               column(width = 12, includeMarkdown("./assets/about.md"), offset = 3)
               )
    )
  }


summary_panel <-
  function(title){
    tabPanel(
      title,
      fluidRow(12, summary_input_panel()), # we can name elemnts for more clarity
      
      
      
      fluidRow(
        column(width = 4, verbatimTextOutput("summary")),
        column(width = 7, includeMarkdown("./assets/explain summary 1.md"), offset = 1)
      ),
      fluidRow(
        column(12, plotlyOutput("plot"))
      )
      
    )
  }

diag_panel <-
  function(title){
    tabPanel(title,
             
             fluidRow(textOutput('pick1') )
             )
  }


# Define UI for random distribution app ----
fluidPage(
  
  # App title ----
  titlePanel("Life at the Frontiers"),
  
  ## theme to look diff
  theme = bs_theme(version = 5, bootswatch = 'lux'), 
  

    ## create a function for the input slider

    # # Sidebar panel for inputs ----
    # sidebarPanel(
    # 
      # selectInput(
      #   inputId = 'la_varname_to_display_on_map',
      #   label = 'local authority variable to display on map',
      #   choices = la_colname_options,
      #   selected = 'mean2019',
      #   selectize = T
      #   )
    # 
    # ),
    
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
                  
                  tabPanel("mapTab", 
                           sidebarLayout(
                             sidebarPanel(map_input_panel()),
                             mainPanel(leafletOutput("map", height = 1000)))
                           ),
                  # tabPanel("mapTab", div(class="outer", leafletOutput("map", height = 1000))),
  #                tabPanel("plotTab", plotlyOutput("plot")),
                  summary_panel('Summary and plots'),
  diag_panel('diagnostics')
                  )                
      )
      
    
