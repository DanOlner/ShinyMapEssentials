#Based on https://shiny.rstudio.com/articles/tabsets.html


#load TTWA level summary map data - we need here to get list of variable names to be able to choose from
#RDS for these two is ~0.2 seconds vs ~10 seconds if loading from geojson directly
ttwa <- readRDS('data/ttwa.rds')

#UI WILL HAVE WIDGET TO SELECT TYPE OF TOP LEVEL DATA

# toplevel_colname_options <-
#   c('UK born %',
#     'frontier_rank',
#     'di_rank'
#   )

#Setting to single var for now
toplevel_colname_options <-
  c('UK born %')

area_options <- 
  c('', ttwa$ttwa11nm %>% unique)

# ui elements  --------------------------------------------------------------

download_data <-
  function(){
    downloadLink('download_app_data', 'Click here to download dashboard data')
  }


# map_input_panel <-
#   function(){
#      selectInput(
#         inputId = 'toplevel_varname_to_display_on_map',
#         label = 'Variable to display on map',
#         choices = toplevel_colname_options,
#         selected = 'UK born %',
#         # selected = 'prop_foreign_born',
#         selectize = T,
#         
#         )
#   }

area_searcher_panel <-
  function(){
#    textInput("password_input", label=h4(":pass"),value = "", width = "50%")
    selectizeInput("postcode_chosen", "Input first part of a postcode to see its containing region:",
                   choices = NULL, ## do this clientsize
                   options=list(maxOptions = 5)
    )
  }

summary_input_panel <-
  function(){
    selectInput(
      inputId = 'area_chosen',
      label = 'To choose a region to view, click on a different region on the map or input its name below:',
      choices = area_options,
     selected = 'London',
      selectize = T
    )
  }


census_select_panel <-
  function(){
     selectInput(
        inputId = 'census_select',
        label = 'Select which Census to display: 2011, 2021 or the percentage-point difference between them (with 2021 frontiers shown)',
        choices = c('2011','2021','difference'),
        selected = '2021',
        selectize = T
        )
  }


toggleSwitch <- 
  function(){
    materialSwitch(inputId = "switch1", label = HTML("<b>Toggle map: one or two colours (defaults to two if viewing Census difference)</b>"))
  }

# Panel layouts -----------------------------------------------------------



about_tab_panel <- 
  function(title){
    tabPanel(title,  
             fluidRow(
               column(width = 11, includeMarkdown("./assets/about.md"), offset = 1)
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
#      fluidRow(width = 12, summary_input_panel()), 
      
      fluidRow(
        column(6, plotlyOutput("rank_plot")),
        
        column(5, offset = 1,
          includeMarkdown("./assets/definitions.md")
        )
      ),
      fluidRow(
        column(width = 5, 
               h4('Summary of regional data'),
               p('Here we plot frontier density by region (above) and the relationship between frontier density and segregation (opposite).'),
               textOutput('frontier_summary'), offset = 1), # This is a reactive write up
        column(6, plotlyOutput("scatter_plot"))
      ),

      
        
      

      
    )
  }

method_panel <-
  function(title){
    tabPanel(title,
             fluidRow(
               column(width = 11, includeMarkdown("./assets/methods.md"), offset = 1)
             ),
             fluidRow(
               column(width = 11, download_data(), offset = 1)
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
                                 'Drag the map and zoom in to see the location of frontiers.'
                               ),
                               # p(
                               #   'Colours represent wider areas with high numbers of frontiers. Drag the map and zoom in to see the location of frontiers.'
                               # ),
                               summary_input_panel(),
                               area_searcher_panel(),
                               # map_input_panel(),
                               census_select_panel(),
                               toggleSwitch(),
                               # plotlyOutput("3Dmap"),
                               textOutput('ttwa_writeup')

                               ),
                             mainPanel(
                               leafletOutput("map", height = 1000))
                           ) 
                           ),
                            
                          
                  summary_panel('Summary and plots'),
    method_panel('Data and methods')
                  )                
      )
      
    
