library(shiny)
library(tidyverse)
library(sf)
library(leaflet)

#Based on https://shiny.rstudio.com/articles/tabsets.html

#Load data in UI so we can use the LA column names to set an option to select which to view on map

#RDS for these two is ~0.2 seconds vs ~10 seconds if loading from geojson directly

#load local authority level summary map data - we need here to get list of LA variable names to be able to choose from
la <- readRDS('data/localauthoritymap_w_IMDsummarydata.rds')

meta <- 
  'data/metadata.rds' %>%
  readRDS()


#LA column names to be able to choose from (can later add lookups for better names, explanations etc)

omit_la_cols <- 
  meta$la$colnames %in% c('NAME', 'CODE', 'geometry')
la_colname_options <- 
  meta$la$colnames[!omit_la_cols] 



# Define UI for random distribution app ----
fluidPage(
  
  #For floating selection widgets above map
  #https://stackoverflow.com/a/42292858
  # tags$head(
  #   tags$style(
  #     HTML(
  #       '
  #           .outer {
  #               position: fixed;
  #               top: 80px;
  #               left: 0;
  #               right: 0;
  #               bottom: 0;
  #               overflow: hidden;
  #               padding: 0;
  #           }
  # 
  #           #controls-filters {
  #               background-color: white;
  #               border:none;
  #               padding: 10px 10px 10px 10px;
  #               z-index:150;
  #           }
  #           '
  #     )
  #   )
  # ),
  
  # App title ----
  titlePanel("Tabsets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

      selectInput(
        inputId = 'la_varname_to_display_on_map',
        label = 'local authority variable to display on map',
        choices = la_colname_options,
        selected = 'mean2019',
        selectize = T
        )

    ),
    
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
    

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("mapTab", leafletOutput("map", height = 1000)),
                  # tabPanel("mapTab", div(class="outer", leafletOutput("map", height = 1000))),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)