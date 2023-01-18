library(shiny)
library(tidyverse)
library(sf)
library(leaflet)

#Based on https://shiny.rstudio.com/articles/tabsets.html

#Load data in UI so we can use the LA column names to set an option to select which to view on map

#RDS for these two is ~0.2 seconds vs ~10 seconds if loading from geojson directly

#load local authority level summary map data - we need here to get list of LA variable names to be able to choose from
la <- readRDS('data/localauthoritymap_w_IMDsummarydata.rds')


#LA column names to be able to choose from (can later add lookups for better names, explanations etc)
la_colname_options <- names(la)[3:20]


# Define UI for random distribution app ----
fluidPage(
  
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
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("mapTab", leafletOutput("map", height = 1000)),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)