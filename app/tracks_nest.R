# this one is my attempt at making a hierarchical select
# https://mastering-shiny.org/action-dynamic.html#hierarchical-select

library(tidyverse)
library(shiny)
library(bslib)
library(sf)
library(leaflet)

# read in data
lookup <- readRDS("data_cleaned/lookup.rds")
gps <- readRDS("data_cleaned/gps.rds")
hr <- st_read("data_cleaned/hr.shp")

# lists of names
site.list <- sort(unique(lookup$site))

# define UI
ui <- fluidPage(
  
  titlePanel(
    
    "Hare space use"
    
  ),
  
  sidebarLayout(
    sidebarPanel(
      
      # hierarchical inputs
      selectInput("season", "Season", choices = c("off", "on")),
      selectInput("site", "Site", choices = NULL),
      selectInput("track", "Track", choices = NULL)
      
    ),
    mainPanel(
      
      textOutput("value"),
      leafletOutput("map")
      
      )
  )
  
)

# define server
# this will render a leaflet map
server <- function (input, output) {
  
  # possible sites
  sites <- reactive({ filter(gps, season == input$season) })
  
  # allow choices update
  observeEvent(sites(), {
    
    choices <- sort(unique(sites()$site))
    updateSelectInput(inputId = "site", choices = choices)
    
  })
  
  # possible tracks
  tsps <- reactive({ filter(gps, 
                            season == input$season,
                            site == input$site) })
  
  # allow choices update
  observeEvent(tsps(), {
    
    choices <- sort(unique(tsps()$track_season_post))
    updateSelectInput(inputId = "track", choices = choices)
    
  })
  
  # rest of the code should be the same?
  focal.tsp <- reactive({ input$track })
  
  # GPS
  focal.gps <- reactive({
    
    gps |> filter(track_season_post == focal.tsp()) |>
      
      st_as_sf(coords = c("lon", "lat"), crs = "epsg:4326")
    
  })
  
  # HR contours
  focal.full <- reactive({
    
    hr |> 
      
      filter(trck_s_ == focal.tsp() & contour == "full") |>
      
      st_transform(crs = "epsg:4326")
    
  })
  
  focal.core <- reactive({
    
    hr |> 
      
      filter(trck_s_ == focal.tsp() & contour == "core") |>
      
      st_transform(crs = "epsg:4326")
    
  })
  
  # lookup information
  focal.info <- reactive({
    
    focal.lookup <- lookup |> filter(track_season_post == focal.tsp())
    
    # tracking period
    focal.track.period <- gps |>
      
      filter(track_season_post == focal.tsp()) |> 
      
      slice(1, n()) |>
      
      dplyr::select(timestamp) |>
      
      mutate(timestamp = substr(timestamp, 1, 10))
    
    # bind together
    focal.out <- paste0(focal.lookup$sex, 
                        " ", 
                        focal.lookup$MRID,
                        " | ",
                        focal.track.period[1, 1],
                        " to ",
                        focal.track.period[2, 1],
                        " | ",
                        nrow(focal.gps()),
                        " relocations")
    
    return(focal.out)
    
  })
  
  # OUTPUTS
  # text value
  output$value <- renderText({ focal.info() })
  
  # leaflet map
  output$map <- renderLeaflet({
    
    leaflet() |>
      
      # ESRI basemap
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(opacity = 0.65)) |>
      
      # HR contours
      addPolygons(data = focal.full(),
                  color = "white",
                  fillColor = "white",
                  weight = 2.5) |>
      addPolygons(data = focal.core(),
                  color = "black",
                  fillColor = "white",
                  weight = 2.5) |>
      
      # points
      addCircles(data = focal.gps(),
                 radius = 3.5,
                 opacity = 0.5,
                 color = "black",
                 fillColor = "white",
                 fillOpacity = 0.5,
                 weight = 1.0)
    
  })
  
}

# call shiny
shinyApp(ui = ui, server = server)
