library(shiny)
library(bslib)
library(sf)
library(leaflet)

# read in data
lookup <- readRDS("data_cleaned/lookup.rds")
gps <- readRDS("data_cleaned/gps.rds")
hr <- st_read("data_cleaned/hr.shp")

# TSP names
tsp.list <- as.list(lookup$track_season_post)

# define UI
# to start, I want to be able to select a TSP and render its map
ui <- page_fluid(
  
  selectInput(
    
    "select",
    "Choose track:",
    tsp.list
    
  ),
  textOutput("value"),
  leafletOutput("map")
  
)


# define server
# this will render a leaflet map
server <- function (input, output) {
  
  # subset TSP
  focal.tsp <- reactive({ input$select })
  
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
  
  # leaflet map
  output$map <- renderLeaflet({
    
    leaflet() |>
    
    # ESRI basemap
    addProviderTiles(providers$Esri.WorldImagery) |>
    
    # HR contours
    addPolygons(data = focal.full(),
                color = "gray",
                fill = NA,
                opacity = 0.8) |>
    addPolygons(data = focal.core(),
                color = "white",
                fill = NA) |>
    
    # points
    addCircleMarkers(data = focal.gps(),
                     radius = 0.5,
                     opacity = 0.75)
    
  })
  
}

# call shiny
shinyApp(ui = ui, server = server)
