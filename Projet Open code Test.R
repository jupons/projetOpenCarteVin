library(shiny)
library(leaflet)
library(mapview)

# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Map App"),
  
  # Tabs
  tabsetPanel(
    tabPanel("Leaflet Map", 
             leafletOutput("mymap1")),
    tabPanel("Mapview Map", 
             mapviewOutput("mymap2"))
  )
)

#Server code:
  

# Define the server logic
server <- function(input, output) {
  
  # Leaflet map
  output$mymap1 <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 13)
  })
  
  # Mapview map
  output$mymap2 <- renderMapview({
    mapview(quakes, zcol = "mag")
  })
}
shinyApp(ui=ui, server=server)
