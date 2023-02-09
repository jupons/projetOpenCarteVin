library(shiny)
library(leaflet)
library(sf)

ui <- fluidPage(
  titlePanel("France"),
  br(),
  fluidRow(
    column(6, leafletOutput("map")),
    column(6, "")
  )
)

server <- function(input, output) {
  regions <- st_read("regions.shp")
  regions <- st_transform(regions, crs = 4326)
  
  departements <- st_read("departements.shp")
  departements <- st_transform(departements, crs = 4326)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      
      setView(lat = 46.6033, lng = 1.8833, zoom = 5)%>%
      addPolygons(data = departements, color = "white", weight = 1, fillOpacity = 0.5, fillColor = "blue")%>%
      addPolygons(data = regions, color = "white", weight = 1, fillOpacity = 1, fillColor = "red")
  })

  
}

shinyApp(ui, server)





