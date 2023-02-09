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
      addPolygons(data = departements, 
                  fillColor = "purple", 
                  weight = 1, 
                  opacity = 1, 
                  color = "blue", 
                  fillOpacity = 0.2, 
                  group = "Département",
                  popup = paste("Département:", departements$nom)) %>%
      addPopups(lng = mean(departements$geometry),
                lat = mean(departements$geometry),
                popup = paste("<a href='https://example.com/departements/", departements$nom, "'>", departements$nom, "</a>"))
    
    })
}

shinyApp(ui, server)



