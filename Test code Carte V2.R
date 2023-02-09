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
  data <- read.csv(file = 'fichierdonnees.csv', sep=';')
  
  regions <- st_read("regions.shp")
  regions <- st_transform(regions, crs = 4326)
  
  departements <- st_read("departements.shp")
  departements <- st_transform(departements, crs = 4326)
  
  bins_r <- c(0, 1000, 5000, 10000, 20000, 50000, Inf)
  bins_d <- c(0, 10000, 50000, 100000, 200000, 500000, Inf)
  pal_regions <- colorBin("Reds", domain = regions$surf_km2, bins = bins_r)
  pal_departements <- colorBin("Blues", domain = departements$surf_ha, bins = bins_d)
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lat = 46.6033, lng = 1.8833, zoom = 5)%>%
      addPolygons(data = departements, color = "white", group = "departements", layerId = 1, weight = 1, fillOpacity = 0.5, fillColor = ~pal_departements(surf_ha), highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE))%>%
      addPolylines(data = regions, color = "red", group = "regions_light", layerId = 3, weight = 2, fillOpacity = 0.2, fillColor = ~pal_regions(surf_km2))%>%
      addPolygons(data = regions, color = "white", group = "regions", layerId = 2, weight = 1, fillOpacity = 1, fillColor = ~pal_regions(surf_km2), highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE))
      
    })

  observe(
    {click = input$map_shape_click
    if(is.null(click))
      return()
    else if (click$id == 2){
      leafletProxy("map")%>%
      setView(lat = click$lat, lng = click$lng, zoom = 6)%>%
      hideGroup("regions")%>%
      addMapPane("regions_light", zIndex=420)%>%
      addMapPane("departements",zIndex=410)
      }
    else if (click$id == 1) {
        leafletProxy("map")%>%
        hideGroup("regions_light")%>%
        setView(lat = click$lat, lng = click$lng, zoom = 8)
    }
    }
  )
  
}

shinyApp(ui, server)





