library(shiny)
library(leaflet)
library(sf)

ui <- fluidPage(
  titlePanel("Bienvenue sur notre carte des vins !"),
  textOutput(outputId="text"),
  selectInput(inputId = "idSelect", label = "Veuillez sélectionner l'année du vin souhaitée ", 
              selected = 11, choices = c("2011"=2011, "2012"=2012, "2013"=2013,
                                         "2014"=2014, "2015"=2015, "2016"=2016, "2017"=2017,
                                         "2018"=2018, "2019"=2019, "2020"=2020, "2021"=2021,
                                         "2022"=2022)),
  #bouton telechargement
  hr(),
  
  h3("Sauvegarder le graphique"),
  
  selectInput(inputId = "ExportGraph",
              label = "Choisir le format",
              choices = c("PNG", "JPEG", "PDF"),
              selected = "PNG"),
  
  downloadButton("downloadData2", "Download"),
  
  fluidRow(
    column(8, leafletOutput("map")),
    column(8, "")
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
                popup = paste(departements$nom, "'>", departements$nom, "</a>"))
    
    })
}

shinyApp(ui, server)



