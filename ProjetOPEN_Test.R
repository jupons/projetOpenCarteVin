library(shiny)
library(leaflet)
library(mapview)

# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Map App"),
  
  # Tabs
  tabsetPanel(
    tabPanel("France Map", 
             leafletOutput("france_map")),
    tabPanel("Region Map", 
             leafletOutput("region_map"))
  ),
  
  # Select input for wine data
  selectInput("wine_var", "Select wine variable:", 
              c("Wine production" = "production", "Wine quality" = "quality"),
              selected = "production"),
  
  # Input for region
  selectInput("region", "Select region:", 
              unique(region_data$region),
              selected = unique(region_data$region)[1])
)

# Define the server logic
server <- function(input, output) {
  
  # Leaflet map of France
  output$france_map <- renderLeaflet({
    leaflet(france_data) %>% addTiles() %>% fitBounds(france_data)
  })
  
  # Leaflet map of regions
  output$region_map <- renderLeaflet({
    region_subset <- region_data[region_data$region == input$region,]
    leaflet(region_subset) %>% addTiles() %>% fitBounds(region_subset)
  })
  
  # Add wine data to maps
  observe({
    if (input$wine_var == "production") {
      leafletProxy("france_map") %>% addCircleMarkers(lng = france_data$lng, lat = france_data$lat, 
                                                      radius = france_data$production/1000, 
                                                      color = "red", fillOpacity = 0.5)
      leafletProxy("region_map") %>% addCircleMarkers(lng = region_subset$lng, lat = region_subset$lat, 
                                                      radius = region_subset$production/1000, 
                                                      color = "red", fillOpacity = 0.5)
    } else {
      leafletProxy("france_map") %>% addCircleMarkers(lng = france_data$lng, lat = france_data$lat, 
                                                      radius = france_data$quality*2, 
                                                      color = "blue", fillOpacity = 0.5)
      leafletProxy("region_map") %>% addCircleMarkers(lng = region_subset$lng, lat = region_subset$lat, 
                                                      radius = region_subset$quality*2, 
                                                      color = "blue", fillOpacity = 0.5)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
