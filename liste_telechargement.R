library(shiny)

ui <- fluidPage(
  
  titlePanel("Bienvenue sur notre carte des vins !"),
  textInput("nom", label="Veuillez entrer votre nom", value="Nom"),
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
  
  downloadButton("downloadData2", "Download")

)

server <- function(input, output) {
  output$text <- renderText({
    paste("Bienvenue ", input$nom, " !")
  })
}
shinyApp(ui = ui, server = server)
