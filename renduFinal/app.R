library(shiny)
library(shinydashboard)
library(data.table)
library(leaflet)
library(sf)

donneesViti <- read.table("fichierdonneesViti.csv", header=TRUE, sep=";", dec=",")

calculGradientReg <- function(anneeSel) {
    donneesVitiTot <- donneesViti
    donneesVitiTot$vinTOT <- donneesVitiTot$AOP + donneesVitiTot$IGP + donneesVitiTot$sansIG
    head(donneesVitiTot)
    donneesVitiTot <- donneesVitiTot[, c(-3:-6, -1)]
    head(donneesVitiTot)
    donneesVitiTot <- donneesVitiTot[, c(2, 1, 3)]
    colnames(donneesVitiTot) <- c("CODE_REG", "annee", "VOLUME_TOTAL")
    
    agg <- aggregate(.~CODE_REG+annee, donneesVitiTot, sum)
    
    ProdAnnee <- agg[agg$annee == anneeSel,]
    ProdAnnee <- ProdAnnee[, -2]
    return(ProdAnnee)
}

recupDataGraph <- function(typeVin, anneeSel, regClick=76) {
    dfVitiReg <- donneesViti
    print(anneeSel)
    print(regClick)
    print(dfVitiReg)
    dfVitiReg <- dfVitiReg[dfVitiReg$annee == anneeSel & dfVitiReg$id_reg == regClick,]
    print(dfVitiReg)
    if(typeVin == "AOP") {
        dfVitiReg <- dfVitiReg[, c(-1:-2, -5:-7)]
        dfVitiReg <- aggregate(AOP ~ mois, data = dfVitiReg, sum)
    } else if (typeVin == "IGP") {
        dfVitiReg <- dfVitiReg[, c(-1:-2, -4, -6:-7)]
        dfVitiReg <- aggregate(IGP ~ mois, data = dfVitiReg, sum)
    } else {
        dfVitiReg <- dfVitiReg[, c(-1:-2, -4:-5, -7)]
        dfVitiReg <- aggregate(sansIG ~ mois, data = dfVitiReg, sum)
    }
    
    moisExi <- list("janvier", "fevrier", "mars", "avril", "mai", "juin", "juillet", "aout", "septembre", "octobre", "novembre", "decembre")
    
    absci <- c()
    for(i in 1:12) {
        dfVitiRegMois <- dfVitiReg[dfVitiReg$mois==moisExi[i],]
        if (length(row.names(dfVitiRegMois[2])) == 0) {
            absci <- c(absci, "0")} else {
                absci <- c(absci, dfVitiRegMois[[2]])
            }
    }
    dataVin <- data.table(
        mois = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"),
        volume = absci)
    return(dataVin)}

ui <- fluidPage(
    tags$head(tags$style(HTML(".leaflet-container {background: #ffffff;}"))),
    titlePanel("Carte Région Viticole"),
    sidebarLayout(
        mainPanel(leafletOutput("map")),
        sidebarPanel(selectInput(inputId = "anneeSelect", label="",
                        selected = 2022, choices = c("2011"=2011, "2012"=2012, "2013"=2013,
                                                    "2014"=2014, "2015"=2015, "2016"=2016, "2017"=2017,
                                                    "2018"=2018, "2019"=2019, "2020"=2020, "2021"=2021,
                                                    "2022"=2022)))
    ))
     # fluidRow(
     #     column(6, leafletOutput("map")),
     #     column(6, selectInput(inputId = "anneeSelect", label="",
     #                           selected = 2022, choices = c("2011"=2011, "2012"=2012, "2013"=2013,
     #                                                      "2014"=2014, "2015"=2015, "2016"=2016, "2017"=2017,
     #                                                      "2018"=2018, "2019"=2019, "2020"=2020, "2021"=2021,
     #                                                      "2022"=2022)),
     #           plotOutput("graphAOP")))

server <- function(input, output) {
    anneeSelect <- reactive({
        input$anneeSelect
    })
    
    #clickReg <- eventReactive(input$map_shape_click, {
    #    rien <- input$map_shape_click
    #    print(rien$CODE_REG)
    #    recupDataGraph("AOP", anneeSelect(), rien)
    #})
    
    output$map <- renderLeaflet({
        regions <- st_read("regions.shp")
        regions <- st_transform(regions, crs = 4326)
        
        ProdAnneeSel <- calculGradientReg(anneeSelect())
        regions <- merge(regions, ProdAnneeSel, by = "CODE_REG")
        pal <- colorNumeric(palette = "PuRd", domain = regions$VOLUME_TOTAL)
        
        leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE))%>%
            setView(lat = 46.6033, lng = 1.8833, zoom = 5)%>%
            addPolygons(data = regions, weight = 1.1, fillOpacity = 1, color = ~pal(VOLUME_TOTAL))%>%
            addLegend(data = regions, pal = pal, values = ~VOLUME_TOTAL, opacity = 1, labFormat = labelFormat(big.mark=" ", suffix=" hl"), title = "Quantité annuelle de vin</br>sortie des chaies par région")
    })
    
    #output$graphAOP <- renderPlot({
    #    if(is.null(clickReg())) {dataVIN <- recupDataGraph("AOP", anneeSelect())} else {dataVIN <- clickReg()}
    #    spline_int <- as.data.frame(spline(dataVIN$mois, dataVIN$volume))
    #    ggplot(spline_int, aes(x = x, y = y)) +
    #        #with_shadow(geom_line(colour = "#ff8686"), colour="#ff8686", x_offset=0, y_offset=5, sigma= 6) +
    #        geom_line(colour = "#ff8686") +
    #        theme_classic() +
    #        theme(axis.title.x = element_blank(),
    #              axis.title.y = element_blank(),
    #              axis.text.x = element_text(angle = 45)) +
    #        scale_x_continuous(breaks = seq(1, 12, by = 1), expand = expansion(mult = c(0, 0)), labels=c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))
    #})
}

shinyApp(ui, server)
