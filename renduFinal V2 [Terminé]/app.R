library(shiny)
library(shinyjs)
library(data.table)
library(plotly)
library(leaflet)
library(sf)
library(rsconnect)

donneesViti <- read.table('fichierdonneesViti.csv', header=TRUE, sep=';', dec=',', encoding='UTF-8')

donneesVitiTot <- donneesViti
donneesVitiTot$vinTOT <- donneesVitiTot$AOP + donneesVitiTot$IGP + donneesVitiTot$sansIG
donneesVitiTot <- donneesVitiTot[, c(-3:-6, -1)]
donneesVitiTot <- donneesVitiTot[, c(2, 1, 3)]
colnames(donneesVitiTot) <- c('CODE_REG', 'annee', 'VOLUME_TOTAL')
donneesVitiTot <- aggregate(.~CODE_REG+annee, donneesVitiTot, sum)

regions <- st_read('carto/regions.shp')
regions <- st_transform(regions, crs = 4326)


calculGradientReg <- function(anneeSel, donneeTotal) {
    ProdAnnee <- donneeTotal[donneeTotal$annee == anneeSel,]
    ProdAnnee <- ProdAnnee[, -2]
    return(ProdAnnee)
}

creerDataGraph <- function(typeVin, anneeSel, couleurPlot, franceTot, regClick=0) {
    dfVitiReg <- donneesViti
    if(franceTot == 1) {dfVitiReg <- dfVitiReg[dfVitiReg$annee == anneeSel,]} else {dfVitiReg <- dfVitiReg[dfVitiReg$annee == anneeSel & dfVitiReg$id_reg == regClick,]}
    if(typeVin == 'AOP') {
        dfVitiReg <- dfVitiReg[, c(-1:-2, -5:-7)]
        dfVitiReg <- aggregate(AOP ~ mois, data = dfVitiReg, sum)
    } else if (typeVin == 'IGP') {
        dfVitiReg <- dfVitiReg[, c(-1:-2, -4, -6:-7)]
        dfVitiReg <- aggregate(IGP ~ mois, data = dfVitiReg, sum)
    } else {
        dfVitiReg <- dfVitiReg[, c(-1:-2, -4:-5, -7)]
        dfVitiReg <- aggregate(sansIG ~ mois, data = dfVitiReg, sum)
    }
    
    moisExi <- c('janvier', 'fevrier', 'mars', 'avril', 'mai', 'juin', 'juillet', 'aout', 'septembre', 'octobre', 'novembre', 'decembre')
    absci <- c()
    for(i in 1:12) {
        dfVitiRegMois <- dfVitiReg[dfVitiReg$mois==moisExi[i],]
        if (length(row.names(dfVitiRegMois[2])) == 0) {absci <- c(absci, NA)} else {absci <- c(absci, dfVitiRegMois[[2]])}
    }
    dataVIN <- data.table(
        mois = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        volume = absci)
    
    p <- plot_ly(data = dataVIN, x = ~mois, y = ~volume, type = 'scatter', line = list(color= couleurPlot, shape = 'spline'), mode = 'lines')%>%
        config(displayModeBar = FALSE)%>%
        layout(
            hovermode = 'x unified',
            xaxis = list(
                fixedrange = TRUE,
                range = c(min(dataVIN$mois),max(dataVIN$mois)),
                title = '',
                tickangle=-40,
                tickfont = list(family = 'Segoe UI Semilight', size = 11),
                ticktext = list('Janvier', 'Février', 'Mars', 'Avril', 'Mai', 'Juin', 'Juillet', 'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre'),
                tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                showgrid = FALSE),
            yaxis = list(
                fixedrange = TRUE,
                title = list(text = 'Volume mensuel sortie des chaies (hL)', standoff = 12L),
                titlefont = list(family='Segoe UI', size = 13),
                tickfont = list(family='Segoe UI Semilight', size = 13),
                tickformat = ',d',
                rangemode = "tozero",
                zerolinecolor = '#272727',
                zerolinewidth = 0.5),
            annotations = list(
                text = typeVin,
                showarrow = FALSE,
                font = list(family = 'Segoe UI', size = 18),
                xref = "x domain",
                yref = "y domain",
                x = 1,
                y = 1
            ),
            separators = ', ')
    return(p)}

ui <- bootstrapPage(
    useShinyjs(),
    tags$head(tags$style(HTML('.leaflet-container {background: #ffffff;}'))),
    div(
        style = "height: 100vh; width: 100vw; display: flex",
        div(
            style = 'height: 100%; width: 33%; display: flex; flex-direction: column; align-items: center;',
            div(titlePanel('Carte Régions Viticoles')),
            selectInput(inputId = 'anneeSelect', label='', selected = 2022, choices = c('2011'=2011, '2012'=2012, '2013'=2013, '2014'=2014, '2015'=2015, '2016'=2016, '2017'=2017,
                                                                                        '2018'=2018, '2019'=2019, '2020'=2020, '2021'=2021, '2022'=2022)),
            div(style = 'height: 7%', hidden(actionButton('bttnRetour', 'France'))),
            leafletOutput('map')
        ),
        div(
            style = 'height: 100%; width: 34%; padding-right: 2rem',
            plotlyOutput('graphAOP', height = '50%'),
            div(textOutput('regNom'), style = 'font-size: 18px; border-bottom: 1px solid #242424; margin: 0 1rem 2rem 0'),
            div("Voici une carte interactive de France permettant d'obtenir les statistiques viticoles pour chacune des régions de France métropolitaine, entre Août 2011 et Octobre 2022.", style = 'font-size: 12px'),
            div(
                style = 'display : flex; font-size: 11px; margin-top: 1rem',
                p("Source :"),
                p('p', style = 'color: rgba(0,0,0,0)'),
                a("Direction Générale de la Douane et Droits indirects", href = 'https://www.douane.gouv.fr/la-douane/opendata?f%5B0%5D=categorie_opendata_facet%3A471', target = "_blank", style = 'font-style: italic; color: #44546A;'))
        ),
        div(
            style = 'height: 100%; width: 33%',
            plotlyOutput('graphIGP', height = '50%'),
            plotlyOutput('graphSansIG', height = '50%')
        )
    ))

server <- function(input, output) {
    v <- reactiveValues(data = FALSE)
    
    clickReg <- reactiveValues(data = NULL)
    
    zoomFr <- reactive({input$map_zoom})
    
    anneeSelect <- reactive({input$anneeSelect})
    
    observeEvent(input$map_shape_click, {
        clickReg$data <- input$map_shape_click$id
    })
    
    observeEvent(input$bttnRetour, {
        v$data <- FALSE
        clickReg$data <- NULL
        hide('bttnRetour')
    })
    
    output$regNom <- renderText({
        if(is.null(clickReg$data) && v$data == FALSE) {"France"} else {
            show('bttnRetour')
            chNom <- regions[regions$CODE_REG==clickReg$data,]
            chNom[[4]]}
    })
    
    output$map <- renderLeaflet({
        ProdAnneeSel <- calculGradientReg(anneeSelect(), donneesVitiTot)
        regions2 <- merge(regions, ProdAnneeSel, by = 'CODE_REG')
        pal <- colorNumeric(palette = 'PuRd', domain = regions$VOLUME_TOTAL)
        
        leaflet(options = leafletOptions(zoomControl = FALSE, dragging = FALSE, minZoom = zoomFr(), maxZoom = zoomFr(), attributionControl=FALSE))%>%
            setView(lat = 46.6033, lng = 1.8833, zoom = 5)%>%
            addPolygons(data = regions2, weight = 1.1, fillOpacity = 1, color = ~pal(VOLUME_TOTAL), layerId = regions2$CODE_REG) #%>%
            #addLegend(data = regions, pal = pal, values = ~VOLUME_TOTAL, opacity = 1, labFormat = labelFormat(big.mark=' ', suffix=' hl'), title = 'Quantité annuelle de vin</br>sortie des chaies par région')
    })
    
    output$graphAOP <- renderPlotly({
        #if(is.null(clickReg()) || v$data == FALSE) {
        if(is.null(clickReg$data) && v$data == FALSE) {
            creerDataGraph('AOP', anneeSelect(), '#B2084C', 1)
        } else {
            creerDataGraph('AOP', anneeSelect(), '#B2084C', 0, clickReg$data)}
    })
    
    output$graphIGP <- renderPlotly({
        #if(is.null(clickReg()) || v$data == FALSE) {
        if(is.null(clickReg$data) && v$data == FALSE) {
            creerDataGraph('IGP', anneeSelect(), '#67001F',1)
        } else {
            creerDataGraph('IGP', anneeSelect(), '#67001F', 0, clickReg$data)}
    })
    
    output$graphSansIG <- renderPlotly({
        #if(is.null(clickReg()) || v$data == FALSE) {
        if(is.null(clickReg$data) && v$data == FALSE) {
            creerDataGraph('sansIG', anneeSelect(), '#D47FBC',1)
        } else {
            creerDataGraph('sansIG', anneeSelect(), '#D47FBC', 0, clickReg$data)}
    })
}

shinyApp(ui, server)