library(shiny)
library(data.table)
library(ggplot2)
library(ggalt)
library(ggfx)

dataAOP <- data.table(
    mois = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"),
    volume = c(6230, 4500, 3700, 3000, 2450, 3200, 4678, 2435, 1342, 1731, 2340, 3500)
) #Ce dataAOP est un exemple pour voir comment générer les graphiques

spline_int <- as.data.frame(spline(dataVIN$mois, dataVIN$volume)) #Permet d'adoucir la courbure de la courbe formée par le jeu de donnée dataAOP

ui <- fluidPage(
    fluidRow(
        column(12,
            "selecteurAnnee",
            fluidRow(
                column(6,
                    #leafletOutput,
                    "légende du gradient"
                    ),
                column(6,
                    fluidRow(
                        column(3,
                            plotOutput("graphIGP"),
                            fluidRow(
                                column(1,
                                    imageOutput("/img/coupe-or"),
                                    imageOutput("/img/coupe-argent"),
                                    imageOutput("/img/coupe-bronze")
                                    ),
                                column(2,
                                    "premiereRegionViticole",
                                    "deuxiemeRegionViticole",
                                    "troisiemeRegionViticole"
                                    )
                                )
                            ),
                        column(3,
                            plotOutput("graphAOP"),
                            plotOutput("graphSansIg")
                            )
                        )
                    )
                )
            )
        )
    )
    
server <- function(input, output) {
    output$graphAOP <- renderPlot({
        ggplot(spline_int, aes(x = x, y = y)) +
            with_shadow(geom_line(colour = "#ff8686"), colour="#ff8686", x_offset=0, y_offset=5, sigma= 6) +
            theme_classic() +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(angle = 45)) +
            scale_x_continuous(breaks = seq(1, 12, by = 1), expand = expansion(mult = c(0, 0)), labels=c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))
    })
    output$graphIGP <- renderPlot({
        ggplot(spline_int, aes(x = x, y = y)) +
            with_shadow(geom_line(colour = "#ff8686"), colour="#ff8686", x_offset=0, y_offset=5, sigma= 6) +
            theme_classic() +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(angle = 45)) +
            scale_x_continuous(breaks = seq(1, 12, by = 1), expand = expansion(mult = c(0, 0)), labels=c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))
    })
    output$graphSansIg <- renderPlot({
        ggplot(spline_int, aes(x = x, y = y)) +
            with_shadow(geom_line(colour = "#ff8686"), colour="#ff8686", x_offset=0, y_offset=5, sigma= 6) +
            theme_classic() +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(angle = 45)) +
            scale_x_continuous(breaks = seq(1, 12, by = 1), expand = expansion(mult = c(0, 0)), labels=c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))
    })
}


shinyApp(ui = ui, server = server)