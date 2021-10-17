library(igraph)
library(network)
library(dplyr)
library(sna)
library(RColorBrewer)
library(ggraph)
library(networkD3)
library(visNetwork)
library(ndtv)
library(shiny)
library(shinyWidgets)                                                           #Please note that data for Nebraska and South Carolina are only available for the Senate and House, respectively.")))))
library(visNetwork)
library(shiny)
library(stringr)
library(rsconnect)
library(shiny)
ui <- fluidPage(titlePanel("Create your own interactive network"), 
                sidebarLayout(position = "left", sidebarPanel(selectInput(inputId = "state", "Pick a state",
                                                                          c("Alaska" = "AK",
                                                                            "Illinois" = "IL",
                                                                            "Indiana" = "IN",
                                                                            "Minnesota" = "MN",
                                                                            "Nebraska" = "NE",
                                                                            "Oklahoma" = "OK",
                                                                            "South Carolina" = "SC")),
                                                              radioButtons("year", "Select a year", c("2010" = "10", "2012" = "12", "2014" = "14", "2016" = "16", "2018" ="18")),
                                                              radioButtons("upper", "Select a chamber", c("Lower" = "L", "Upper" = "U"))),mainPanel = visNetworkOutput("view")))


server <- function(input, output, session) { 
    vars <- read.csv("pooledallpartyids.csv")
    edges <- read.csv("edgesall.csv")
    filtered1 <- reactive({    
        edges %>% dplyr::filter(state %in% input$state)%>%
            dplyr::filter(year%in%input$year) %>%
            dplyr::filter(upper%in%input$upper)})
    
    filtered2 <- reactive({vars %>% dplyr::filter(state %in% input$state) %>%
            dplyr::filter(year%in%input$year)%>%
            dplyr:: filter(upper%in%input$upper)})
    
    output$view <- renderVisNetwork({
        nodes2 <- as.data.frame(filtered2())
        names(nodes2)[3] <- "party2"
        names(nodes2)[16] <- "value"
        nodes2$value <- nodes2$value *3
        names(nodes2)[63] <- "color"
        edges2 <- as.data.frame(filtered1()[1:2])
        visNetwork(nodes2, edges2, width = "90%") %>%
            visNodes(shape = "circle", label = "id", color = "color") %>%
            visEdges(arrows = "from", smooth = F) %>%
            visOptions(selectedBy = list(variable = "party2", multiple = T), highlightNearest = T, nodesIdSelection = T) %>%
            visPhysics(stabilization = F,
                       enabled = T,hierarchicalRepulsion = list(centralGravity = 1000, nodeDistance = 5000), 
                       forceAtlas2Based = list(avoidOverlap = 1), barnesHut = list(gravitationalConstant = -1000, avoidOverlap = 0.2))
    })
    
}
shinyApp(ui = ui, server = server)

