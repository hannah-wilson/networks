library(igraph)
library(network)
library(dplyr)
library(sna)
library(RColorBrewer)
library(ggraph)
library(networkD3)
library(visNetwork)
library(ndtv)
library(plotly)
library(shiny)
library(shinyWidgets)
library(bslib)                                                             #Please note that data for Nebraska and South Carolina are only available for the Senate and House, respectively.")))))
library(visNetwork)
library(shiny)
library(stringr)
setwd("/Users/hannahwilson/Documents/Notre Dame/Dissertation/NetInf Data/Plots/")
vars <- read.csv("pooledallpartyids.csv")
edges <- read.csv("edgesall.csv")

#Select Upper/Lower
library(shiny)
ui <- fluidPage(titlePanel("Create your own interactive network"), 
                       sidebarLayout(position = "left", sidebarPanel(selectInput(inputId = "state", "Pick a state",
                                                                                    c("AK" = "AK",
                                                                                      "IL" = "IL",
                                                                                      "IN" = "IN",
                                                                                      "MN" = "MN",
                                                                                      "NE" = "NE",
                                                                                      "OK" = "OK",
                                                                                      "SC" = "SC")),
                                           radioButtons("year", "Select a year", c("10" = "10", "12" = "12", "14" = "14", "16" = "16", "18" ="18")),
                                           radioButtons("upper", "Select a chamber", c("Lower" = "L", "Upper" = "U"))),mainPanel = visNetworkOutput("view")))
                                       

server <- function(input, output, session) { 
   filtered1 <- reactive({ edges %>% dplyr::filter(state %in% input$state)%>%
         dplyr::filter(year%in%input$year) %>%
         dplyr::filter(upper%in%input$upper)})

   filtered2 <- reactive({vars %>% dplyr::filter(state %in% input$state) %>%
         dplyr::filter(year%in%input$year)%>%
         dplyr:: filter(upper%in%input$upper)})

      output$view <- renderVisNetwork({
         nodes2 <- as.data.frame(filtered2())
         names(nodes2)[2] <- "party2"
         names(nodes2)[16] <- "value"
         nodes2$value <- nodes2$value *3
         names(nodes2)[63] <- "color"
         edges2 <- as.data.frame(filtered1()[1:2])
         visNetwork(nodes2, edges2, width = "100%") %>%
            visNodes(shape = "circle", label = "id", color = "color") %>%
            visEdges(arrows = "from", smooth = F) %>%
            visOptions(selectedBy = list(variable = "group", multiple = T), highlightNearest = T, nodesIdSelection = T) %>%
            visPhysics(stabilization = F,
                       enabled = T,hierarchicalRepulsion = list(centralGravity = 1000, nodeDistance = 5000), 
                       forceAtlas2Based = list(avoidOverlap = 1), barnesHut = list(gravitationalConstant = -1000, avoidOverlap = 0.2))
         })
      
}


shinyApp(ui = ui, server = server)
