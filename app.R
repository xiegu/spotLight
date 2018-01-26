library(leaflet)
library(leafletCN)
library(leaflet.extras)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyjs)

load('data/haierTable.RData')

source('util.R', local = TRUE)

ui <- navbarPage(title = 'Spotlight',
                 tabPanel(title = 'Interactive map',
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                              ),
                              leafletOutput('SpotMap', width = '100%', height = '100%'),
                              absolutePanel(id = "controls", class = "panel panel-default",
                                         draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                                         width = 250, height = "auto", cursor = "default",
                                         h3("Spot explorer"),
                                         br(),
                                         selectInput('Province', 'Province', choices = c('山东省', '河南省')),
                                         selectInput('City', 'City', choices = names(cityCode), selected = names(cityCode)[1]),
                                         selectInput('District', 'District', choices = filter(adCodeSet, city == names(cityCode)[1])%>%pull(district)),
                                         selectInput('Indicator', 'Indicator', choices = c('Traffic' = 'traffic',
                                                                                           'Business' = 'business',
                                                                                           'Competition' = 'competition',
                                                                                           'Finance' = 'finance',
                                                                                           'Residence' = 'residence',
                                                                                           'Hot point' = 'score'), selected = 'score'),
                                         useShinyjs(),
                                         awesomeRadio('SpotType', 
                                                      label = "Spot type", choices = c("Balance",
                                                                                       'Competitor',
                                                                                       'Community',
                                                                                       'Market'), selected = "Balance", checkbox = TRUE),
                                         switchInput(
                                           'Level',
                                           label = 'Spot level',
                                           offLabel = 'Low',
                                           onLabel = 'High',
                                           value = TRUE,
                                           labelWidth = '100px'
                                           ),
                                         br(),
                                         sliderInput(
                                           'ScoreScope',
                                           'Scope',
                                           min = 100,
                                           max = 500,
                                           value = 200,
                                           step = 100
                                         )
                                         
                              )
                          )
                          
                 )
                 
)

server <- function(input, output, session){
  observe({
    updateSelectInput(session, 'City', label = 'City', choices = filter(adCodeSet, province == input$Province) %>% pull(city) %>% unique)
  })
  
  observe({
      updateSelectInput(session, 'District', label = 'District', choices = filter(adCodeSet, city == input$City) %>% pull(district))
  })
  
  poi <- reactive({
    city <- input$City
    data <- poi_extractor(cityCode[[city]], input$Level)
    return(data)
  })
  
  heat <- reactive({
    poiDistrict <- poi()%>%lapply(., function(x) filter(x, city == input$City & district == input$District) %>% ungroup %>% select(lng2, lat2, n))
    return(heat_builder(poiDistrict, input$Level, input$SpotType))
  })
  
  haierCity <- reactive({
    return(filter(haierTable, city == input$City))
  })
 
  scoreCheck <- reactive({
    return(ifelse(input$Indicator == 'score', TRUE, FALSE))
  })
  
  observe({
    toggle('SpotType', condition = scoreCheck(), anim = TRUE, time = 0.2)
  })
  
  output$SpotMap <- renderLeaflet({
    baseData <- haierCity()
    baseMap <- leaflet() %>% amap(group = "Normal") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
      addAwesomeMarkers(data = baseData, ~longitude, ~latitude,
                 label = ~name,
                 labelOptions = labelOptions(style = list("font-weight" = "normal", "font-size" = "15px")),
                 icon = awesomeIcons(
                   icon = "shopping-cart", library = "fa", markerColor = "darkblue",
                   iconColor = 'gold'
                 ),
                 group = '海尔专卖店'
      ) %>%
      addLayersControl(
        baseGroups = c('Normal', 'Dark'),
        overlayGroups = c('海尔专卖店'),
        position = 'topleft',
        options = layersControlOptions(collapsed = FALSE)
      )
    baseMap
  })
  
  observe({
    indicator <- input$Indicator
    scope <- input$ScoreScope
    city <- input$City
    sub <- input$District
    viewLng <- mapView[[city]] %>% filter(district == sub) %>% pull(lng)
    viewLat <- mapView[[city]] %>% filter(district == sub) %>% pull(lat)
    leafletProxy('SpotMap') %>% setView(lng = viewLng, lat = viewLat, zoom = 12) %>% clearShapes() %>% clearHeatmap() %>% map_generator(dataList = heat(), category = indicator, radius = scope)
  })
}

shinyApp(ui, server)