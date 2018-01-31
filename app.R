library(leaflet)
library(leafletCN)
library(leaflet.extras)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyjs)

load('data/haierTable.RData')

source('util.R', local = TRUE)

ui <- navbarPage(title = 'Spots',
                 tabPanel(title = 'Network spot',
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                              ),
                              leafletOutput('SpotMap', width = '100%', height = '100%'),
                              absolutePanel(id = "control", class = "panel panel-default",
                                         draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                                         width = 250, height = "auto", cursor = "default",
                                         h3("Spot explorer"),
                                         br(),
                                         selectInput('Province', 'Province', choices = c('山东省', '河南省', '河北省', '安徽省')),
                                         selectInput('City', 'City', choices = names(cityCode), selected = names(cityCode)[1]),
                                         selectInput('District', 'District', choices = filter(adCodeSet, city == names(cityCode)[1])%>%pull(district)),
                                         selectInput('Indicator', 'Indicator', choices = c('Traffic' = 'traffic',
                                                                                           'Business' = 'business',
                                                                                           'Competition' = 'competition',
                                                                                           'Finance' = 'finance',
                                                                                           'Residence' = 'residence',
                                                                                           'Hot point' = 'score'), selected = 'score'),
                                         switchInput(
                                           'Level',
                                           label = 'Spot level',
                                           offLabel = 'Low',
                                           onLabel = 'High',
                                           value = TRUE,
                                           labelWidth = '100px'
                                         ),
                                         br(),
                                         
                                         useShinyjs(),
                                         awesomeRadio('ModelType', 
                                                      label = "Spot type", choices = c("Balance",
                                                                                       'Competitor',
                                                                                       'Community',
                                                                                       'Market'), selected = "Balance", checkbox = TRUE),
                                         sliderInput(
                                           'ScoreScope',
                                           'Scope (m)',
                                           min = 100,
                                           max = 500,
                                           value = 100,
                                           step = 100
                                         )
                                         
                              )
                          )
                          
                 ),
                 tabPanel(title = 'On-demand spot',
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                              ),
                              leafletOutput('OnDemandMap', width = '100%', height = '100%'),
                              absolutePanel(id = "control3", class = "panel panel-default",
                                            draggable = TRUE, top = 50, left = "auto", right = "40%", bottom = "auto",
                                            width = 250, height = "auto", cursor = "default",
                                            h4("Spot coordinates"),
                                            uiOutput("ClickedLocation"),
                                            actionButton("Reset", "Reset", class = "btn-primary")
                                            ),
                              absolutePanel(id = "control2", class = "panel panel-default",
                                            draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                                            width = 250, height = "auto", cursor = "default",
                                            h3("Spot explorer"),
                                            br(),
                                            selectInput('Province2', 'Province', choices = c('山东省', '河南省', '河北省', '安徽省')),
                                            selectInput('City2', 'City', choices = names(cityCode), selected = names(cityCode)[1]),
                                            selectInput('District2', 'District', choices = filter(adCodeSet, city == names(cityCode)[1])%>%pull(district)),
                                            sliderInput('LocationRange', 'Search range (m)', min = 500, max = 5000, value = 1000, step = 500),
                                            selectInput('Indicator2', 'Indicator', choices = c('Traffic' = 'traffic',
                                                                                              'Business' = 'business',
                                                                                              'Competition' = 'competition',
                                                                                              'Finance' = 'finance',
                                                                                              'Residence' = 'residence',
                                                                                              'Hot point' = 'score'), selected = 'score'),
                                            switchInput(
                                              'Level2',
                                              label = 'Spot level',
                                              offLabel = 'Low',
                                              onLabel = 'High',
                                              value = TRUE,
                                              labelWidth = '100px'
                                            ),
                                            br(),
                                            
                                            useShinyjs(),
                                            awesomeRadio('ModelType_2', 
                                                         label = "Spot type", choices = c("Balance",
                                                                                          'Competitor',
                                                                                          'Community',
                                                                                          'Market'), selected = "Balance", checkbox = TRUE),
                                            sliderInput(
                                              'ScoreScope2',
                                              'Scope (m)',
                                              min = 100,
                                              max = 500,
                                              value = 100,
                                              step = 100
                                            )
                                            
                              )
                          )
                          
                 )
                 
)

server <- function(input, output, session){
  # Network spot
  observe({
    updateSelectInput(session, 'City', label = 'City', choices = filter(adCodeSet, province == input$Province) %>% pull(city) %>% unique)
  })
  
  observe({
      updateSelectInput(session, 'District', label = 'District', choices = filter(adCodeSet, city == input$City) %>% pull(district))
  })
  
  observe({
    level <- input$Level
    updateSliderInput(session, 'ScoreScope', label = 'Scope (m)', min = ifelse(input$Level, 100, 500), max = ifelse(input$Level, 500, 2000), value = ifelse(input$Level, 100, 1000), step = ifelse(input$Level, 100, 500))
  })
  
  poi <- reactive({
    city <- input$City
    data <- poi_extractor(cityCode[[city]], input$Level)
    return(data)
  })
  
  heat <- reactive({
    poiDistrict <- poi()%>%lapply(., function(x) filter(x, city == input$City & district == input$District) %>% ungroup %>% select(lng2, lat2, n))
    return(heat_builder(poiDistrict, input$Level, input$ModelType))
  })
  
  haierCity <- reactive({
    return(filter(haierTable, city == input$City))
  })
 
  scoreCheck <- reactive({
    return(ifelse(input$Indicator == 'score', TRUE, FALSE))
  })
  
  observe({
    toggle('ModelType', condition = scoreCheck(), anim = TRUE, time = 0.2)
  })
  
  observe({
    toggle('ScoreScope', condition = scoreCheck(), anim = TRUE, time = 0.2)
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
  
  # On-demand spot
  observe({
    updateSelectInput(session, 'City2', label = 'City', choices = filter(adCodeSet, province == input$Province2) %>% pull(city) %>% unique)
  })
  
  observe({
    updateSelectInput(session, 'District2', label = 'District', choices = filter(adCodeSet, city == input$City2) %>% pull(district))
  })
  
  observe({
    level <- input$Level
    updateSliderInput(session, 'ScoreScope2', label = 'Scope (m)', min = ifelse(input$Level2, 100, 500), max = ifelse(input$Level2, 500, 2000), value = ifelse(input$Level2, 100, 1000), step = ifelse(input$Level2, 100, 500))
  })
  
  output$ClickedLocation <- renderUI({
    if(is.null(mapClick$clicked)){
      return(NULL)
    }else{
      wellPanel(
        p(paste0('Longitude: ', round(mapClick$clicked[['lng']], 4))), 
        p(paste0('Latitude: ', round(mapClick$clicked[['lat']], 4)))
      )
    }
  })
  
  poi2 <- reactive({
    city <- input$City2
    data <- poi_extractor(cityCode[[city]], input$Level2)
    return(data)
  })
  
  heat2 <- reactive({
    poiDistrict <- poi2()%>%lapply(., function(x) filter(x, city == input$City2 & district == input$District2) %>% ungroup %>% select(lng2, lat2, n))
    return(heat_builder(poiDistrict, input$Level2, input$ModelType_2))
  })
  
  haierCity2 <- reactive({
    return(filter(haierTable, city == input$City2))
  })
  
  scoreCheck2 <- reactive({
    return(ifelse(input$Indicator2 == 'score', TRUE, FALSE))
  })
  
  observe({
    toggle('ScoreScope2', condition = scoreCheck2(), anim = TRUE, time = 0.2)
  })
  
  observe({
    toggle('ModelType_2', condition = scoreCheck2(), anim = TRUE, time = 0.2)
  })
  
  output$OnDemandMap <- renderLeaflet({
    baseData <- haierCity2()
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
  
  mapClick <- reactiveValues(clicked = NULL)
  
  observe({
  mapClick$clicked <- input$OnDemandMap_click
  })
  
  observeEvent(input$Reset, {
    mapClick$clicked <- NULL
  })
  
  observe({
    indicator <- input$Indicator2
    scope <- input$ScoreScope2
    city <- input$City2
    sub <- input$District2
    range <- input$LocationRange
    viewLng <- mapView[[city]] %>% filter(district == sub) %>% pull(lng)
    viewLat <- mapView[[city]] %>% filter(district == sub) %>% pull(lat)
    if(is.null(mapClick$clicked)){
      leafletProxy('OnDemandMap') %>% setView(lng = viewLng, lat = viewLat, zoom = 12) %>% clearShapes() %>% clearHeatmap()
    }else{
      center <- c(mapClick$clicked[['lng']], mapClick$clicked[['lat']])
      heat <- lapply(heat2(), function(x) mutate(x, distance = ((lng2-center[1])*111000)^2 + ((lat2-center[2])*111000*cos(center[2]/180*pi))^2, isInCircle = ifelse(distance <=range^2, TRUE, FALSE)) %>% 
                       filter(isInCircle == TRUE))
      leafletProxy('OnDemandMap') %>% setView(lng = center[1], lat = center[2], zoom = 14) %>% clearShapes() %>% clearHeatmap() %>% 
        addCircles(lng = center[1], lat = center[2], radius = range) %>%
        map_generator(dataList = heat, category = indicator, radius = scope)
    }
      
      #
  })
  
  
}

shinyApp(ui, server)