library(dplyr)
library(readr)
library(leaflet)
library(leaflet.extras)
library(scales)
library(sp)

cityCode <- list('青岛市'='370200',
                 '焦作市'='410800',
                 '信阳市' = '411500',
                 '廊坊市' = '131000',
                 '黄山市' ='341000',
                 '日照市' = '371100',
                 '合肥市' = '340100',
                 '承德市' = '130800'
                 )

mapInfo <- function(city){
 mapGeo <- leafletGeo(city)
 info <- data.frame(id = mapGeo$id, district = mapGeo$name, city = city, lng = coordinates(mapGeo)[,1], lat = coordinates(mapGeo)[,2])
 return(info)
}

mapView <- lapply(names(cityCode), mapInfo)
names(mapView) <- names(cityCode)

adCodeSet <- lapply(unlist(cityCode), function(x) read_csv(paste0("data/", x, ".csv"), col_types = cols(type = col_character(), lat = col_double(), lng = col_double())) %>%
              select(province, city, district) %>% unique) %>% do.call(rbind, .)

# 2017.10.30 即墨市升级为即墨区，仍旧按照网格命名规则
# 2015.11，撤销密云县，设立密云区, 仍旧按照网格命名规则
# 2015.11，撤销延庆县，设立延庆区, 仍旧按照网格命名规则
adCodeSet <- mutate(adCodeSet, district = ifelse(district == '即墨区', '即墨市', ifelse(district == '密云区', '密云县', ifelse(district == '延庆区', '延庆县', district)))) %>% unique


poi_extractor <- function(cityCode, high_level = TRUE){
  if(high_level){
    level <- 3
  }else{
    level <- 2
  }
  data <- read_csv(paste0("data/", cityCode, ".csv"), col_types = cols(type = col_character(), lat = col_double(), lng = col_double())) %>% mutate(., lng2 = round(lng,level), lat2 = round(lat, level))
  
  # 2017.10.30 即墨市升级为即墨区，仍旧按照网格命名规则
  # 2015.11，撤销密云县，设立密云区, 仍旧按照网格命名规则
  data <- mutate(data, district = ifelse(district == '即墨区', '即墨市', ifelse(district == '密云区', '密云县',  ifelse(district == '延庆区', '延庆县', district))))
  
  bus <- filter(data, grepl("1507[0-9]*", type))
  parking <- filter(data, grepl("1509[0-9]*", type))
  bank <- filter(data, grepl("1601[0-9]*", type))
  residence <- filter(data, grepl("1203[0-9]*", type))
  mart <- filter(data, grepl("0604[0-9]*", type))
  emart <- filter(data, grepl("0603[0-9]*", type))
  mall <- filter(data, grepl("0601[0-9]*",type))
  metro <- filter(data, grepl("150501", type))
  
  # 5-dimension factors
  traffic <- bind_rows(bus, parking, metro) %>% select(., province, city, district, lng2, lat2) %>% group_by(province, city, district, lng2, lat2) %>% count %>% filter(!is.na(lng2)&!is.na(lat2))
  business <- bind_rows(mart, mall) %>% select(., province, city, district, lng2, lat2) %>% group_by(province, city, district, lng2, lat2) %>% count %>% filter(!is.na(lng2)&!is.na(lat2))
  competition <- bind_rows(emart) %>% select(., province, city, district, lng2, lat2) %>% group_by(province, city, district, lng2, lat2) %>% count %>% filter(!is.na(lng2)&!is.na(lat2))
  finance <- bind_rows(bank) %>% select(., province, city, district, lng2, lat2) %>% group_by(province, city, district, lng2, lat2) %>% count %>% filter(!is.na(lng2)&!is.na(lat2))
  residence <- bind_rows(residence) %>% select(., province, city, district, lng2, lat2) %>% group_by(province, city, district, lng2, lat2) %>% count %>% filter(!is.na(lng2)&!is.na(lat2))
  
  poi <- list(traffic = traffic,
              business = business,
              competition = competition,
              finance = finance,
              residence = residence)
  
  return(poi)
}

heat_builder <- function(poi = list(), high_level = TRUE, type = c('Balance', 'Competitor', 'Community', 'Market')){
  traffic <- poi[['traffic']]
  business <- poi[['business']]
  competition <- poi[['competition']]
  finance <- poi[['finance']]
  residence <- poi[['residence']]
  
  if(type == 'Balance'){
  score <- inner_join(traffic, business, by = c('lng2' = 'lng2', 'lat2' = 'lat2')) %>%
    inner_join(competition, by = c('lng2' = 'lng2', 'lat2' = 'lat2')) %>%
    #inner_join(finance, by = c('lng2' = 'lng2', 'lat2' = 'lat2')) %>%
    inner_join(residence, by = c('lng2' = 'lng2', 'lat2' = 'lat2'))
  heatscore <- mutate(score, n = n.x*n.y*n.x.x*n.y.y) %>% select(lng2, lat2, n)
  }else if(type == 'Competitor'){
    score <- inner_join(competition, business, by = c('lng2' = 'lng2', 'lat2' = 'lat2')) %>%
      inner_join(traffic, by = c('lng2' = 'lng2', 'lat2' = 'lat2'))
    heatscore <- mutate(score, n = n.x*n.y*n) %>% select(lng2, lat2, n)
  }else if(type == 'Community'){
    score <- inner_join(residence, traffic, by = c('lng2' = 'lng2', 'lat2' = 'lat2'))
    heatscore <- mutate(score, n = n.x*n.y) %>% select(lng2, lat2, n)
  }else if(type == 'Market'){
    score <- inner_join(business, finance, by = c('lng2' = 'lng2', 'lat2' = 'lat2')) %>%
      inner_join(traffic,  by = c('lng2' = 'lng2', 'lat2' = 'lat2'))
    heatscore <- mutate(score, n = n.x*n.y*n) %>% select(lng2, lat2, n)
  }else{
    score <- inner_join(traffic, business, by = c('lng2' = 'lng2', 'lat2' = 'lat2')) %>%
      inner_join(competition, by = c('lng2' = 'lng2', 'lat2' = 'lat2')) %>%
      inner_join(finance, by = c('lng2' = 'lng2', 'lat2' = 'lat2')) %>%
      inner_join(residence, by = c('lng2' = 'lng2', 'lat2' = 'lat2'))
    heatscore <- mutate(score, n = n.x*n.y*n.x.x*n.y.y*n) %>% select(lng2, lat2, n)
  }
  
  heattraffic <- traffic
  heatbusiness<- business
  heatcompetition <- competition
  heatfinance <- finance
  heatresidence <- residence
  
  if(nrow(heatscore) == 0){
    heatscore$opacity <- numeric(0)
  }
  else if(!high_level){
    m_n <- mean(heatscore$n)
    heatscore <- filter(heatscore, n > m_n) %>%arrange(desc(n))
    heatscore$opacity <- rescale(heatscore$n, to=c(0,1))
  }else{
    heatscore$opacity <- rescale(heatscore$n, to=c(0,1))
  }
  
  heat <- list(traffic = heattraffic,
               business = heatbusiness,
               competition = heatcompetition,
               finance = heatfinance,
               residence = heatresidence,
               score = heatscore)
  return(heat)
}

map_generator <- function(dataList, baseMap, category = c('traffic', 'business', 'competition', 'finance', 'residence', 'score'), radius = 200){
  if(category == 'score'){
    baseMap %>%
      addCircles(data = dataList[[category]], lng = ~lng2, lat = ~lat2, color = 'red', opacity = ~opacity, radius = radius)
  }else if(category %in%c('traffic', 'business', 'competition', 'finance', 'residence')){
    baseMap %>%
      addHeatmap(data = dataList[[category]], lng = ~lng2, lat = ~lat2, intensity = ~n, blur = 20, max = 0.05, radius = 15)
  }else{
    stop('Note that type or category input is not correct.')
  }
}
