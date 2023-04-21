load("./06_geodataframe/06_apt_price.rdata")
load("./07_map/07_kde_high.rdata") 
load("./07_map/07_kde_hot.rdata") 

library(sf)
grid <- st_read("./06_geodataframe/seoul.shp")
bnd <- st_read("./06_geodataframe/seoul.shp")

#1단계 이상치 설정
pcnt_10 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by=.1))[1]) 
pcnt_90 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by=.1))[9])

load("./circle_marker/circle_marker.rdata")
circle.colors <- sample(x=c("red", "green", "blue"), size=1000, replace = TRUE)

#2단계 마카 클러스트링 시각화
#install.packages("purrr")
library(purrr)
 leaflet() %>%
  addTiles() %>% 
  addPolygons(data=bnd, weight = 3, color = "red" , fill = NA) %>%
  #최고가 레스터 이미지 불러오기
   
  addRasterImage(raster_high,
                 colors = colorNumeric(c("blue","green", "yellow", "red"),
                                      values(raster_high), na.color = "transparent"), 
                 opacity = 0.4, group = "2021 최고가" ) %>%

  addRasterImage(raster_hot,
                 colors = colorNumeric(c("blue","green", "yellow", "red"),
                                       values(raster_hot), na.color = "transparent"), 
                 opacity = 0.4, group = "2021 급등지" ) %>%
  addLayersControl(baseGroups = c("2021 최고가", "2021 급등지"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addCircleMarkers(data=apt_price, lng = unlist(map(apt_price$geometry,1)),
                   lat = unlist(map(apt_price$geometry,2)), radius = 10, stroke = FALSE,
                   fillOpacity =  0.6, fillColor = circle.colors, weight = apt_price$py,
                   clusterOptions = markerClusterOptions(iconCreateFuntion = JS(avg.formula)))
 
 rm(list = ls())
                   