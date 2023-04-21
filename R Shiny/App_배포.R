load("./06_geodataframe/06a_apt_price.rdata")
library(sf)
#서울시 경계선
bnd <- st_read("./01_code/sigun_bnd/seoul.shp")
load("./07_map/07_kde_high.rdata") # 최고가 래스터이미지
load("./07_map/07_kde_hot.rdata") # 급등지역 래스터이미지
#서울시 격자 그리드
grid <- st_read("./01_code/sigun_grid/seoul.shp")
#1단계 마커클러스터링
pcnt_10 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by=.1))[1]) 
pcnt_90 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by=.1))[9])

load("./01_code/circle_marker/circle_marker.rdata") 
circle.colors <- sample(x=c("red", "green", "blue"), size=1000, replace = TRUE)
#2단계 반응형 지도 만들기
library(leaflet)
library(purrr)
library(raster)
leaflet() %>% 
  #기본맵 설정 : 오픈스트리트맵
  addTiles(options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>% 
  # 최고가 지역 KDE(kernal density estimate 커널 밀도 추정)
  addRasterImage(raster_high, 
                 colors = colorNumeric(c("blue", "green","yellow","red"), 
                                       values(raster_high), na.color = "transparent"), opacity = 0.4, 
                 group = "2021 최고가") %>%
  # 급등지 지역 KDE
  addRasterImage(raster_hot, 
                 colors = colorNumeric(c("blue", "green","yellow","red"), 
                                       values(raster_hot), na.color = "transparent"), opacity = 0.4, 
                 group = "2021 급등지") %>%
  #스위치 메뉴
  addLayersControl(baseGroups = c("2021 최고가", "2021 급등지"), 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  #서울시 외곽경계
  addPolygons(data=bnd, weight = 3, stroke = T, color = "red", 
              fillOpacity = 0) %>%
  # 마커 클러스터링
  addCircleMarkers(data = apt_price, lng =unlist(map(apt_price$geometry,1)), 
                   lat = unlist(map(apt_price$geometry,2)), radius = 10, stroke = FALSE, 
                   fillOpacity = 0.6, fillColor = circle.colors, weight=apt_price$py, 
                   clusterOptions = markerClusterOptions(iconCreateFunction=JS(avg.formula)))
  
  #그리드 추가
grid <- st_read("./01_code/sigun_grid/seoul.shp")
grid <- as(grid, "Spatial"); grid <- as (grid, "sfc")
grid <- grid[which(sapply(st_contains(st_sf(grid), apt_price), length) > 0)]
plot(grid)

m <- leaflet() %>%
  addTiles(options = providerTileOptions(minZoom =9, maxZoom =18)) %>%
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
  
  addPolygons(data = bnd, weight = 3, stroke = T, color = "red",
              fillOpacity = 0) %%  
  #마커 클러스터링
  addCircleMarkers(data=apt_price, lng = unlist(map(apt_price$geometry,1)),
                   lat = unlist(map(apt_price$geometry,2)), radius = 10, stroke = FALSE,
                   fillOpacity =  0.6, fillColor = circle.colors, weight = apt_price$py,
                   clusterOptions = markerClusterOptions(iconCreateFuntion = JS(avg.formula))) %>%
leafem ::addFeatures(st_sf(grid), layerId= ~seq_len(length(grid)), color = "grey")
m

library(shiny)
library(mapedit)
library(dplyr)

ui <- fluidPage(
  selectModUI("selectmap"),
  "선택은 할 수 있지만 아무런 반응이 없습니다."
server <- function(input, output) {
  callModule(selectMod, "selectmap", m)}

shinyApp(ui, server)

ui <- fluidPage(
  selectModUI("selectmap"), 
  textOutput("sel")
)
server <- function(input, output, session) {
  df <- callModule(selectMod, "selectmap", m)
  output$sel <- renderPrint({df()[1]})
}
shinyApp(ui, server)
install.packages("dt")
library(DT)
ui <- fluidPage(
  fluidRow(
    column(9, selectModUI("selectmap"), div(style = "height:45px")),
    column(3, 
           sliderInput("range_area", "전용면적", sep="", min=0, max =350,
                       value))
  )
)
  
)