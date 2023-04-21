load("./06_geodataframe/06_apt_price.rdata")
library(sf)
#서울시 경계선
load("./07_map/07_kde_high.rdata") # 최고가 래스터이미지
load("./07_map/07_kde_hot.rdata") # 급등지역 래스터이미지
#서울시 격자 그리드
grid <- st_read("./01_code/sigun_grid/seoul.shp") 

#1단계 마커클러스터링
pcnt_10 <-as.numeric(quantile(apt_price$py, probs=seq(.1,.9,by=.1))[1])
pcnt_90 <-as.numeric(quantile(apt_price$py, probs=seq(.1,.9,by=.1))[9]) 
load("./01_code/circle_marker/circle_marker.rdata")
circle.colors <- sample(x=c("red","green","blue"), size=1000, replace=TRUE)

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

#4단계지도 어플리케이션 만들기 
grid <- st_read("./01_code/sigun_grid/seoul.shp") # 그리드 로드
grid <- as(grid, "Spatial") ; grid <- as(grid, "sfc") #변화
grid <- grid[which(sapply(st_contains(st_sf(grid),apt_price),length) > 0)]  
plot(grid)

#5단계 반응형 지도 모듈화 
m <- leaflet() %>% 
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
                   clusterOptions = markerClusterOptions(iconCreateFunction=JS(avg.formula))) %>% 
  #그리드 추가
  leafem::addFeatures(st_sf(grid), layerID=~seq_len(length(grid)), color='grey')
m

#5단계 샤이니와 mapedit 애플리케이션 구동
#install.packages("mapedit")
library(shiny)
library(dplyr) 
library(mapedit)
# 5-1단계 UI
ui <- fluidPage(
  selectModUI('selectmap'), #그리드 선택 모듈
  "선택은 할수 있지만 아무 반응이 없음"
)
server <- function(input, output) {
  callModule(selectMod, "selectmap", m)
}
# 5-2단계
shinyApp(ui, server)


# 6단계 반응식 추가
ui <- fluidPage( 
  selectModUI("selectmap"),
  textOutput("sel")
)
#---# 서버
server <- function(input, output, session) {       
  df <- callModule(selectMod, "selectmap", m)
  output$sel <- renderPrint({df()[1]})
} 
shinyApp(ui, server)

#7단계 반응형 지도 애플리케이션 완성 
#install.packages("DT")
library(DT)
ui <- fluidPage(
fluidRow(  
  column( 9, selectModUI("selectmap"), div(style = "height:45px")),
  column( 3,
          sliderInput("range_area", "전용면적", sep = "", min = 0, max = 350, 
                      value = c(0, 200)),
          sliderInput("range_time", "건축 연도", sep = "", min = 1960,  max = 2020, 
                      value = c(1980, 2020)), ),
  #---# 하단 화면: 테이블 출력
  column(12, dataTableOutput(outputId = "table"), div(style = "height:200px")))) 

#---# 서버
server <- function(input, output, session) {       
  # 반응식 설정 : 슬라이더 입력 필터링
  apt_sel = reactive({
   apt_sel = subset(apt_price, con_year >= input$range_time[1] &
                      con_year <= input$range_time[2] & 
                        area >= input$range_area[1] & 
                      area <= input$range_area[2] )
   return(apt_sel)})
  g_sel <- callModule(selectMod, "selectmap",
            leaflet() %>% 
            addTiles(options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>%
            addRasterImage(raster_high, 
              colors = colorNumeric(c("blue", "green","yellow","red"), 
                      values(raster_high), na.color = "transparent"), opacity = 0.4,
                      group = "2021 최고가") %>%
            addRasterImage(raster_hot, 
               colors = colorNumeric(c("blue", "green","yellow","red"), 
                       values(raster_hot), na.color = "transparent"), opacity = 0.4, 
                       group = "2021 급등지") %>%
            addLayersControl(baseGroups = c("2021 최고가", "2021 급등지"), 
                options = layersControlOptions(collapsed = FALSE)) %>%
            addPolygons(data=bnd, weight = 3, stroke = T, color = "red", 
                     fillOpacity = 0) %>%
            addCircleMarkers(data = apt_price, lng =unlist(map(apt_price$geometry,1)), 
                      lat = unlist(map(apt_price$geometry,2)), radius = 10, stroke = FALSE, 
                       fillOpacity = 0.6, fillColor = circle.colors, weight=apt_price$py, 
                       clusterOptions = markerClusterOptions(iconCreateFunction=JS(avg.formula))) %>% 
           leafem::addFeatures(st_sf(grid), layerID=~seq_len(length(grid)), color='grey')                )
  rv <- reactiveValues(intersect=NULL, selectgrid=NULL) 
  #---# 반응 결과(rv: reactive value) 저장
  observe({
    gs <- g_sel() 
    rv$selectgrid <- st_sf(grid[as.numeric(gs[which(gs$selected==TRUE),"id"])])
    if(length(rv$selectgrid) > 0){
      rv$intersect <- st_intersects(rv$selectgrid, apt_sel())
      rv$sel       <- st_drop_geometry(apt_price[apt_price[unlist(rv$intersect[1:10]),],])
    } else {
      rv$intersect <- NULL
    }
  })
  #반응 결과 렌더링
  output$table <- DT::renderDataTable({
    dplyr::select(rv$sel, ymd, addr_1, apt_nm, price, area, floor, py) %>%
      arrange(desc(py))}, extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                 scrollY = 300, scrollCollapse = T, paging = TRUE, buttons = c('excel')))
} 
shinyApp(ui, server)

