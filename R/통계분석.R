install.packages("sf")
install.packages("raster")
library(sf)

load("./06_geodataframe/06_apt_price.rdata")
load("./07_map/07_kde_high.rdata") 
grid <- st_read("./06_geodataframe/seoul.shp")

#1단계 관심지역 그리드 찾기
#install.packages("tmap")
install.packages("tmap")
library(tmap)
tmap_mode('view')
tm_shape(grid)+ tm_borders() + tm_text('ID', col = "red") + 
  tm_shape(raster_high) + 
  tm_raster(palette = c("blue", "green", "yellow","red" ), alpha = .4) +
  tm_basemap(server = c('OpenStreetMap'))

#2단계 전체지역 / 관심지역 저장하기
library(dplyr)
apt_price <-st_join(apt_price, grid, join = st_intersects)
apt_price <- apt_price %>% st_drop_geometry()
all <- apt_price
sel <- apt_price %>% filter(ID == 81016) 
dir.create("08_chart")
save(all, file="./08_chart/all.rdata")
save(sel, file="./08_chart/sel.rdata")
rm(list=ls())
