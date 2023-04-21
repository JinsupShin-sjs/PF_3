setwd('c:/Workplace/R_shiny')  
load("./06_geodataframe/06_apt_price.rdata") 
library(sf)
grid <- st_read("./06_geodataframe/seoul.shp")

#1단계 실거래가 + 데이터결합
apt_price <- st_join(apt_price, grid, join=st_intersects)
head(apt_price, 2)
kde_high <- aggregate(apt_price$py, by=list(apt_price$ID), mean)
colnames(kde_high) <- c("ID","avg_price")
head(kde_high, 2)
#2단계 kde_high , grid 합쳐서 지도에 표시
kde_high <- merge(grid, kde_high, by="ID")
head(kde_high, 2)
library(ggplot2)
library(dplyr)
kde_high %>% ggplot(aes(fill=avg_price)) + geom_sf() + scale_fill_gradient(low="white", high = "red")

#3단계 지도 경계
library(sp)
kde_high_sp <- as(st_geometry(kde_high), "Spatial") #sf 형 -> sp 형 볂환
x <- coordinates(kde_high_sp)[,1]
y <- coordinates(kde_high_sp)[,2]

#3-1 기준경계 설정
l1 <- bbox(kde_high_sp) [1,1] - (bbox(kde_high_sp) [1,1] * 0.0001)
l2 <- bbox(kde_high_sp) [1,2] + (bbox(kde_high_sp) [1,2] * 0.0001)
l3 <- bbox(kde_high_sp) [2,1] - (bbox(kde_high_sp) [2,1] * 0.0001)
l4 <- bbox(kde_high_sp) [2,2] + (bbox(kde_high_sp) [1,1] * 0.0001)

install.packages("spatstat")
library(spatstat)

win <- owin(xrange = c(l1,l2), yrange = c(l3,l4))
plot(win)
#3-2 지도(밀도그래프) 넣기
#경계값 위에 좌표값 포인트 생성
#rm(list = c("kde_high_sp", "apt_price", "l1", "l2","l3","l4"))

p <- ppp(x, y, window = win)
d <- density.ppp(p, weights=  kde_high$avg_price, #커널밀도 함수로 변환
                 sigma = bw.diggle(p),
                 kernel = 'gaussian')
plot(d) 

#rm(list = c("x", "y", "win", "p"))


d[d < quantile(d)[4] + (quantile(d)[4] *0.1)] <- NA
install.packages("raster")
library(raster)
raster_high <- raster(d)
plot(raster_high)

#3-3
bnd <- st_read("./06_geodataframe/seoul.shp")
raster_high <- crop(raster_high, extent(bnd))
crs(raster_high) <- sp::CRS("+proj=longlat + datum=WGS84 +no_defs + ellps=WGS84 + towgs94=0,0,0")
plot(raster_high)
plot(bnd, col=NA, border= "red", add=TRUE)

library(rgdal)
library(leaflet)
leaflet() %>%
addProviderTiles(providers$CartoDB.Positron) %>%
addPolygons(data=bnd, weight = 3, color = "red" , fill = NA) %>%
addRasterImage(raster_high,
               colors = colorNumeric(c("blue","green", "yellow", "red"),
               values(raster_high), na.color = "transparent"), opacity = 0.4)

dir.create("07_map")
save(raster_high, file="./07_map/07_kde_high.rdata")
rm(list=ls()) #작업 메모리  삭제

#search() - 설치된 패키지 확인
#detach(package:XXXXX) - 패키지 삭제

