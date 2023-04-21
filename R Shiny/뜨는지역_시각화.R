setwd('c:/Workplace/R_shiny')  
load("./06_geodataframe/06_apt_price.rdata") 
grid <- st_read("./06_geodataframe/seoul.shp")
apt_price <- st_join(apt_price, grid, join=st_intersects)
head(apt_price,2)

#1단계 이전  / 이후 데이터 세트 만들기
kde_befor <- subset(apt_price, ymd <"2021-07-01")
kde_befor <-aggregate(kde_befor$py, by=list(kde_befor$ID),mean) 
colnames(kde_befor) <- c("ID", "before")

kde_after <- subset(apt_price, ymd > "2021-07-01")
kde_after <-aggregate(kde_after$py, by=list(kde_after$ID),mean) 
colnames(kde_after) <- c("ID", "after")

kde_diff <- merge(kde_befor, kde_after, by="ID")
kde_diff$diff <- round((((kde_diff$after-kde_diff$before)/
                           kde_diff$before)*100),0)
head(kde_diff,2)
library(sf)
kde_diff <-kde_diff[kde_diff$diff > 0,] 
kde_hot <- merge(grid, kde_diff, by="ID")

library(ggplot2)
library(dplyr)
kde_hot %>% 
  ggplot(aes(fill=diff)) + geom_sf() + scale_fill_gradient(low="white", high = "red")

library(sp)
kde_hot_sp <- as(st_geometry(kde_hot), "Spatial") 
x <- coordinates(kde_hot_sp)[,1]
y <- coordinates(kde_hot_sp)[,2] 

l1 <- bbox(kde_hot_sp) [1,1] - (bbox(kde_hot_sp) [1,1] * 0.0001)
l2 <- bbox(kde_hot_sp) [1,2] + (bbox(kde_hot_sp) [1,2] * 0.0001)
l3 <- bbox(kde_hot_sp) [2,1] - (bbox(kde_hot_sp) [2,1] * 0.0001)
l4 <- bbox(kde_hot_sp) [2,2] + (bbox(kde_hot_sp) [1,1] * 0.0001)

library(spatstat)

win <- owin(xrange = c(l1,l2), yrange = c(l3,l4))
plot(win)

p <- ppp(x, y, window = win, mark =kde_hot$diff)
d <- density.ppp(p, weights=  kde_hot$diff, 
                 sigma = bw.diggle(p),
                 kernel = 'gaussian')
plot(d)

#3-3 레스터 변환

d[d < quantile(d)[4] + (quantile(d)[4] *0.1)] <- NA
library(raster)
raster_hot <- raster(d)
plot(raster_hot)

#3-4 클리핑
bnd <- st_read("./06_geodataframe/seoul.shp")
raster_hot <- crop(raster_hot, extent(bnd))
crs(raster_hot) <- sp::CRS("+proj=longlat + datum=WGS84 +no_defs + ellps=WGS84 + towgs94=0,0,0")
plot(raster_hot)
plot(bnd, col=NA, border= "red", add=TRUE)


library(rgdal)
library(leaflet)
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=bnd, weight = 3, color = "red" , fill = NA) %>%
  addRasterImage(raster_hot,
                 colors = colorNumeric(c("blue","green", "yellow", "red"),
                 values(raster_hot), na.color = "transparent"), opacity = 0.4)
save(raster_hot, file="./07_map/07_kde_hot.rdata")
rm(list=ls()) 