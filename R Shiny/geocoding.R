setwd('C:/workspace/r_shiny')
load("./04_preprocess/04_preprocess.rdata")
#1단계 중복주소 제거
apt_juso <- data.frame(apt_price$juso_jibun)
apt_juso <- data.frame(apt_juso[!duplicated(apt_juso), ])
head(apt_juso, 2)

#2단계 주소를 좌표로 변환
#2-1 지오코딩 준비
add_list <- list()
cnt <- 0
kakao_key = '3359387e37c678364171afcef4993b15'
install.packages('httr')
install.packages('RJSONIO')
library(httr)
library(RJSONIO)
library(data.table)
library(dplyr)

#2-2 반복문으로 위도경도값 구하기
for(i in 1:nrow(apt_juso)){ 
  
  tryCatch(
    {
     lon_lat <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
                     query = list(query = apt_juso[i,]),
                     add_headers(Authorization = paste0("KakaoAK ", kakao_key)))
    
      coordxy <- lon_lat %>% content(as = 'text') %>% RJSONIO::fromJSON()
      
      cnt = cnt + 1
    
      add_list[[cnt]] <- data.table(apt_juso = apt_juso[i,], 
                                    coord_x = coordxy$documents[[1]]$x, 
                                    coord_y = coordxy$documents[[1]]$y)
      #---# 진행상황 알림 메시지
      message <- paste0("[", i,"/",nrow(apt_juso),"] 번째 (", 
                        round(i/nrow(apt_juso)*100,2)," %) [", apt_juso[i,] ,"] 지오코딩 중입니다: 
       X= ", add_list[[cnt]]$coord_x, " / Y= ", add_list[[cnt]]$coord_y)
      cat(message, "\n\n")
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
}

juso_geocoding <- rbindlist(add_list)
juso_geocoding$coord_x <- as.numeric(juso_geocoding$coord_x)
juso_geocoding$coord_y <- as.numeric(juso_geocoding$coord_y)
juso_geocoding <- na.omit(juso_geocoding) #결측치제거

dir.create('./05_geocoding')
save(juso_geocoding, file="./05_geocoding/05_juso_geocoding.rdata")
write.csv(juso_geocoding,"./05_geocoding/05_juso_geocoding.csv")






 