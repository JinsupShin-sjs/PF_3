loc <-read.csv("./sigun_code/sigun_code.csv", fileEncoding = "UTF-8")
getwd() 
setwd('c:/Workplace/R_shiny')
#1단계 수집대상지역 설정
loc$code <- as.character(loc$code)
head(loc, 2)
#2단계 수집기간 설정
datelist <-seq(from = as.Date('2021-01-01'),
               to = as.Date('2021-12-31'),
               by = '1 month')
datelist <- format(datelist, format='%Y%m')
datelist[1:3]
#3단계 인증키 입력
service_key <- "hnaRJ7qT0t07ZQXEfv%2FVTEB7xCRtCt08GPlRb3B5QhtHVeZAGy%2FFH0kbV3DC6VDmcM0LBrMBm%2FWryreuxeVcOg%3D%3D"
#4단계 요청목옥 생성
#4-1 요청목옥 만들기
url_list <- list() #빈 스트 만들기
cnt <-0 #반복문 제어변수 초기값 설정
#4-2 요청목록 채우기
for( i in 1:nrow(loc)){ #구
  for(j in 1:length(datelist)){ #날짜별
    cnt <- cnt +1
    url_list[cnt] <-paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?",
                           "LAWD_CD", loc[i, 1],
                           "&DEAL_YMD=", datelist[j],
                           "&num0fRows=",100,
                           "&serviceKey=", service_key)
} 
Sys.sleep(0.1)
msg <- paste0("[",i,"/", nrow(loc), "] ", loc[i,3],"의 크롤링 목록이 생성됨 => 총[", cnt,"] 건")
cat(msg, "\n\n")
}
length(url_list)
browseURL(paste0(url_list[1]))

#5단계 크롤링 실행
#install.packages("XML")
#install.packages("data.table")
#install.packages("stringer")

library(XML)
library(data.table)
library(stringer)

raw_data <-list()
root_Node <-list()
total <-list()
dir.create("02_raw_data")

# 5-1 단계 url 요청 xml 응답
for(i in 1:length(url_list)){
  raw_data[[i]] <- xmlTreeParse((url_list[i], useInternalNodes = T, encoding = "utf-8")
  root_Node[[i]] <- xmlRoot(raw_data[[i]])       
#5-2 단계 전체 거래 건수를 확인
  items < - root_Node[[i]][[2]][['items']]
  #전체거래 건수 확인
  size < - xmlSize(itens)
  #5-3 단계 거래 내역 추출
item <- list()
item_temp_dt <- data.table()
Sys.sleep(.1)
  for(m in 1:size) {
    #세부 거래 내역 분리
    item_temp <-xmlSApply(items[[m]], xmlValue)
    item_temp_dt <- data.table(year = item_temp[4],
                               month = item_temp[7],
                               day = item_temp[8],
                               price = item_temp[1],
                               code = item_temp[12],
                               dong_nm = item_temp[5],
                               jibun = item_temp[11],
                               con_year = item_temp[3],
                               apt_nm = item_temp[6],
                               area = item_temp[9],
                               floor = item_temp[13])
    item[[m]] <- item_temp_dt}
apt_bind <- rbindlist(item)
region_nm <- subset(loc, code == str_sub(url_list[i], 115, 119))$addr_1 #지역명
month <- str_sub(url_list[i], 130,135) #연월
path <- as.character(paste0("./02_raw_data/", region_nm, "-", month, ".csv"))
write.csv(apt_bind, path) #csv 저장
msg <- paste0("[", i, "/", length(url_list),
              "] 수집한 데이터를 [", path,"]에 저장합니다.") #알림메시지
cat (msg, "\n\n")
}

files <- dir("./02_raw_data/")
#install.packages("plyr")
library(plyr)
apt_price <- ldply(as.list(paste0("./02_raw_data/", files)), read.csv, fileEncoding="UTF-8")
tail(apt_price, 2)


dir.create("./03_integrated")
save(apt_price, file = "./03_integrated/03_apt_price.rdata")
write.csv(apt_price, "./03_integrated/03_apt_price.csv") 
