setwd('c:/Workplace/R_shiny')
options(warn=-1)
#1단계 아파트실거래가 불러오기
load("./03_integrated/03_apt_price.rdata")
head(apt_price, 2)
table(is.na(apt_price))
apt_price <- na.omit(apt_price)
#2단계 결측값
table(is.na(apt_price))
head(apt_price$price, 2) 
library(stringr) 
apt_price <- as.data.frame(apply(apt_price,2, str_trim))
head(apt_price$price, 2) 

library(lubridate)
#install.packages("lubridate")
library(dplyr)
#install.packages("dplyr")
apt_price <- apt_price %>% mutate(ymd=make_date(year, month, day))
apt_price$ym <- floor_date(apt_price$ymd, "month")
head(apt_price, 2)

#매매가 확인
head(apt_price$price, 3)
apt_price$price <- apt_price$price %>% sub(",","",.) %>% as.numeric() 
head(apt_price$price, 3)

#3-3 주소 조합
head(apt_price$apt_nm , 30)
apt_price$apt_nm <-gsub("\\(.*","",apt_price$apt_nm)
head(apt_price$apt_nm , 50)
head(apt_price$apt_nm , 30)

loc <- read.csv("sigun_code.csv", fileEncoding = "UTF-8")
apt_price <- merge(apt_price, loc, by ='code')
apt_price$juso_jibun <- paste0(apt_price$addr_2, apt_price$dong,"", 
                               apt_price$jibun,"",apt_price$apt_nm)
head(apt_price, 2)

#건축년도 변환
head(apt_price$con_year,3)
apt_price$con_year <-apt_price$con_year %>% as.numeric()
head(apt_price$con_year,3)

#평당 매매가 만들기
head(apt_price$area,3)
apt_price$area <- apt_price$area %>% as.numeric() %>% round(0)
head(apt_price$area,3)

apt_price$py <-round(((apt_price$price/apt_price$area)*3.3),0) 
head(apt_price$py,3)

#층수 변환 하기
head(apt_price$floor,3)
min(apt_price$floor)
apt_price$floor <- apt_price$floor %>% as.numeric() %>% abs()
min(apt_price$floor)

apt_price$cnt <- 1
head(apt_price, 2)

#4단계 저장 
#4-1필요한 컬럼 만 추출
apt_price <- apt_price %>% select(ymd, ym, year, code, addr_1,apt_nm, juso_jibun, price, con_year, area, floor, py, cnt)
head(apt_price, 2)
dir.create("./04_preprocess")
save(apt_price, file ="./04_preprocess/04_preprocess.rdata")
write.csv(apt_price, "./04_preprocess/04_preprocess.csv")
