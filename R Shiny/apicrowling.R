loc <- read.csv("sigun_code.csv", fileEncoding="UTF-8")
getwd()
setwd('C:/workspace/r_shiny')
# 1단계 수집 대상 지역 설정
loc$code <- as.character(loc$code)
head(loc, 2)
# 2단계 수집 기간 설정
datelist <- seq(from= as.Date('2021-01-01'),
                to = as.Date('2021-12-31'),
                by = '1 month')
datelist <- format(datelist, format='%Y%m')
datelist[1:3]

# 3단계 인증키 입력
 
service_key <- "huEy2FccpR8mKLnAqpCZ99QdqUA%2FxwQtGysBZIpD3ML4Fc%2FEdqeprh8uNpLrIshd0%2BXkq1M95bMVvNb%2FzMXcDA%3D%3D"
# 4단계 요청목록 생성
# 4-1 요청 목록 만들기
url_list =　list() #빈리스 만들기
cnt <- 0 # 반복문 제어 변수 초기값 설정

#4-2 요청목록 채우기
for (i in 1:nrow(loc)) { # 구별
  for (j in 1:length(datelist)){ #날짜
    cnt <-  cnt + 1
    url_list[cnt] <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?",
                            "LAWD_CD=", loc[i,1],         # 지역코드
                            "&DEAL_YMD=", datelist[j],    # 수집월
                            "&numOfRows=", 100,           # 한번에 가져올 최대 자료 수
                            "&serviceKey=", service_key)  # 인증키
  }
  Sys.sleep(0.1) # 0.1초 멈춤
  msg <- paste0("[", i,"/",nrow(loc), "]  ", loc[i,3], " 의 크롤링 목록이 생성됨 => 총 [", cnt,"] 건")
  cat(msg, "\n\n")
}

length(url_list)
browseURL(paste0(url_list[1]))
 
