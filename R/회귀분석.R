install.packages("lubridate")
install.packages("ggplot")
install.packages("ggpmisc")

load("./08_chart/all.rdata")
load("./08_chart/sel.rdata")
library(dplyr)
library(lubridate)
#1단계 월별 평당 거래가 요약
all <- all %>% group_by(month=floor_date(ymd, "month")) %>%
  summarise(all_py = mean(py))
sel <- sel %>% group_by(month=floor_date(ymd, "month")) %>%
  summarise(sel_py = mean(py))
#2회귀식 모델링
fit_all <- lm(all$all_py ~ all$month)
fit_sel <- lm(sel$sel_py ~ sel$month)
coef_all <- round(summary(fit_all)$coefficients[2],1) * 365
coef_sel <- round(summary(fit_sel)$coefficients[2],1) * 365

library(grid)
grob_1 <- grobTree(textGrob(paste0('전체지역:', coef_all, "만원(평당)"),
                            x= 0.05, y=0.88, hjust=0, 
                            gp=gpar (col="blue", fontsize=13, fontface ="italic")))#전체 지역
grob_2 <- grobTree(textGrob(paste0('관심지역:', coef_sel, "만원(평당)"),
                            x= 0.05, y=0.95, hjust=0, 
                            gp=gpar (col="red", fontsize=13, fontface ="bold")))#관심지역
#회귀선 그리기
#install.packages("ggpmisc")
library(ggpmisc)
gg <- ggplot(sel, aes(x=month, y=sel_py ))+
  geom_line() + xlab("월")+ ylab("가격") +
  theme(axis.text.x = element_text(angle = 90))+
  stat_smooth(method = 'lm', colour = "dark grey", linetype= "dashed") + 
  theme_bw()

#서을 전체회귀선
gg + geom_line(color = "red", size= 1.5)+
  geom_line(data=all, aes(x=month, y=all_py), color ="blue", size = 1.5) +
  annotation_custom(grob_1) +
  annotation_custom(grob_2)    

rm(list=ls())
