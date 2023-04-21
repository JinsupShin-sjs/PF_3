load("./08_chart/sel.rdata")
pca_01 <- aggregate(list(sel$con_year, sel$floor, sel$py, sel$area),
                    by=list(sel$apt_nm), mean)
colnames(pca_01) <- c("apt_nm", "신축", "층수", "가격", "면적")
m <- prcomp(~ 신축 + 층수 + 가격 + 면적, data=pca_01, scale=T)
summary (m)

 
#그래프 그리기
#install.packages("ggfortify")
library(ggfortify) 

autoplot(m, loadings.label=T, loadings.label.size=6) +
  geom_label(aes(label=pca_01$apt_nm), size=4)

 
