getwd()
setwd("C:/R-3.6.3")
library(readxl)
library(ggplot2)

coef= as.data.frame(read_excel('coef.xls', col_names=F))
coef[] <- +(coef >= 0.5) # binary 변환

#install.packages("matlab")
library(matlab)

#class(coef)
coef_mat=as.matrix(coef)
#typeof(coef_mat)
class(coef_mat)
imagesc(x=seq(ncol(coef_mat)), y=seq(nrow(coef_mat)), coef_mat, col=jet.colors(12))

coef= as.data.frame(read_excel('coef.xls', col_names=F))
coef[] <- +(coef >= 0.7) # binary 변환
coef_mat=as.matrix(coef)
imagesc(x=seq(ncol(coef_mat)), y=seq(nrow(coef_mat)), coef_mat, col=jet.colors(12))

coef2= as.data.frame(read_excel('coef.xls', col_names=F))
'''
for ((i,j) in coef2){
  if coef2[i]<0:
    coef2[i] <- -coef2[i]
  coef2[] <- +(coef2 >=0.7)
}
'''
for (i in coef2){
  coef2[i]
}
coef2[1,2]

coef2 <- ifelse (coef2<0 , 0, 1)
coef2_mat=as.matrix(coef2)
imagesc(x=seq(ncol(coef2_mat)), y=seq(nrow(coef2_mat)), coef2_mat, col=jet.colors(12))

#coef2 <- ifelse (coef2<0 , -coef2[], coef2[])

cc0= as.data.frame(read_excel('base_cc.xls',col_names=c('cc1'))) # header=FALSE 필요 없음
bc0= as.data.frame(read_excel('base_bc.xls',col_names=c('bc1')))
cc1= as.data.frame(read_excel('1year_cc.xls',col_names=c('cc2')))
bc1= as.data.frame(read_excel('1year_bc.xls',col_names=c('bc2')))
cc2= as.data.frame(read_excel('2year_cc.xls',col_names=c('cc3')))
bc2= as.data.frame(read_excel('2year_bc.xls',col_names=c('bc3')))

fit1<-manova(cbind()~factor(), data=)
summary(fit1, test=c("Hotelling"))