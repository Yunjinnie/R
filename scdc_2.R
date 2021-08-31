getwd()
setwd('C:/R-3.6.3')
#install.packages('readxl')
library(readxl)
trend=read.csv('Track2_1_trend_w_demo.csv', header=TRUE)
head(trend)
job=read_excel('Track2_2_업종_예시.xlsx')
head(job)
sum(is.na(trend))
library(dplyr)
library(data.table)

names(trend)[3] <- c("gender")
names(trend)[4] <- c("age")
names(trend)[5] <- c("marriage")
names(trend)[6] <- c("infant")
names(trend)[7] <- c("elementary")
names(trend)[8] <- c("mid_high")
names(trend)[9] <- c("univ")
names(trend)[10] <- c("housewife")
summary(trend)

#View(trend)
library(ggplot2)
ggplot(trend, aes(x=YM))+geom_bar()
ggplot(trend, aes(x=Category))+geom_bar()
ggplot(trend, aes(x=gender))+geom_bar()
ggplot(trend, aes(x=age))+geom_bar()
ggplot(trend, aes(x=marriage))+geom_bar()
ggplot(trend, aes(x=infant))+geom_bar()
ggplot(trend, aes(x=elementary))+geom_bar()
ggplot(trend, aes(x=mid_high))+geom_bar()
ggplot(trend, aes(x=univ))+geom_bar()
ggplot(trend, aes(x=housewife))+geom_bar()

ggplot(trend)+geom_bar(aes(age))+facet_wrap(~YM, ncol=1) 
# 2019년에 비해 2020년에는 모든 연령층에서 소비량이 감소
ggplot(trend)+geom_bar(aes(Category))+facet_wrap(~YM, ncol=1) 
# 2020년에 항공여행, 할인점, 디저트, 뷰티, 취미, 호텔숙박, 면세점은 소비 감소, 
# 오픈마켓, 전문몰은 소비 증가
# 종합몰은 소비 비슷하지만 전반적으로 소비 감소 경향

#plot(trend$Category, trend$infant, xlab='category', ylab='infant')
#plot(trend$Category, trend$elementary, xlab='category', ylab='elementary')
#plot(trend$Category, trend$mid_high, xlab='category', ylab='mid_high')
#plot(trend$Category, trend$univ, xlab='category', ylab='univ')
#plot(trend$Category, trend$marriage, xlab='category', ylab='marriage')

mosaicplot(table(trend$Category, trend$age))
mosaicplot(table(trend$Category, trend$marriage)) # 기혼자들은 디저트, 오픈마켓, 전문몰, 할인점의 소비가 높음
mosaicplot(table(trend$Category, trend$housewife)) # 주부가 아닌 사람들은 디저트, 오픈마켓, 전문몰, 할인점의 소비가 높음
mosaicplot(table(trend$Category, trend$univ)) # 대학생 자녀가 없는 사람들은 위 분야의 소비가 높음
mosaicplot(table(trend$Category, trend$mid_high)) # 중고생 자녀가 없는 사람들은 위 분야의 소비가 높음
mosaicplot(table(trend$Category, trend$elementary)) # 골고루
mosaicplot(table(trend$Category, trend$infant)) # 초등생 자녀가 없는 사람들은 위 분야의 소비가 높음
mosaicplot(table(trend$YM, trend$marriage))

mosaicplot(table(trend$age, trend$marriage)) # C-D-E-(A,F,G,H,B)
mosaicplot(table(trend$age, trend$housewife)) # A,D>C,F>B,G,H>E
mosaicplot(table(trend$age, trend$infant)) # B,G,H-F-A,E-C,D
mosaicplot(table(trend$age, trend$elementary)) # E,H-A,D-F-C-G-B
mosaicplot(table(trend$age, trend$mid_high)) # C,E,H-B-D,G-A,F
mosaicplot(table(trend$age, trend$univ)) # C,D,E-A,H-F-B-G
mosaicplot(table(trend$gender, trend$housewife)) # low의 비율이 압도적으로 높은 0은 남자, 1은 여자
# C,D,E는 갓 결혼한 신혼부부, 중고/대학생 자녀의 비율이 적고 유아, 초등 자녀의 비율이 높음 20대~30대 초반으로 추정
# A,F,H는 유아, 초등, 중고, 대학생 자녀의 비율이 골고루 높은 30대 중반~ 40대 중반으로 추정
# B,G는 유아 자녀의 비율이 낮고 대학생/초중고 학생 자녀의 비율이 높은 것으로 보아 40대 후반~50대 이상으로 추정 

trend0=trend[trend$gender=='0',]
trend1=trend[trend$gender=='1',]
# man은 housewife일 확률이 없음

trend2=trend[trend$age=='C'|trend$age=='D'|trend$age=='E',]
trend3=trend[trend$age=='A'|trend$age=='F'|trend$age=='H',]
trend4=trend[trend$age=='B'|trend$age=='G',]
ggplot(trend2,aes(age, marriage), na.omit=T)+geom_bar(stat='identity')+facet_wrap(~marriage)


trend0=trend[trend$gender=='0',]
trend1=trend[trend$gender=='1',]

trend5=trend[trend$YM=='201904'|trend$YM=='202004',]
trend6=trend[trend$YM=='201905'|trend$YM=='202005',]
ggplot(trend5)+geom_bar(aes(age))+facet_wrap(~YM, ncol=1) 
ggplot(trend6)+geom_bar(aes(age))+facet_wrap(~YM, ncol=1) 

trend$marriage = ifelse(trend$marriage=='high', 0.8,
                        ifelse(trend$marriage=='mid',0.5,0.2))
trend$infant = ifelse(trend$infant=='high', 0.8,
                        ifelse(trend$infant=='mid',0.5,0.2))
trend$elementary = ifelse(trend$elementary=='high', 0.8,
                        ifelse(trend$elementary=='mid',0.5,0.2))
trend$mid_high = ifelse(trend$mid_high=='high', 0.8,
                        ifelse(trend$mid_high=='mid',0.5,0.2))
trend$univ = ifelse(trend$univ=='high', 0.8,
                        ifelse(trend$univ=='mid',0.5,0.2))
trend$housewife = ifelse(trend$housewife=='high', 0.8,
                        ifelse(trend$housewife=='mid',0.5,0.2))

trend2=trend[trend$YM=='202004'|trend$YM=='202005',]

ggplot(trend, aes(x=marriage))+geom_histogram()
library(ggcorrplot)
corr<-cor(trend[,5:10])
ggcorrplot(corr)

age_m=ggplot(trend,aes(x=marriage,y=infant))+geom_point()+geom_smooth(color="red")
age_m2=age_m+scale_x_continuous(name="marriage")+scale_y_continuous(name="infant")
age_m2

xnum <- trend2[5:10]
trend_k=kmeans(xnum, centers=3)
'''
dx<-round(dist(xnum), digits=2)
D2<-dist(xnum, method="manhattan")
str(trend_k)
trend_k$cluster
table(trend_k$cluster)
hc1<-hclust(dist(xnum), method="single")
hc2<-hclust(dist(xnum), method="complete")
hc3<-hclust(dist(xnum), method="average")

clust<-data.frame(trend, kmeans=factor(trend_k$cluster),
                  +                   single=factor(cutree(hc1, k=3)),
                  +                   complete=factor(cutree(hc2, k=3)),
                  +                   average=factor(cutree(hc1, k=3)))
ggplot(clust, aes(age, trestbps))+geom_point(aes(color=single),
                                             +size=7, alpha=0.2)
ggplot(clust, aes(age, trestbps))+geom_point(aes(color=complete),
                                             +size=7, alpha=0.2)
ggplot(clust, aes(age, trestbps))+geom_point(aes(color=average),
                                             +size=7, alpha=0.2)
ggplot(clust, aes(age, trestbps))+geom_point(aes(color=kmeans),
                                             +size=7, alpha=0.2)
'''

tt<-trend[trend$Category=='호텔/숙박',]
tt[trend$age=='C'|trend$age=='D'|trend$age=='E',ngroup:='2030']
tt[연령대=="A"|연령대=="F"|연령대=="H",ngroup:="3040"]
tt[연령대=="B"|연령대=="H",ngroup:="5060"]
#ggplot(tt,aes(ngroup,기혼스코어),na.omit=T)+geom_bar(stat="identity")+facet_wrap(~기혼스코어)

#install.packages('gmodels')
library(gmodels)
# H0 : 두 변수가 독립, H1 : 두 변수 독립 x
CrossTable(trend$YM, trend$housewife, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
# 독립
CrossTable(trend$YM, trend$gender, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
# 독립
CrossTable(trend$Category, trend$YM, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
# 독립 x
CrossTable(trend$Category, trend$gender, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
# 독립 x
CrossTable(trend$Category, trend$housewife, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
# 독립 x
CrossTable(trend$Category, trend$gender, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
# 독립 x
CrossTable(trend$marriage, trend$housewife, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
# 독립 x
CrossTable(trend$age, trend$housewife, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
CrossTable(trend$age, trend$gender, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
CrossTable(trend$age, trend$Category, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
CrossTable(trend$age, trend$elementary, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
CrossTable(trend$age, trend$univ, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
CrossTable(trend$age, trend$mid_high, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
CrossTable(trend$age, trend$marriage, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)
CrossTable(trend$age, trend$YM, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE,
           chisq=TRUE, expected = TRUE)

# mh=matrix(trend$marriage, trend$housewife)
# chi_1<-chisq.test(mh)

fit1<-manova(cbind(marriage, infant, elementary, mid_high, univ, housewife)~factor(Category), data=trend1)
summary(fit1, test=c("Hotelling"))
fit2<-manova(cbind(marriage, infant, elementary, mid_high, univ)~factor(Category), data=trend0)
summary(fit2, test=c("Hotelling"))
# Category 변수들이 유의미
fit3<-manova(cbind(Category, marriage, infant, elementary, mid_high, univ, housewife)~factor(age), data=trend2)
summary(fit3, test=c("Hotelling"))

x1.anova<-aov(marriage~age, data=trend2)
summary(x1.anova)
# 역시 유의미
# TukeyHSD(x1.anova)
x2.anova<-aov(infant~age, data=trend2)
summary(x2.anova)
x3.anova<-aov(YM~age, data=trend2)
summary(x3.anova)
x4.anova<-aov(elementary~age, data=trend2)
summary(x4.anova)
x5.anova<-aov(mid_high~age, data=trend2)
summary(x5.anova)
x6.anova<-aov(univ~age, data=trend2)
summary(x6.anova)
x7.anova<-aov(housewife~age, data=trend2)
summary(x7.anova)

xbar1<-matrix(colMeans(trend[trend$age=="A", 5:10]))
S1<-var(trend[trend$age=="A", 5:10])
xbar2<-matrix(colMeans(trend[trend$age=="B", 5:10]))
S2<-var(trend[trend$age=="B", 5:10])
xbar3<-matrix(colMeans(trend[trend$age=="C", 5:10]))
S3<-var(trend[trend$age=="C", 5:10])
xbar4<-matrix(colMeans(trend[trend$age=="D", 5:10]))
S4<-var(trend[trend$age=="D", 5:10])
xbar5<-matrix(colMeans(trend[trend$age=="E", 5:10]))
S5<-var(trend[trend$age=="E", 5:10])
xbar6<-matrix(colMeans(trend[trend$age=="F", 5:10]))
S6<-var(trend[trend$age=="F", 5:10])
xbar7<-matrix(colMeans(trend[trend$age=="G", 5:10]))
S7<-var(trend[trend$age=="G", 5:10])
xbar8<-matrix(colMeans(trend[trend$age=="H", 5:10]))
S8<-var(trend[trend$age=="H", 5:10])

p<-nrow(S2)
n1<-sum(data$manufacturer=="General Mills")
n2<-sum(data$manufacturer=="Kelloggs")
n3<-sum(data$manufacturer=="Quacker")
Sp1<-((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
Sp1
T2_1<-t(xbar1-xbar2)%*%solve((1/n1+1/n2)*Sp1)%*%(xbar1-xbar2)
T2_1
(n1+n2-2)*p/(n1+n2-p-1)*qf(0.95, p, n1+n2-p-1)
T2_1>(n1+n2-2)*p/(n1+n2-p-1)*qf(0.95, p, n1+n2-p-1)
# reject H0
library(ICSNP)
HotellingsT2(data[data$manufacturer=="General Mills", 3:13], data[data$manufacturer=="Kelloggs", 3:13])

Sp2<-((n1-1)*S1+(n3-1)*S3)/(n1+n3-2)
T2_2<-t(xbar1-xbar3)%*%solve((1/n1+1/n3)*Sp2)%*%(xbar1-xbar3)
T2_2
(n1+n3-2)*p/(n1+n3-p-1)*qf(0.95, p, n1+n3-p-1)
T2_2>(n1+n3-2)*p/(n1+n3-p-1)*qf(0.95, p, n1+n3-p-1)
# cannot reject H0

ld<-lda(age~marriage+infant+elementary+mid_high+univ+housewife, data=trend)

pc<-predict(ld)$class
head(pc)
table(trend$age, pc)
correct<-mean(trend$age==pc)
error<-mean(trend$age!=pc)
correct
error
trend$pred<-pc
trend$miss<-trend$age!=pc
head(trend)
#ggplot(data,aes(calories, protein))+geom_point(data=data[data$miss,],col='red', size=4)+
#geom_point(aes(color=factor(manufacturer)))
LD<-predict(ld)$x
ld
lda.data<-data.frame(LD, orig=trend$age, pred=pc,
                     miss=trend$age!=pc)
head(lda.data)
ggplot(lda.data, aes(LD1, LD2))+geom_point(aes(color=orig))
ggplot(lda.data, aes(LD1, LD2))+
  geom_point(data=lda.data[lda.data$miss,],color='red', size=4)+
  geom_point(aes(color=orig))


set.seed(1)
dim(trend)
table(trend$age)
test.data<-c(sample(1:13, 4), sample(14:26, 4), sample(27:39, 4), sample(40:52, 4), sample(53:65, 4), sample(66:78, 4))
test.data
train<-trend[-test.data,]
train
test<-trend[test.data,]
dim(train)
table(train$age)
dim(test)
table(test$age)
data.ld<-lda(age~marriage+infant+elementary+mid_high+univ+housewife, data=trend)
data.ld
train.pc<-predict(data.ld, train)$class
test.pc<-predict(data.ld, test)$class
test.pc
train.pc
table(train$age, train.pc)
train.miss<-mean(train.pc!=train$age)
train.miss
table(test$age, test.pc)
test.miss<-mean(test.pc!=test$age)
test.miss

LD.train<-predict(data.ld, train)$x
LD.test<-predict(data.ld, test)$x
LD<-rbind(LD.train, LD.test)
pc<-c(as.character(train.pc), as.character(test.pc))
Manu<-c(as.character(train$age), as.character(test$age))
plot.data<-data.frame(LD, Manu, pc, miss= Manu!=pc)
ggplot(plot.data, aes(LD1, LD2))+
  geom_point(data=plot.data[plot.data$miss,], color='red', size=4)+
  geom_point(aes(color=Manu))

set.seed(2020)                                                                     
table(tren$age)
dim(tren)
test.data<-c(sample(1:26000, 5200), sample(26001:52000, 5200), sample(52001:78000, 5200), sample(78001:104000, 5200), sample(104001:130000, 5200), sample(130001:156000, 5200), sample(156001:182000, 5200), sample(182001:210468, 5700))
test.data
train.data<-tren[-test.data,]
test.data<-tren[test.data,]
dim(train.data)
table(train.data$age)
dim(test.data)
table(test.data$age)
heart.ld<-lda(age~., data=train.data)
heart.ld
train.pc<-predict(heart.ld,train.data)$class 
test.pc<-predict(heart.ld,test.data)$class 
test.pc
train.miss<-mean(train.pc!=train.data$age)
train.miss
table(test.data$age, test.pc)
test.pc
test.miss<-mean(test.pc!=test.data$age)
test.miss
LD.train<-predict(heart.ld, train.data)$x
LD.test<-predict(heart.ld, test.data)$x
head(LD.train)
head(LD.test)
LD<-rbind(LD.train, LD.test)
pc<-c(as.character(train.pc),as.character(test.pc)) 
Target<-c(as.character(train.data$age), 
          as.character(test.data$age)) 
plot.data<-data.frame(LD,Target,pc,miss=Target!=pc)
ggplot(plot.data, aes(LD1))+geom_density(aes(color=factor(tren$age)))

install.packages('factoextra')
library(factoextra)
df=scale(trend2[,5:10])
fviz_cluster(trend_k, data = df)
fviz_nbclust(df, kmeans, method = "wss") # Elbow Method

library(glmnet)
logit1=glm(YM~marriage+infant+elementary+mid_high+univ+housewife, data=trend)
# logit1$fitted.values
summary(logit1)

logit2=glm(gender~marriage+infant+elementary+mid_high+univ, data=trend)
summary(logit2)

#logit3=glm(age~marriage+infant+elementary+mid_high+univ+housewife, data=trend)

install.packages('stargazer')
library(stargazer)
stargazer(logit1, type='text', out='logit.htm')
invlog=function(x){
  1/(1+exp(-x))
}
invlog(coef(logit1)[2])

allmean<-cbind(trend, predict(logit1, newdata=trend, type='response', se.fit=TRUE))
allmean$ll=allmean$fit-1.96*allmean$se.fit
allmean$ul=allmean$fit+1.96*allmean$se.fit
allmean

ggplot(allmean, aes(x=Category, y=fit))+
  geom_errorbar(aes(ymin=ll, ymax=ul), width=0.2, lty=1, lwd=1, col='red')+
  #geom_point(shape=18, size=5, fill='black')+
  scale_x_discrete(limits=c('호텔/숙박', '할인점', '항공/여행', '오픈마켓/소셜', '디저트', '전문몰', '뷰티', '면세점', '취미'))

#library(MASS)
#m1=polr(infant~YM+gender, data=trend, Hess=TRUE)
#summary(m1)
#pred1=predict()

#install.packages('ca')
#library(ca)
#trend2=trend%>%
  #select(-c(YM, gender))
#ca1=ca(trend2)

#install.packages('FactoMineR')
#library(FactoMineR)
#ca2=CA(trend2, graph=FALSE)

#library(MASS)
#ca5=corresp(trend, nf=5)
