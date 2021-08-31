install.packages("ggplot2")
library("ggplot2")
library("dplyr")
library("tidyverse")
#install.packages("hms")
getwd()
setwd('C:/R-3.6.3')
data = read.csv("./heart.csv")
#View(data)
names(data)[1] <- c("age")
head(data)
dim(data)
str(data)
table(is.na(data))
summary(data)

''
names(data)
attributes(data)
nrow(data)
ncol(data)
length(data)
''


ggplot(data=data, aes(x='', y=age))+geom_boxplot()
heart<-data %>%
  mutate(sex=if_else(sex==1, "Male", "Female"),
         cp=if_else(cp==1, "angina",
                    if_else(cp==2, "non", "asymptomatic")),
         fbs=if_else(fbs==1, "up", "down"),
         exang=if_else(exang==1, "yes", "no"),
         restecg=if_else(restecg==0, "normal",
                         if_else(restecg==1, "lv1", "lv2")),
         slope=as.factor(slope),
         ca=as.factor(ca),
         thal=as.factor(thal),
         target=if_else(target==1, "yes", "no")
  ) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(sex, cp, fbs, exang, restecg, slope, ca, thal, target, everything())

summary(heart)
boxplot(heart[, 10:14])

''
table(data$age)
table(data$sex)
table(data$cp)
table(data$trestbps)
table(data$chol)
table(data$fbs)
table(data$restecg)
table(data$thalach)
table(data$exang)
table(data$oldpeak)
plot(data$oldpeak)
table(data$slope)
table(data$ca)
table(data$thal)
''

ggplot(heart, aes(x=age))+geom_histogram(binwidth = 1)
ggplot(heart, aes(x=trestbps))+geom_histogram(binwidth = 0.1)
ggplot(heart, aes(x=chol))+geom_histogram(binwidth = 5)
ggplot(heart, aes(x=thalach))+geom_histogram(binwidth = 0.1)
ggplot(heart, aes(x=oldpeak))+geom_histogram(binwidth = 0.1)
ggplot(heart, aes(x=sex))+geom_bar()
ggplot(heart, aes(x=cp))+geom_bar()
ggplot(heart, aes(x=fbs))+geom_bar()
ggplot(heart, aes(x=restecg))+geom_bar()
ggplot(heart, aes(x=exang))+geom_bar()
ggplot(heart, aes(x=slope))+geom_bar()
ggplot(heart, aes(x=ca))+geom_bar()
ggplot(heart, aes(x=thal))+geom_bar()
ggplot(heart, aes(x=target))+geom_bar()
View(heart)
#target : dependent variable
# 0 : no heart disease, 1 : diseased
# all other variables : indep
# target 기준으로 파악

# ggplot 상관관계
install.packages("ggcorrplot")
corr<-cor(heart[,10:14])
corr
library(ggcorrplot)
ggcorrplot(corr)

age_p=ggplot(data,aes(x=age,y=target))+geom_point()+geom_smooth(color="red")
age_p2=age_p+scale_x_continuous(name="age")+scale_y_continuous(name="target")
age_p2
#30~60 : decreasing, 60~70 : increasing

barplot(table(data$sex,data$target),
        col=c("red","blue"),
        beside=TRUE,
        xlab="target",
        ylab="count")
#proportion of diseased : female is higher

sex_p=ggplot(data,aes(x=age,y=target))+geom_point(aes(color=sex))+geom_smooth(color="red")
sex_p2=sex_p+scale_x_continuous(name="age")+scale_y_continuous(name="target")
sex_p2
#female : 45-55 - diseased, 55-65 - not diseased
#male : under 55 - not diseased

chest_p=ggplot(data,aes(x=age,y=target))+geom_point(aes(color=cp))+geom_smooth(color="red")
chest_p2=chest_p+scale_x_continuous(name="age")+scale_y_continuous(name="target")
chest_p2
# cp 2 : likely to be diseased

rbp_p=ggplot(data,aes(x=trestbps,y=target))+geom_point(aes(color=trestbps))+geom_smooth(color="red")
rbp_p2=rbp_p+scale_x_continuous(name="Rest Blood Pressure")+scale_y_continuous(name="target")
rbp_p2
#90 -120 are more likely to get diseased, decreasing after RBP 150

chol_p=ggplot(data,aes(x=chol,y=target))+geom_smooth()+
  scale_x_continuous(name="cholestrol")+scale_y_continuous(name="target")
chol_p
# after 300, increasing
ggplot(data,aes(x=age,y=thalach))+geom_point()+geom_smooth()+
  scale_x_continuous(name="age")+
  scale_y_continuous(name="Maximum Heart Rate")
# age에 따라 heart rate decreasing

max_p = ggplot(data,aes(x=thalach,y=target))+geom_point()+geom_smooth()+
  scale_x_continuous(name="Maximum Heart Rate")+scale_y_continuous(name="target")
max_p
#increasing heart rate probability of diseased is increasing

ang_p = ggplot(data,aes(x=exang,y=target))+geom_point()+geom_smooth(color="red")+
  scale_x_continuous(name="Exercise Induced Angina")+scale_y_continuous(name="target")
ang_p
# exang increase, diseased : decrease

peak_p = ggplot(data,aes(x=oldpeak,y=target))+geom_point()+geom_smooth(color="red")+
  scale_x_continuous(name="oldPeak")+scale_y_continuous(name="target")
peak_p
# increasing oldpeak, probability of heart attack decreasing

sl_p = ggplot(data,aes(x=slope,y=target))+geom_point()+geom_smooth(color="red")+
  scale_x_continuous(name="slope")+scale_y_continuous(name="target")
sl_p
# after 1 of slope, increase in slope, probability of heart attack increasing

ca_p= ggplot(data,aes(x=ca,y=target))+geom_point()+geom_smooth(color="red")+
  scale_x_continuous(name="ca")+scale_y_continuous(name="target")
ca_p
# initially, heart attack is decreasing, local minimum at 2, after increasing
# 2차 함수 형태

# fbs & RECG : 유의미한 결과 x

# PCA
pca <- princomp(heart[,10:14], cor = TRUE)
pca
summary(pca)
pca$loadings
pca$sdev^2
screeplot(pca, type="lines")
biplot(pca)

library(GGally)
ggpairs(data.frame(pca$scores), columns=1:4)

#LDA
set.seed(2020)                                                                     
table(heart$target)
test.heart<-c(sample(1:101, 20), sample(102:202, 20), sample(203:303, 20))
test.heart
train.heart<-heart[-test.heart,]
test.heart<-heart[test.heart,]
dim(train.heart)
table(train.heart$target)
dim(test.heart)
table(test.heart$target)
heart.ld<-lda(target~., data=train.heart)
heart.ld
train.pc<-predict(heart.ld,train.heart)$class 
test.pc<-predict(heart.ld,test.heart)$class 
test.pc
train.miss<-mean(train.pc!=train.heart$target)
train.miss
table(test.heart$target, test.pc)
test.pc
test.miss<-mean(test.pc!=test.heart$target)
test.miss
LD.train<-predict(heart.ld, train.heart)$x
LD.test<-predict(heart.ld, test.heart)$x
head(LD.train)
head(LD.test)
LD<-rbind(LD.train, LD.test)
pc<-c(as.character(train.pc),as.character(test.pc)) 
Target<-c(as.character(train.heart$target), 
            +           as.character(test.heart$target)) 
plot.data<-data.frame(LD,Target,pc,miss=Target!=pc)
ggplot(plot.data, aes(LD1))+ 
  +   geom_density(aes(color=factor(heart$target)))
ld.result<-lda(target~age+trestbps+chol+thalach, data=heart)
ld.result
pc<-predict(ld.result, heart)$class
head(pc)
correct<-mean(heart$target==pc)
error<-mean(heart$target!=pc)
correct
error
heart$pred<-pc
heart$miss<-heart$target!=pc
head(heart)
ggplot(heart,aes(age,trestbps))+geom_point(data = heart[heart$miss,],col="red",size=4)+ geom_point(aes(color=factor(target)))
ggplot(heart,aes(age,chol))+geom_point(data = heart[heart$miss,],col="red",size=4)+ geom_point(aes(color=factor(target)))
ggplot(heart,aes(age,thalach))+geom_point(data = heart[heart$miss,],col="red",size=4)+ geom_point(aes(color=factor(target)))
LD<-predict(ld.result, heart)$x
plot.data<-data.frame(heart,LD)
ggplot(plot.data, aes(LD1))+ 
  +   geom_density(aes(color=factor(target)))+
  +   geom_point(data = plot.data[plot.data$miss,],aes(x=LD1, y=0.05, color=factor(target)))

# CCA
# variable too much...

# Hierarchical Cluster
xnum <- heart[10:14] # Numeric variables
xcat <- heart[1:8]
dx<-round(dist(xnum), digits=2)
D2<-dist(xnum, method="manhattan")
hc1<-hclust(dist(xnum), method="single")
plot(hc1, labels=heart$target, hang=-1, main="dendrogram: single")
hc2<-hclust(dist(xnum), method="complete")
plot(hc2, labels=heart$target, hang=-1, main="dendrogram: complete")
hc3<-hclust(dist(xnum), method="average")
plot(hc3, labels=heart$target, hang=-1, main="dendrogram: average")

# K-means (Non-hierarchical)
heart_k=kmeans(xnum, centers=3)
str(heart_k)
heart_k$cluster
table(heart_k$cluster)
clust<-data.frame(heart, kmeans=factor(heart_k$cluster),
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
library(factoextra)
df=scale(heart[,10:14])
fviz_cluster(heart_k, data = df)
fviz_nbclust(df, kmeans, method = "wss") # Elbow Method

# Factor
# 기타 그밖의 analysis