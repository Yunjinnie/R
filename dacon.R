library(ggplot2)
library(readxl)
#install.packages("readr")
library(readr)
library(ggcorrplot)

getwd()
setwd("C:/python_data/input")
setwd("C:/R-3.6.3") # 끌 때 여기로 변경

#index=read.csv("./index.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
index_csv=read_csv("./index.csv")
index=as.data.frame(index_csv)
fpopl=read.csv("./fpopl.csv")
delivery_csv=read_csv("./delivery.csv") #
delivery=as.data.frame(delivery_csv)
table=read_excel("./COVID_eng_kor_table.xlsx")
TimeAge=read.csv("./CovidTimeAge.csv")
TimeGender=read.csv("./CovidTimeGender.csv")
TimeProvince_csv=read_csv("./CovidTimeProvince.csv") #
TimeProvince=as.data.frame(TimeProvince_csv)
card_csv=read_csv("./card.csv") #
card=as.data.frame(card_csv)
Case_csv=read_csv("./CovidCase.csv") #
Case=as.data.frame(Case_csv)
PatientInfo_csv=read_csv("./CovidPatientInfo.csv") #
PatientInfo=as.data.frame(PatientInfo_csv)
Policy=read.csv("./CovidPolicy.csv")
Region_csv=read_csv("./CovidRegion.csv") #
Region=as.data.frame(Region_csv)
Time=read.csv("./CovidTime.csv")
adstrd_csv=read_csv("./adstrd_master.csv") #
adstrd=as.data.frame(adstrd_csv)
head(index)
# cgi : 2018년 월평균 대비 매출 성장 비율, 100을 기준으로 이상이면 매출 상승, 이하면 하락
head(fpopl) # 행정동별 유동인구
head(delivery)
head(table)
head(TimeAge)
head(TimeGender)
head(TimeProvince)
head(card)
head(Case)
head(PatientInfo)
head(Policy)
head(Region)
head(Time)
head(adstrd)

# ggplot - 범주형 bar, 연속형 histogram
ggplot(index, aes(x=sigungu, y=cgi))+geom_boxplot()
mosaicplot(table(index$catm, index$sigungu))
ggplot(fpopl, aes())+geom_histogram()
ggplot(card, aes(x=adstrd_code, y=selng_cascnt))

# 상관관계 분석
## 포스트 코로나 시대 >>유망 품목 및 산업<< 발굴 - 소비가 늘어난 품목 찾기
## 코로나로 인한 국민의 생활/소비의 변화 파악 - 배달 card 비용, 건수 내역 시기별로 변화

# 1. index 품목별 소비지수 : period - pre, post : 코로나 확진자수 급증 시기/ (age, gender, sido, sigungu)에 따른 cgi 변화
# 2. fpopl 행정동별 유동인구 데이터 : base_ymd, tmzon 따라 adstrd 별 popltn (sex, age 세분화)
# 3. adstrd 행정동 코드 : adstrd 코드에 따른 행정동명, 시도, 시군구
# 4. card 업종별 결제 금액 : date 별로 행정동, 가맹점 업종 frequency / 매출발생금액 & 건수
# 5. delivery 배달 호출 정보 : date 별로 배달 상점 업종 / 위치 - 행정동 코드, 배달 목적지 위치, 배달 비용 및 수수료
# 카드 금액, 배달 완료 시간- 배달 요청 시간에 따른 취소 건수 분석
# 코로나 확진자가 많은 지역(Region->province), 나이, 성별에 따라 
# 위의 데이터들 활용하여 소비 패턴 - 시계열 분석
# 다른 영향도 있는가? policy?

# MANOVA
## 유의한 결과 얻을 수 있는 가설 세워서 검정 - 영향을 줄만한 요소

# 소비지수랑 연관지어서 - index에서 식품은 감소 추세고 delivery는 거의 음식
# index의 식품 종류는 유제품, 빵, 제과류 =/= 배달 음식이랑은 다르게
# 1. index에서는 주류/담배/기호식품 확실히 증가 
## 다른구들도 확인해봐야하나 확실히 확진자수 늘어날때 증가하는 추세가 보임(금천구) 
## 사람들 스트레스성 소비와 연관
# 2. 건강의료 소비지수도 늘어나고 - 코로나 직접적인 영향
# 3. 화장품/ 바디+헤어/ 뷰티 소품은 감소하는 추세 - 외출 감소

# PCA - Comp graph ... 안쓸듯, LDA - 에러값 표시한 graph, 변수별 그룹 graph
## 예측할만한 게 있나?
# 전달 소비 데이터를 바탕으로 예측한 결과와의 에러 값 - 미래 데이터
# Clustering - dendrogram, elbow method 
## 지역별, 나이별, 성별별로 소비 패턴 clustering
## 관악구, 금천구
# 결측치 예측?
# 시계열 ARIMA 분석 - 지금까지, 앞으로의 소비 패턴 예측
## 코로나 확진자 시기별로 많은 확진자가 나온 지역, 나이, 성별을 선택
## 이에 따라 지출액 (delivery, item (높은 소비지수)별 card내역) 높은 것 찾아 예측치 그래프화
## 배달, 식품, 건강의료, 화장품