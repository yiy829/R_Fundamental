# chap10_Ttest_Anova

# 1-1. 단일집단 비율차이 검정
# - 비모수 검정

# 1) data 가져오기
getwd()
setwd("c:/Rwork/data")
data <- read.csv("one_sample.csv")
head(data)

# 2) 빈도/통계량 
x <- data$survey # 만족도 변수
summary(x)
table(x)
# 0(불만)  1(만족)
#   14      136   -> 150

# 3) 가설검정

binom.test(14, 150, p=0.2) 
# binom.test(성공횟수, 시행횟수, p=확률) 기존 불만율 20% => 0.2
# p-value = 0.0006735 < 0.05 : 기각
# H1 채택 : 기존 불만율과 2015년도 불만율에 차이가 있다.
?binom.test
binom.test(14, 150, p=0.2, alternative = "two.sided",
           conf.level = 0.95) # 결과는 같다 양측검정이 기본
# p-value = 0.0006735


# 대립가설 채택 : 방향성이 있는 가설
# 2015(14%) > 2014(20%)  - 기각 (귀무가설과 반대)
binom.test(14, 150, p=0.2, alternative = "greater", 
           conf.level = 0.95) 
# p-value = 0.9999 > 0.05

# 2015(14%) < 2014(20%)  - 채택
binom.test(14, 150, p=0.2, alternative = "less", 
           conf.level = 0.95) 
# p-value = 0.0003179 < 0.05


# 1-2. 단일집단 평균차이 검정

# 1) data 가져오기
data <- read.csv("one_sample.csv")
head(data)
time <- data$time

# 2) 전처리
mean(time, na.rm=T)

x <- na.omit(time)
length(x) # 109

# 3) 전제조건 : 정규성검정

shapiro.test(x)
# p-value = 0.7242 >= 0.05 : 정규분포

# 4) 단일집단 평균차이 검정
t.test(x, mu = 5.2)
# t = 3.9461, df = 108, p-value = 0.0001417 < 0.05
# 기각 -> 평균차이가 있다.
?t.test
# t : -1.96 ~ 1.96(95%) : 채택역(범위내에 있으면) 
# -> 현재는 기각역

# 대립가설 : 단측검정(방향성)
# A회사 > 국내
t.test(x, mu = 5.2, alternative = "greater")
# p-value = 7.083e-05 < 0.05 (채택)

# A회사 < 국내
t.test(x, mu = 5.2, alternative = "less")
# p-value = 0.9999 > 0.05 (기각)


# 2-1. 두 집단 비율차이 검정

# 1) data 가져오기
getwd()
setwd("c:/Rwork/data")
data <- read.csv("two_sample.csv")
data
head(data) # 변수명 확인

# 2) 두 집단 subset 작성
data$method # 1, 2 -> 노이즈 없음
data$survey # 1(만족), 0(불만족)
# 데이터 정체/전처리
x<- data$method # 교육방법(1, 2) -> 노이즈 없음
y<- data$survey # 만족도(1: 만족, 0:불만족)
table(x)
# 1   2 
# 150 150 

table(y)
# 0   1 
# 55 245 

table(x, y)
#     0   1
# 1  40 110 -> 집단1(ppt)
# 2  15 135 -> 집단2(실습)

# 3) 두 집단 비율차이검증 - prop.test()
help(prop.test) # prop.test(x,n,p, alternative, conf.level, correct)

# 양측검정
prop.test(c(110,135),c(150,150)) # 방법A 만족도와 방법B 만족도 차이 검정
# p-value = 0.0003422 < 0.05 : 차이가 있다
#sample estimates: 집단 간 비율
#   prop 1    prop 2
# 0.7333333 0.9000000

prop.test(c(110,135),c(150,150), alternative="two.sided", conf.level=0.95)
# 해설) p-value = 0.0003422 - 두 집단간의 만족도에 차이가 있다.

# 단측검정 (방법1 > 방법2)
prop.test(c(110,135),c(150,150), alternative="greater", conf.level=0.95)
# p-value = 0.9998 > 0.05

# 단측검정 (방법1 < 방법2)
prop.test(c(110,135),c(150,150), alternative="less", conf.level=0.95)
# p-value = 0.0001711 < 0.05 (채택)

# 결론 : 교육방법2의 만족도가 방법1보다 높다


# 2-2. 두 집단 평균검정

# 1) data 가져오기
data <- read.csv("c:/Rwork/data/two_sample.csv", header=T)
data
print(data)
head(data) #4개 변수 확인
summary(data) # score - NA's : 73개

# 2) 두 집단 subset 작성(데이터 정제,전처리)
result <- subset(data, !is.na(score), c(method, score))
# c(method, score) : data의 전체 변수 중 두 변수만 추출
# !is.na(score) : na가 아닌 것만 추출
# 위에서 정제된 데이터를 대상으로 subset 생성
result # 방법1과 방법2 혼합됨
length(result$score) # 227

# data 분리

# (1) 교육방법 별로 분리
a <- subset(result, method == 1)
b <- subset(result, method == 2)

# (2) 교육방법에서 점수 추출
a1 <- a$score # 방법1의 실기점수
b1 <- b$score # 방법2의 실기점수

# 기술통계량 -> 평균값 적용 -> 정규성 검정 필요
length(a1) # 109
length(b1) # 118

# 3) 분포모양 검정 : 두 집단의 분포모양 일치 여부 검정
# 귀무가설 : 두 집단 간 분포의 모양이 동질적이다.
# 두 집단간 동질성 비교(분포모양 분석)
var.test(a1, b1) # p-value = 0.3002 > 0.05 -> 차이가 없다.
# 동질성 분포 : t.test()
# 비동질성 분포 : wilcox.test()


# 4) 가설검정 – 두 집단 평균 차이검정
t.test(a1, b1)
t.test(a1, b1, alter="two.sided", conf.int=TRUE, conf.level=0.95)
# p-value = 0.0411 < 0.05 : 두 집단간 평균에 차이가 있다.

# 대립가설 : a1 > b1
t.test(a1, b1, alter="greater", conf.int=TRUE, conf.level=0.95)
# p-value = 0.9794 > 0.05 : 기각 a1이 b1보다 크지 않다.

# 대립가설 : a1 < b1
t.test(a1, b1, alter="less", conf.int=TRUE, conf.level=0.95)
# p-value = 0.02055 < 0.05 : 채택 a1이 b1보다 작다.

mean(a1); mean(b1) #  5.556881 < 5.80339


# 3. 분산분석
# 두 집단 이상 평균차이 검정(집단 분산 차이 검정)
# 일원배치분산분석 : 독립변수(X), 종속변수(Y)
# cf) 이원배치분산분석 : y ~ x1 + x2

# aov(y ~ x, data = dataset)

# 독립변수 : 집단변수(범주형)
# 종속변수 : 연속형변수(비율, 등간 척도)
# ex) 쇼핑몰 고객의 연령대(20, 30, 40, 50)별 구매금액(연속형)
# 독립변수 : 연령대, 종속변수 : 구매금액

# 귀무가설 : 집단별 평균(분산)의 차이가 없다.
# 대립가설 : 적어도 한 집단에 평균 차이가 있다


####################
### iris dataset
####################

# 귀무가설 : 꽃의 종별로 꽃받침의 너비 차이가 없다
# 대립가설 : 적어도 한 집단에 너비 차이가 있다.

# 1. 변수 선택
str(iris)
x <- iris$Species # 집단변수
y <- iris$Sepal.Width # 연속형

# 2. data 전처리

# 3. 동질성 검정
bartlett.test(Sepal.Width ~ Species, data = iris) 
# bartlett.test(y ~ x) 이미 지정했기 때문에 data = iris 생략가능
# p-value = 0.3515 > 0.05 -> 동질성확인 -> aov 

# 4. 분산분석 : aov(y ~ x, data)
model <- aov(Sepal.Width ~ Species, data = iris)
model


# 5. 분산분석 해석
summary(model)
# p-value = 2e-16 < 0.05 
# 결론 : 세 집단중 적어도 한 집단 이상에 평균 
# 차이가 높게(***) 보인다.

# 6. 사후검정 : 각 집단 차이 상세히 분석
# (분산에 얼마나 차이를 보이는지)
TukeyHSD(model)

#                   diff(차이) lwr(하한가)
# versicolor-setosa    -0.658 -0.81885528 가장 많은 평균의 차이를 보이는 집단
# virginica-setosa     -0.454 -0.61485528 
# virginica-versicolor  0.204  0.04314472 
# p-value : 집단 간 평균차이 유무 해설
# diff : 평균차이 정도
# p adj(p-value)가 모두 0.05 이하로 세집단 전부 유의미한 수준에서 차이를 보인다


plot(TukeyHSD(model)) 
# 신뢰구간 : 집단 간 평균차이 유무 해설
# 신뢰구간이 0에 걸쳐서 나뉘어져 있으면 평균의 차이가 없다는 뜻


# 통계검정 : 각 집단의 평균차이
library(dplyr) # dataset %>% function
iris %>% group_by(Species) %>% summarise(avg=mean(Sepal.Width))
#   Species      avg
# 1 setosa      3.43
# 2 versicolor  2.77
# 3 virginica   2.97

# versicolor - setosa
2.77 - 3.43 # -0.66 : diff

# virginica - versicolor
2.97 - 2.77 # 0.2


####################
### 비모수 검정
####################

names(iris)
x <- iris$Species
y <- iris$Sepal.Length

# 2. 동질성 검정
bartlett.test(Sepal.Length ~ Species, data = iris) 
# p-value = 0.0003345  < 0.05 정규분포가 아니다(비모수 검정)

# 3. 분산분석(비모수 검정) : 평균 -> 중위수
model2 <- kruskal.test(Sepal.Length ~ Species, data = iris)

# Kruskal-Wallis chi-squared = 96.937, df = 2, p-value < 2.2e-16 : 각각의 집단별로 중위수의 차이가 있다

# 4. 사후검정 : 집단별 중위수 비교
library(dplyr)
iris %>% group_by(Species) %>% summarise(med=median(Sepal.Length))
# Species        med
# <fct>         <dbl>
# 1 setosa       5  
# 2 versicolor   5.9
# 3 virginica    6.5


####################
### quakes
####################

# 1. 전처리
str(quakes)
# 'data.frame':	1000 obs. of  5 variables:
# $ lat(위도)   : num  -20.4 -20.6 -26 -18 -20.4 ...
# $ long(경도)  : num  182 181 184 182 182 ...
# $ depth(깊이) : int  562 650 42 626 649 195 82 194 211 622 ...
# $ mag(규모)   : num  4.8 4.2 5.4 4.1 4 4 4.8 4.4 4.7 4.3 ...
# $ stations(관측소) : int  41 15 43 19 11 12 43 15 35 19 ...


range(quakes$depth) # 40 680
680 - 40
div <- round(640/3)
div # 213

# 코딩변경(연속형 -> 범주형)
quakes$depth2[quakes$depth <= (40 + div)] <- "low"
quakes$depth2[quakes$depth > (40 + div) & quakes$depth <= (680 - div) ] <- "mid"
quakes$depth2[quakes$depth > (680 - div)] <- "high"

y <- quakes$mag # 연속형
x <- quakes$depth2 # 집단변수(연속형 -> 범주형)
table(quakes$depth2)
# high  low  mid 
# 366  508  126 

# 2. 동질성 검정
bartlett.test(mag ~ depth2, data = quakes)
p-value = 0.1554 > 0.05 # 모수 검정

# 3. 분산분석(모수 검정)
q <- aov(mag ~ depth2, data = quakes)
summary(q)

# 5.78e-14 < 0.05 : 깊이 따른 지진 규모에 적어도 한집단에서 평균에 차이가 있다

# 4. 사후검정
TukeyHSD(q)

#               diff        lwr      p adj
# low-high  0.17127705  0.1083477  0.0000000 : 평균 차이
# mid-high -0.07543586 -0.1702399  0.1486744 > 0.05 : 평균에 차이가 없다
# mid-low  -0.24671291 -0.3380606  0.0000000 : 평균 차이

plot(TukeyHSD(q))
