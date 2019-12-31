# chap06_DataVisualization

# 1. 이산변수(discrete quantitative data) 시각화
# - 정수단위로 나누어 측정할 수 있는 변수(자녀수, 판매수)

# 차트 데이터 생성
chart_data <- c(305,450, 320, 460, 330, 480, 380, 520) 
names(chart_data) <- c("2016 1분기","2017 1분기","2016 2분기","2017 2분기","2016 3분기","2017 3분기","2016 4분기","2017 4분기")
str(chart_data)
chart_data

# 1) 막대차트 시각화
# 세로막대차트
barplot(chart_data, ylim = c(0,600), col = rainbow(8), 
        main = "2016년도 vs 2017년도 분기별 매출현황 비교")
?barplot

# 가로막대차트
barplot(chart_data, xlim = c(0,600), col = rainbow(8), 
        horiz = T, xlab = "매출액(단위:만원)", ylab = "년도별 분기현황", 
        main = "2016년도 vs 2017년도 분기별 매출현황 비교")

# 1행2열 구조
par(mfrow=c(1,2)) # 1행 2열 그래프 보기

# 개별 막대차트
barplot(VADeaths, beside=T,col=rainbow(5),
        main="미국 버지니아주 하위계층 사망비율")

# 누적형 막대차트
barplot(VADeaths, beside=F,col=rainbow(5),
        main="미국 버지니아주 하위계층 사망비율")

# 2) 점 차트 시각화
par(mfrow=c(1,1)) # 1행1열
dotchart(chart_data, color = c("green", "red"),
         labels = names(chart_data),
         xlab = "매출액",
         main = "분기별 판매현황"
         )

# 3) 파이 차트 시각화
pie(chart_data, labels = names(chart_data),
    col = rainbow(8), cex = 1.2)
    
#  제목 추가
title("2016 vs 2017 판매현황")



# 2. 연속변수 (Continueous quantitative)
# - 시간, 길이 등과 같이 연속성을 가진 실수 단위 변수값

# 1) 상자 그래프 시각화
VADeaths
str(VADeaths)
# num [1:5, 1:4] -> matrix 자료구조
summary(VADeaths)

boxplot(VADeaths, range=0) # 요약통계치 시각화

# 2) 히스토그램 시각화 : 대칭성 확인
iris
head(iris)

mean(iris$Sepal.Length) # 5.843333
range(iris$Sepal.Length) # 최솟값(4.3), 최댓값(7.9)

hist(iris$Sepal.Length, xlab = "Sepal.Length", 
     col = "magenta",
     main = "iris 꽃받침 길이", xlim = c(4.3, 7.9)
     )

mean(iris$Sepal.Width) # 3.057333
range(iris$Sepal.Width) # 최솟값(2.0), 최댓값(4.4)

hist(iris$Sepal.Width, xlab = "Sepal.Width", 
     col = "green",
     main = "iris 꽃받침 넓이", xlim = c(2.0, 4.4)
)


# 정규분포 가설 : 정규분포와 차이가 없다.
# 정규성 검정
shapiro.test(iris$Sepal.Length) 
# p-value = 0.01018 < 알파 = 0.05 : 정규성에 어긋난다. 기각

shapiro.test(iris$Sepal.Width) 
# p-value = 0.1012 > 알파 = 0.05 :정규성에 부합한다. 채택

# 이후에 모수(정규성), 비모수(정규성 x)검정으로 넘어간다.


# 3) 산점도 시각화
price <- runif(10, min=1, max=100)
price

plot(price) # y, x : index

par(mfrow=c(2,2))
plot(price, type = "o") # circle
plot(price, type = "l") # line
plot(price, type = "h") # height
plot(price, type = "s") # step


# 만능차트
par(mfrow=c(1,1))
data()

# 시계열 데이터 시각화
AirPassengers
plot(AirPassengers)

plot(WWWusage)

# 회귀모델 -> 회귀모델 시각화
install.packages("HistData")
library(HistData)

library(help="HistData")

data(Galton)
str(Galton)
# 'data.frame':	928 obs. of  2 variables:
# $ parent
# $ child

model <- lm(child ~ parent, data = Galton) # lm(y ~ x)
model # Intercept(절편), parent(x에 대한 기울기)

# 회귀모델 관련 시각화
par(mfrow=c(2,2))
plot(model)

methods(plot)


# 3. 변수간의 비교 시각화
?pairs
pairs(iris[1:4])

# 꽃의 종별 변수 비교
unique(iris$Species) # setosa versicolor virginica 

pairs(iris[iris$Species=="setosa",1:4]) # 종별 산점도
pairs(iris[iris$Species=="versicolor",1:4])
pairs(iris[iris$Species=="virginica",1:4])


# 4. 차트 파일 저장
setwd("c:/Rwork/data/output")
jpeg("iris.jpg", width = 720, height = 480)
plot(iris$Sepal.Length, iris$Petal.Length)
title("iris datatset 시각화")
dev.off()



#########################
### 3차원 산점도 
#########################
install.packages('scatterplot3d')
library(scatterplot3d)

# 꽃의 종류별 분류 
iris_setosa = iris[iris$Species == 'setosa',]
iris_versicolor = iris[iris$Species == 'versicolor',]
iris_virginica = iris[iris$Species == 'virginica',]

# scatterplot3d(밑변, 오른쪽변, 왼쪽변, type='n') # type='n' : 기본 산점도 제외 
d3 <- scatterplot3d(iris$Petal.Length, iris$Sepal.Length, iris$Sepal.Width, type='n')

d3$points3d(iris_setosa$Petal.Length, iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width, bg='orange', pch=21)

d3$points3d(iris_versicolor$Petal.Length, iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width, bg='blue', pch=23)

d3$points3d(iris_virginica$Petal.Length, iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width, bg='green', pch=25)

