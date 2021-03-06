# chap11_Correlation

# dataset 가져오기
setwd("c:/Rwork/data")
product <- read.csv("Product.csv")

# 친밀도, 적절성, 만족도 : 5점(등간척도)
str(product)

# 1. 상관분석
# - 두 변수의 확률분포의 상관관계 정도를 나타내는 계수(-1 ~ +1)

corr <- cor(product, method = "pearson") # 상관계수 행렬
corr
cor(x = product$제품_적절성, y = product$제품_만족도)
# 0.7668527


# 2. 상관분석 시각화
install.packages("corrplot")
library(corrplot)

corrplot(corr, method = "number")
corrplot(corr, method = "square")
corrplot(corr, method = "circle")

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(product)


#  3. 공분산
# - 두 변수의 확률분포의 상관관계 정도를 나타내는 값
cov(product)
#               제품_친밀도 제품_적절성 제품_만족도
# 제품_친밀도   0.9415687   0.4164218   0.3756625
# 제품_적절성   0.4164218   0.7390108   0.5463331
# 제품_만족도   0.3756625   0.5463331   0.6868159


# 상관계수 vs 공분산
# 상관계수 : 크기, 방향(-, +)
# 공분산 : 크기


####################
### iris 적용
####################

cor(iris[-5])

# 양의 상관계수(0.9628654)
plot(iris$Petal.Length, iris$Petal.Width)
abline(lm(Petal.Width ~ Petal.Length, data = iris), col = "red", lty = 5)

# 음의 상관계수(-0.4284401)
plot(iris$Sepal.Width, iris$Petal.Length)
abline(lm(Petal.Length ~ Sepal.Width, data = iris), col = "red", lty = 5)
