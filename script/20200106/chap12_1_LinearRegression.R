# chap12_1_LinearRegression

##########################
# 1. 단순선형회귀분석
##########################
# - 독립변수(1) -> 종속변수(1) 미치는 영향 분석

setwd("c:/Rwork/data")
product <- read.csv("product.csv")
str(product)

# 'data.frame':	264 obs. of  3 variables:
# $ 제품_친밀도: int  3 3 4 2 2 3 4 2 3 4 ...
# $ 제품_적절성: int  4 3 4 2 2 3 4 2 2 2 ...
# $ 제품_만족도: int  3 2 4 2 2 3 4 2 3 3 ...

x <- product$"제품_적절성" # 독립변수
y <- product$"제품_만족도" # 종속변수


df <- data.frame(x,y)
df 

# 2) 회귀모델 생성
model <- lm(y ~ x, data = df)
model
# Coefficients : 회귀계수(기울기, 절편)
# (Intercept)            x  
#0.7789(절편)       0.7393(기울기) 

# 회귀방정식(y) = 0.7393 * x + 0.7789
head(df)
# x=4, y=3
x = 4; Y = 3 # 관측치
y = 0.7393 * x + 0.7789 # 예측치
y # 3.7361

# 오차 = 관측치(정답) - 예측치
err <- Y - y
err # -0.7361
abs(err) # 0.7361
mse = mean(err^2) # 평균제곱 오차
mse # 0.5418432 # 제곱(부호+, 패널티)

names(model) # 12칼럼 제공
# "coefficients" : 계수
# "residuals" : 오차(잔차)
# "fitted.values" : 적합치(예측치)

model$coefficients
model$residuals[1] # -0.735963 
# 실제정답과 예측치의 오차
model$fitted.values[1] # 3.735963


# 3) 회귀모델 분석
summary(model)

# <회귀모델 분석 순서>
# 1. F검정 통계량 : 모델의 유의성 검정(p < 0.05)
# F-statistic:   374 on 1 and 262 DF,  p-value: < 2.2e-16 : 통계적으로 유의하다
# 2. 모델의 설명력 : Adjusted R-squared(0.5865) -> 1에 가까울수록 설명력이 좋다(예측력이 좋다)
# 3. x변수 유의성 검정 : < 2e-16 < 0.05 : x는 y에 영향력을 가지고 있다. (*이 3개. 강한 영향력)
# t 검정통계량 : 19.340 > 1.96(기각역 범위) => 대립가설 채택. x는 y에 영향력을 가지고 있다.


# R-squared = R^2
R <- sqrt(0.5865)
R # 0.7658329 : 높은 상관성


# 4) 회귀선 : 회귀방정식에 의해서 구해진 직선(y의 예측치)

# x, y 산점도
plot(df$x, df$y)
# 회귀선(직선)
abline(model, col = "red")


##########################
# 2. 다중선형회귀분석
##########################
# - 독립변수(n) -> 종속변수(1) 미치는 영향 분석

install.packages("car")
library(car)

Prestige
str(Prestige)
# 102 직업군 대상 : 교육수준, 수입, 여성비율, 평판, 직원수, 유형(x)
# 'data.frame':	102 obs. of  6 variables:
row.names(Prestige) # 102 직업군 확인

# 1) subset
newdata <- Prestige[c(1:4)]
str(newdata)

# 2) 상관분석
cor(newdata)
#             education(x1)     income       women(x2)   prestige(x3)
# education     1.00000000    0.5775802     0.06185286  0.8501769
# income(y)     0.57758023    1.0000000     -0.44105927  0.7149057

# 3) 회귀모델
model <- lm(income ~ education + women + prestige, data = newdata)
model
#(Intercept)    education        women     prestige  
#     -253.8        177.2        -50.9        141.4  

# Y = matmul(X, a) + b -> python
# Y = (X %*% a)(행렬곱) + b -> R

income <- 12351 # Y(정답)
education <- 13.11 # x1
women <- 11.16 # x2
prestige <- 68.8 # x3

head(newdata)

# 예측치 : 회귀방정식
y_pred <- 177.2*education - 50.9*women + 141.4*prestige - 253.8
y_pred # 11229.57

err <- income - y_pred
err # 1121.432

# 4) 회귀모델 분석
summary(model)
# 모델 유의성 : F-statistic: 58.89 on 3 and 98 DF,  p-value: < 2.2e-16
# 모델 설명력 : Adjusted R-squared:  0.6323 (1에 가까울수록 좋다, 63%의 설명력을 가진다)
# x 유의성 검정(T 검정통계량) : 영향력 판단

#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -253.850   1086.157  -0.234    0.816    
# education    177.199    187.632   0.944    0.347(영향 없음)    
# women        -50.896      8.556  -5.948 4.19e-08 ***(- 영향)
# prestige     141.435     29.910   4.729 7.58e-06 ***(+ 영향)
# t value가 +- 1.96 안에 있으면 채택역 : 관계가 없다
# 영향력 없는 변수는 빼는 게 낫다(변수 선택)


##########################
# 3. 변수 선택법
##########################
# - 최적 모델을 위한 x변수 선택

newdata2 <- Prestige[c(1:5)]
dim(newdata2) # 102   5

library(MASS) # 변수선택 함수
model2 <- lm(income ~ ., data = newdata2)

step <- stepAIC(model2, direction = "both") 
step # 최적의 변수 선택 함수

model3 <- lm(formula = income ~ women + prestige, data = newdata2)
summary(model3)

# F-statistic: 87.98 on 2 and 99 DF,  p-value: < 2.2e-16
# Adjusted R-squared:  0.6327
#           Estimate Std.    Error t value  Pr(>|t|)    
# (Intercept)  431.574    807.630   0.534    0.594    
# women        -48.385      8.128  -5.953 4.02e-08 ***
# prestige     165.875     14.988  11.067  < 2e-16 ***

0.6327 - 0.6323
# 4e-04 정도 설명력이 높아졌다 stepwise는 100% 신뢰할 만하진 않지만 어느정도 참고가 가능



##########################
# 4. 기계학습
##########################

iris_data <- iris[-5]
str(iris_data)
# 'data.frame':	150 obs. of  4 variables:


# 1) train / test set(70 : 30)
idx <- sample(x=nrow(iris_data), size=nrow(iris_data)*0.7, replace = FALSE)
idx
train <- iris_data[idx, ]
test <- iris_data[-idx, ]
dim(train) # 105   4(y+x)
dim(test) # 45  4(y+x)

# 2) model(train)
model <- lm(Sepal.Length ~ ., data=train)

# 3) model 평가(test)
y_pred <- predict(model, test) # y 예측치
y_true <- test$Sepal.Length # y 정답

# 평가 : mse(평균제곱오차), cor(상관계수)
mse <- mean((y_true - y_pred)^2)
mse # 0.08643196

cor(y_true, y_pred) # 0.9111003 높은 상관계수


# y real value vs y prediction
plot(y_true, col = "blue", type = "o", pch = 18)
points(y_pred, col = "red", type = "o", pch = 19)
title("real value vs prediction")
# 범례
legend("topleft", legend = c("real", "pred"), col = c("blue", "red"), pch = c(18, 19))



##########################################
##  5. 선형회귀분석 잔차검정과 모형진단
##########################################

# 1. 변수 모델링  y ~ x
# 2. 회귀모델 생성 
# 3. 모형의 잔차검정 
#   1) 잔차의 등분산성 검정
#   2) 잔차의 정규성 검정 
#   3) 잔차의 독립성(자기상관) 검정 
# 4. 다중공선성 검사 
# 5. 회귀모델 생성/ 평가 


names(iris)

# 1. 변수 모델링 : y:Sepal.Length <- x:Sepal.Width,Petal.Length,Petal.Width
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width


# 2. 회귀모델 생성 
model <- lm(formula = formula,  data=iris)
model
names(model)


# 3. 모형의 잔차검정
par(mfrow=c(2,2))
plot(model)
#Hit <Return> to see next plot: 잔차 vs 적합값 -> 패턴없이 무작위 분포(포물선 분포 좋지않은 적합) 
#Hit <Return> to see next plot: Normal Q-Q -> 정규분포 : 대각선이면 잔차의 정규성 
#Hit <Return> to see next plot: 척도 vs 위치 -> 중심을 기준으로 고루 분포 
#Hit <Return> to see next plot: 잔차 vs 지렛대값 -> 중심을 기준으로 고루 분포 

# (1) 등분산성 검정 
par(mfrow=c(1,1))
plot(model, which =  1) # 어느정도 등분산성의 형태를 띄고 있다.
methods('plot') # plot()에서 제공되는 객체 보기 

# (2) 잔차 정규성 검정
attributes(model) # coefficients(계수), residuals(잔차), fitted.values(적합값)
res <- residuals(model) # 잔차 추출 
shapiro.test(res) # 정규성 검정 - p-value = 0.9349 >= 0.05
# 귀무가설 : 정규성과 차이가 없다.

# 정규성 시각화  
hist(res, freq = F) 
qqnorm(res)

# (4) 잔차의 독립성(자기상관 검정 : Durbin-Watson) 
install.packages('lmtest')
library(lmtest) # 자기상관 진단 패키지 설치 
dwtest(model) # 더빈 왓슨 값
# DW = 1.8767, p-value = 0.2511 >= 0.05 : 변수간 독립이다.

# 4. 다중공선성 검사 
# - 독립변수 간의 강한 상관관계로 인해서 발생하는 문제
# - ex) 생년월일, 생일

library(car)
sqrt(vif(model)) > 2 
# Sepal.Width Petal.Length  Petal.Width 
# FALSE         TRUE         TRUE 
# => TRUE x변수 중에 petal.Length, Width를 같이 쓰게되면 다중공선성이 생길 수 있으므로 둘 중 하나만 써라 

# 5. 모델 생성/평가 
formula = Sepal.Length ~ Sepal.Width + Petal.Length 
cor(iris[-5])

model <- lm(formula = formula,  data=iris)
summary(model) # 모델 평가
# Adjusted R-squared:  0.838 예측력(설명력)이 84%이다

