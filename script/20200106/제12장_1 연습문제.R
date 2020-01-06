#################################
## <제12장 연습문제>
################################# 

# 01. mpg의 엔진크기(displ)가 고속도록주행마일(hwy)에 어떤 영향을 미치는가?    
# <조건1> 단순선형회귀모델 생성 
# <조건2> 회귀선 시각화 
# <조건3> 회귀분석 결과 해석 : 모델 유의성검정, 설명력, x변수 유의성 검정  

# <조건1>
library(ggplot2)
data(mpg)
x <- mpg$displ
y <- mpg$hwy
df <- data.frame(x, y)
model <- lm(y ~ x, data = df)
model

# <조건2>
plot(df$x, df$y)
# text 추가
text(df$x, df$y, labels=df$y, cex = 0.7, pos = 1, col ="blue")
abline(model, col="red")

ggplot(model, aes(displ, hwy)) +
  geom_point() +
  stat_smooth(method = lm, level = 0.95)

# <조건3>
summary(model)
# 모델 유의성 : F-statistic: 329.5 on 1 and 232 DF,  p-value: < 2.2e-16  => 통계적으로 유의하다
# 설명력 : Adjusted R-squared:  0.585 => 설명력이 좋다
# x 변수 유의성 : t = -18.15, <2e-16 *** < 0.05 => x는 y에 영향력을 가지고 있다. (*이 3개. 강한 영향력)



# 02. product 데이터셋을 이용하여 다음과 같은 단계로 다중회귀분석을 수행하시오.
setwd("c:/Rwork/data")
product <- read.csv("product.csv", header=TRUE)

#  단계1 : 학습데이터(train),검정데이터(test)를 7 : 3 비율로 샘플링
idx <- sample(x = nrow(product), size = nrow(product)*0.7, replace = F)
str(idx)

train <- product[idx, ]
test <- product[-idx, ]
dim(train)
dim(test)

#  단계2 : 학습데이터 이용 회귀모델 생성 
#           변수 모델링) y변수 : 제품_만족도, x변수 : 제품_적절성, 제품_친밀도
model <- lm(제품_만족도 ~ ., data = train)

#  단계3 : 검정데이터 이용 모델 예측치 생성 
y_pred <- predict(model, test)
y_true <- test$"제품_만족도"

#  단계4 : 모델 평가 : cor()함수 이용  
mse <- mean((y_true - y_pred)^2)
mse # 0.2546804

cor(y_true, y_pred) # 0.7836135

# 03. ggplot2패키지에서 제공하는 diamonds 데이터 셋을 대상으로 
# carat, table, depth 변수 중 다이아몬드의 가격(price)에 영향을 
# 미치는 관계를 다중회귀 분석을 이용하여 예측하시오.
#조건1) 다이아몬드 가격 결정에 가장 큰 영향을 미치는 변수는?
#조건2) 다중회귀 분석 결과를 정(+)과 부(-) 관계로 해설

library(ggplot2)
data(diamonds)

# diamonds에서 비율척도 대상으로 식 작성 
formula <- price ~ carat +  table + depth
head(diamonds)
model <- lm(formula, data=diamonds) 
summary(model) # 회귀분석 결과

# <해설>carat은 price에 정(+)의 영향을 미치지만, table과 depth는 부(-)의 영향을 미친다.
# 85%의 설명력을 가진다,  p-value: < 2.2e-16  => 통계적으로 유의하다, < 0.05 => x는 y에 영향력을 가지고 있다.