# chap13_2_RandomForest

# 패키지 설치
install.packages("randomForest")
library(randomForest)

names(iris)


# 1. model
model <- randomForest(Species ~ ., data=iris)
# 500 dataset 생성 -> 500 tree(model) -> 모델의 예측치 제공

model
# Number of trees: 500 -> 트리 갯수
# No. of variables tried at each split: 2 -> 노드분류 사용된 변수 갯수

# OOB estimate of error rate : 4.67%
# 분류 정확도 95%
# Confusion matrix:
#             setosa versicolor virginica class.error
# setosa         50          0         0        0.00
# versicolor      0         47         3        0.06
# virginica       0          4        46        0.08

(50+47+46) / 150 # 0.95333
?randomForest
model2 <- randomForest(Species ~., data = iris, ntree = 400, mtry = 2, importance = T, na.action = na.omit)
# mtry 트리를 분해하는 x변수 갯수, importance 중요변수 대입
# 범주형 : classification model + confusion matrix 생성 // 연속형 : mse model + 회귀(분류) tree 생성

model2

importance(model2)
# MeanDecreaseGini(gini 갯수) : 노드 불순도(불확실성) 개선에 기여하는 변수 -> 높을수록 y에 중요한 역할

varImpPlot(model2)


##############################
## 분류트리(y변수 : 연속형)
##############################
library(MASS)
data("Boston")

str(Boston)

# y = medv
# x = 13칼럼

# 1. bootstrap의 수 m은 어느 정도 커야할까?
# 답: m이 100이상이면 충분하지만 검정오차가 안정화될 만큼 큰 값을 사용(예:400개 이상)

# 2. 랜덤포리스트의 설명변수 갯수 a는 얼마가 적당한가?
# 답: 회귀트리 : 1/3p, 분류트리 : p의 제곱근=sqrt(전체변수 개수)
# 변수 간 상관성에 따라 최적의 a값이 다를 수 있다

p = 14
(1/3) * p # 4.6666

mtry = round((1/3) * p) # 반올림
mtry = floor((1/3) * p)
mtry # 4

model3 <- randomForest(medv ~ ., data = Boston, ntree = 500, mtry = mtry, importance = T, na.action = na.omit)
model3
# 연속형 Mean of squared residuals: 9.897165 => error과 같은 의미 (0에 가까울수록 예측치와 실제값 차이가 없다)


# 중요변수 시각화
varImpPlot(model3)
