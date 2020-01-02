# chap08_Hypothesis_basic


# 가설(Hypothesis) : 어떤 사건을 설명하기 위한 가정 
# 검정(Test) : 검정통계량(표본)으로 가설 채택 or 기각 과정 
# 추정 : 표본을 통해서 모집단을 확률적을로 추측 
# 신뢰구간 : 모수를 표함하는 구간(채택역), 벗어나면(기각역) 
# 유의수준 : 알파 표시, 오차 범위의 기준 
# 구간추정 : 신뢰구간과 검정통계량을 비교해서 가설 기각 유무 결정  

##################
# 1. 가설과 검정 
##################

# 귀무가설(H0) : 중2학년 남학생 평균키는 165.2cm와 차이가 없다.
# 대립가설(H1) : 중2학년 남학생 평균키는 165.2cm와 차이가 있다.

# 1. 모집단에서 표본 추출(1,000 학생)
x <- rnorm(1000, mean = 165.2 , sd = 1) # 정규분포 

hist(x)

# 정규성 검정 
# 귀무가설 : 정규분포와 차이가 없다.(0)
shapiro.test(x)
# W = 0.99908, p-value = 0.9101
# W : 검정통계량 -> p-value : 유의확률 
# p-value = 0.9101 >= 알파(0.05) -> 채택 


# 2. 평균차이 검정 : 평균 : 165.2cm
t.test(x, mu=165.2)
# t = -0.55503, df = 999, p-value = 0.579
# t, df : 검정통계량 
# p-value : 유의확률 
# p-value = 0.579 >= 알파(0.05) -> 귀무가설 채택 
# 95 percent confidence interval: 95%
# 165.1238 ~ 165.2426 : 신뢰구간(채택역)  
# mean of x 
# 165.1832  -> 실제 평균(통계량 : 표본의 통계) 
mean(x) # 165.1832

# [해설] 검정통계량이 신뢰구간에 포함되므로 모수의 평균키
# 165.2cm와 차이가 없다고 볼 수 있다.


# 3. 기각역의 평균검정 
t.test(x, mu=165.09, conf.level = 0.95) # 95%
# 귀무가설 : 평균키 165.09cm 차이가 없다.(x)
# 대립가설 : 평균키 165.09cm 차이가 있다.(0)
# t = 3.0767, df = 999, p-value = 0.00215
# p-value = 0.00215 < 알파(0.05) -> 기각 
# 95 percent confidence interval:
#  165.1238 ~ 165.2426 : 통계량 기각역에 해당  


# 4. 신뢰수준 변경(95% -> 99%) 
t.test(x, mu=165.2, conf.level = 0.99) # 99%
# t = -0.55503, df = 999, p-value = 0.579 >= 0.05
# 99 percent confidence interval: % 99%
# 165.1050 ~ 165.2614
# 신뢰수준 향상 -> 신뢰구간 넓어짐(가설 기각 어렵다.)


########################
# 2. 표준화 vs 정규화 
########################

# 1. 표준화 : 정규분포 -> 표준정규분포(0, 1) 

# 정규분포 
n <- 2000
x <- rnorm(n, mean = 100, sd = 10)

shapiro.test(x)
# p-value = 0.1871 > 유의수준(알파) : 0.05

# 표준화 
# 표준화 공식(z) = (x - mu) / sd(x)
mu <- mean(x) # 99.65749
z = (x - mu) / sd(x)
z # 표준정규분포 
hist(z)
mean(z) # 6.142044e-16 = 6 * 10-16 = 0
sd(z) # 1

# scale() 함수 이용 
z2 = as.data.frame(scale(x)) # matrix -> data.frame
str(z2)
z2 <- z2$V1
hist(z2)
mean(z2) # 6.142044e-16 = 6 = 0
sd(z2) # 1


# 2. 정규화
# - 특정 변수값을 일정한 범위(0~1)로 일치시키는 과정 
str(iris)
head(iris)
summary(iris[-5])

# 1) scale() 함수 
# 정규화 -> data.frame 형변환 
iris_nor <- as.data.frame(scale(iris[-5]))
summary(iris_nor)

# 2) 정규화 함수 정의(0~1)
nor <- function(x){
  n <- (x - min(x)) / (max(x) - min(x))
  return(n)
}

iris_nor2 <- apply(iris[-5], 2, nor)
summary(iris_nor2)
head(iris_nor2)


###########################
## 3. 데이터셋에서 샘플링 
###########################

#sample(x, size, replace = FALSE) # 비복원추출 

no <- 1:100 # 번호 
score <- runif(100, min=40, max=100)# 성적  

df <- data.frame(no, score) 
df
nrow(df) # 100

# 15명 학생 샘플링 
idx <- sample(x = nrow(df), size = 15)
idx

sam <- df[idx, ]
sam


# train(70%)/test(30%) dataset 
idx <- sample(x = nrow(df), size = nrow(df)*0.7)
idx

train <- df[idx, ] # model 학습용 
test <- df[-idx, ] # model 평가용 
dim(train) # 70  2
dim(test) # 30  2

##########################
## train/test model 적용 
##########################

# 50% vs 50%
idx <- sample(nrow(iris), nrow(iris)*0.5)

train <- iris[idx, ]
test <- iris[-idx, ]
dim(train) # 75  5
dim(test) # 75  5

head(iris)
# Sepal.Length : y(종속변수) -> 정답 
# Petal.Length : x(독립변수) -> 입력 
# model : 꽃잎 길이 -> 꽃받침 길이 


# train dataset model 
model <- lm(Sepal.Length ~ Petal.Length, data=train)
pred <- model$fitted.values  # 예측치(꽃받침 길이) 

# test dataset model 
model2 <- lm(Sepal.Length ~ Petal.Length, data=test)
pred2 <- model2$fitted.values # 예측치(꽃받침 길이)


# train_x
train_x <- train$Petal.Length # train x  

# test_x
test_x <- test$Petal.Length # test x 

# 정답 vs 예측치
plot(train_x, pred, col="blue", pch=18) # train
points(test_x, pred2, col="red", pch=19) # test

# 범례
legend("topleft", legend = c("train", "test"),
       col = c("blue", "red"), pch = c(18, 19) )






