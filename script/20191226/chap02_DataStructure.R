# chap02_DataStructure

# 1. vector 자료구조
# - 1차원 배열구조
# - 동일 자료형을 갖는 1차원 배열구조
# - c(), seq(), rep()

# c() 함수
x <- c(1:5)
x
y <- c(1, 3, 5)
y

# seq()
seq(1, 9, by=2) #(start, end, step(증가 or 감소))
seq(9, 1, by=-2)

# rep()
rep(1:3, 3) # 123123123
rep(1:3, each=3) #111222333

# 색인(index) : 자료의 위치(index)
# R index = 1부터 시작
a <- c(1:100)
a # [1] <- index  1 <- content
a[100] # 100
a[50:60]
length(a) # 100
length(a[50:60]) # 11

start = length(a)-5
end = length(a)
a[start:end]
a[(length(a)-5):length(a)] # 95:100

#boolean index : 특정 조건
a[a>=10 & a<=50]


# 2. matrix 자료구조
# - 동일한 자료형을 갖는 2차원 배열
# - 생성 함수 : matrix(), rbind(), cbind()


# matrix()
m1 <- matrix(c(1:9), nrow = 3)
m1
dim(m1) # 행렬구조의 차수 (3, 3)

x1 <- 1:5 #c(1:5) 같은 의미
x2 <- 2:6
x1; x2

# rbind()
m2 <- rbind(x1, x2)
m2
dim(m2)

# cbind()
m3 <- cbind(x1, x2)
m3 # 5*2

# matrix index : obj(row, column)
m3[1, 2] #2
m3[1, ] # 1행 전체
m3[ , 2] # 2열 전체 

# box
m2[c(1:2), c(2:4)]

x <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
x

colnames(x) <- c("one", "two", "three") #변수명 변경
x

# scala(0), vector(1), matrix(2)

# broadcast 연산 : 차원이 작은 쪽이 큰 차원으로 늘어남

# 1) vector vs scala
x <- 1:10
x * 0.5 # vector(10) * scala(10)

# 2) vector vs matrix
x <- 1:3
y <- matrix(1:6, nrow = 2)
dim(y) # 2 3

z <- x * y
z

#apply() : 처리

apply(z, 1, sum) # 20, 26 // col별로 처리
apply(z, 2, mean) # 2.5 6.5 14.0 // row별로 처리
apply(z, 2, max)


# 3. array
# - 동일한 자료형을 갖는 3차원 배열구조
arr <- array(1:12, c(3,2,2))
arr
dim(arr) # 3(row) 2(col) 2(side)

arr[,,1] # 1면
arr[,,2] # 2면

# 4. data.frame
# - 항렬구조를 갖는 자료구조
# - 칼럼단위 서로 상이한 자료형을 갖는다.

no <- 1:3
name <- c("홍길동", "이순신", "유관순")
age <- c(35, 45, 25)
pay <- c(200, 300, 150)

emp <- data.frame(No=no, Name=name, Age=age, Pay=pay)
emp

# obj$column
epay <- emp$Pay # 벡터화
epay

mode(epay)

# 산포도 : 분산과 표준편차
var(epay)

# 양의 제곱근
sqrt(var(epay)) #루트
sd(epay) # 76.37626

score <- c(90, 85, 83)
#분산
var(score) #13
# 표준편차 : 분산의 양의 제곱근
sd(score)
sqrt(var(score))


# 분산 = sum((x- 산술평균)^2) / n-1   (n으로 나누면 모집단에 대한 분산)
avg <- mean(score) # 산술평균
diff <- (score - avg)^2
diff_sum <= sum(diff)
var <- diff_sum / (length(score) - 1)
var # 13

sd <- sqrt(var)
sd


# 5. list 자료구조
# - 서로 다른 자료형(숫자, 문자, 논리형)과 자료구조(1,2,3)를 갖는 자료구조
# - key = value (python : dict)


# 1) key 생략 : [key =] value
lst1 <- list('lee', "이순신", 35, "hong", "홍길동", 45)
lst1
#[[1]] -> default key
#[1] "lee" -> value(string)   => 이 전체가 하나의 원소

# [[6]] -> default key
# [1] 45 -> value(int)

# key -> value 접근

lst1[[1]] #-> "lee"의 key : "lee"
lst1[[2]] #-> "이순신"의 key : "이순신"

# index -> key:value 접근
lst1[4] # -> [[4]], "hong"
#[[1]] -> key 전체를 봤을땐 1234로 나오지만 하나만 봤을때는 원소 1개만 나오기 때문에 1로 나온다
#[1] "hong" -> value

install.packages("stringr")
library(stringr)

str <- "홍길동35이순신45"
names <- str_extract_all(str, "[가-힣]{3}")
names
# [[1]] -> key
# [1] "홍길동" "이순신" -> value

class(names) # "list"
names[[1]] # [1] "홍길동" "이순신"
names[[1]][2] # [1] "이순신" // [[1]] : key, [2] : vector index

# 2) key = value
member = list(name = c("홍길순", "이순신"),
              age = c(35, 45),
              gender = c("여", "남"))

# $name -> key
# [1] "홍길순" "이순신" -> value

# key -> value
member$name # [1] "홍길순" "이순신"
member$name[2] # [1] "이순신"
class(member) # "list"

# $기호
# data.frame vs list
# data.frame$column
# list$key
