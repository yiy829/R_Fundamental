# chap01_Basic

# 수업내용 
# 1. session 정보
# 2. R 실행방법
# 3. 패키지와 데이터셋
# 4. 변수와 자료형
# 5. 기본함수 사용 및 작업공간

# 1. session 정보
# session : R의 실행환경
sessionInfo() #7개의 기본패키지 확인가능
# R ver, 다국어(locale), base packages


## 2. R 실행방법

#주요 단축키
# script 실행 : Ctrl + Enter or Ctrl + R
# 자동완성 : Ctrl + Space
# 저장 : Ctrl + s
# 여러줄 주석 : Ctrl + Shift + C

# 1) 줄 단위 실행
a <- rnorm(n=20) # <- or =
a
hist(a) # 히스토그램
mean(a) # -0.347..

# 2) 블럭 단위 실행
getwd() # 현재 작업경로 보기 - 'c:/Rwork'
pdf("test.pdf") #현재 작업경로에 pdf 파일 생성
hist(a) #히스토그램
dev.off() #close 되는 순간 해당 pdf 차트 생성
#pdf~dev.off 까지 블록단위실행된다.


## 3. 패키지와 데이터셋

# 1) 패키지 = function + [dataset]

# 사용가능한 패키지
dim(available.packages()) #가용한 패키지를 표기
# 15328(row) (패키지갯수) * 17(col) (17줄의 설명제공)


# 패키지 설치/사용
install.packages("stringr")

# 패키지 in memory
library(stringr)

# 사용가능한 패키지 확인
search()

#설치위치
.libPaths()

#패키지 활용
str <- "홍길동35이순신45유관순25"
str

# 이름 추출
str_extract_all(str, "[가-힣]{3}") 
# 가~힣까지 3글자 단위로 추출 하겠다.

# 나이 추출
str_extract_all(str, "[0-9]{2}") 

# 패키지 삭제
remove.packages("stringr")

# 2) 데이터셋
data() #기본 데이터셋 목록 불러오기
data("Nile") # in memory
Nile
length(Nile) #100
mode(Nile) #"numeric"
plot(Nile)
mean(Nile) # 100년치의 평균값


## 4. 변수와 자료형

# 1) 변수(variable) : 메모리 주소 저장
# - R의 모든 변수는 객체(참조변수)
# - 변수 선언 시 type 없음
a <- c(1:10)
a

# 2) 변수 작성 규칙
# - 첫자는 영문자
# - 점(.)을 사용(lr.model)
# - 예약어 사용 불가
# - 대소문자 구분 : num or NUM

var1 <- 0 # var1 = 0
# int var1 = 0
var1 <- 1
var1

var2 <- 10
var3 <- 20
var2; var3 #두 변수 동시에 확인 가능

# 객체.멤버
member.id <- "hong"
member.name <- "홍길동"
member.pwd <- "1234"

num <- 10
NUM <- 100
num; NUM

# scala(1) vs vector(n)
name <-  c("홍길동", "이순신", "유관순")

name[2] #특정 원소만 직접 참조 "이순신"

# tensor : scala(0), vector(1), matrinx(2)

#변수 목록
ls()

# 3) 자료형
# - 숫자형, 문자형, 논리형

int <- 100 # 숫자형(연산, 차트 )
string <- "대한민국" #'대한민국' 둘다 가능
boolean <- TRUE # T, FALSE(F)

# 자료형 반환 함수
mode(int) # numeric
mode(string) # charactor
mode(boolean) # logical

# is.xxx()
is.numeric(int) # TRUE
is.character(string) # TRUE
is.numeric(boolean) # FALSE

x <- c(100, 90, NA, 65, 78) # NA : 결측치(9999)
is.na(x) # 해당 객체에 NULL 값이 있냐


# 4) 자료형변환(casting)

# 1) 문자열 -> 숫자형
num <- c(10, 20, 30, "40")
num
mode(num) #numeric // 한개만 문자형 -> 전부 문자형 변환
plot(num)
mean(num)
num <- as.numeric(num)
mode(num)
mean(num)


# 2)요인형(Factor)
# - 동일한 값을 범주로 갖는 집단변수 생성
#ex) 성별) 남(0), 여(1) -> 더미변수

gender <- c("M", "F", "M", "F", "M")
mode(gender)
plot(gender) #ERROR

# 요인형 변환 : 문자열 -> 요인형
fgender <- as.factor(gender)
mode(fgender)
fgender
# [1] M F M F M
# Levels: F M 자동으로 레벨이라는 수준을 만들어 numeric 형태

str(fgender)
# Factor w/ 2 levels "F"(1),"M"(2): 2 1 2 1 2
plot(fgender)

x <- c('M', 'F')
fgender2 <- factor(gender, levels = x)
str(fgender2)
# Factor w/ 2 levels "M"(1),"F"(2): 1 2 1 2 1

# mode vs class
# mode() : 자료형 반환
# class() : 자료구조 반환 
mode(fgender)
class(fgender)


# Factor형 고려사항
num <- c(4, 2, 4, 2)
mode(num)
fnum <- as.factor(num)
fnum
# [1] 4(2) 2(1) 4 2
# Levels: 2(1) 4(2)
str(fnum)
# Factor w/ 2 levels "2","4": 2 1 2 1

# 요인형 -> 숫자형
num2 <- as.numeric(fnum)
num2 # [1] 2 1 2 1 원래 4242 였는데 2121로 바뀜

# 요인형 -> 문자형 -> 숫자형 : 정상적으로 변경
snum <- as.character(fnum)
num2 <- as.numeric(snum)
num2 # 4 2 4 2


## 5. 기본함수 사용 및 작업공간

# 1) 함수 도움말
mean(10, 20, 30) #평균 - 10 (오류)
x <- c(10, 20, 30, NA)
mean(x, na.rm = TRUE) #20
help(mean) #?mean 같은 의미

mean

sum(x, na.rm = TRUE)

# 2) 작업공간
getwd() #"C:/Rwork"
setwd("C:/Rwork/data")

test <- read.csv("test.csv")
test

str(test)
# 'data.frame' : 402 obs. of 5 variables:
# obs. : rhkscmrcl 402(행)
# variables : 변수, 변인 5(열)
