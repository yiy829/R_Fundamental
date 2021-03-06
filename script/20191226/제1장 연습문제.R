#################################
## <제1장 연습문제>
#################################

#01. 현재 작업 공간을 확인하고 "D:/Rwork/"로 변경하시오.
getwd()
setwd("C:/Rwork/R_Fundamental")
#02. 다음 조건에 맞게 name, age, address 변수를 생성하고 출력하시오.

#조건1) name, age, address 변수에 자신의 이름, 나이, 주소를 만들고 출력한다. 
name <- c("여인영")
age <- c(29)
address <- c("경기도 고양시 일산동구")
name; age; address

#조건2) mode() 함수를 이용하여 각 변수의 자료형(data type)을 확인한다.
mode(name)
mode(age)
mode(address)

#03. 다음 rain 변수는 비 유무를 나타내는 변수이다. 이 변수를 요인형으로 변경하시오.  
rain <- c('YES', 'No', 'YES', 'YES', 'NO')
rain2 <- as.factor(rain)
class(rain2)

#04. R에서 제공하는 women 데이터 셋을 다음과 같이 처리하시오.

#조건1) women은 어떤 데이터 셋 인지를 쓰시오? 15열의 관측치 및 키와 몸무게 변수를 가진 데이터프레임
data()
#Average Heights and Weights for American Women

str(women)
women

#조건2) women 데이터 셋의 자료형은 무엇인가? list
mode(women)

#조건3) plot() 함수를 이용하여 기본 차트 그리기
plot(women)


#05. R에서 제공하는 c()함수를 이용하여 벡터를 생성하고, 자료를 처리하시오.

#조건1) 1~10까지 벡터를 생성한다.
num <- c(1:10)

#조건2) 생성된 벡터를 대상으로 평균을 구한다.
mean(num)
