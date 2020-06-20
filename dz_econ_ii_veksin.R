library(tidyverse)
library(fable)
library(feasts)
library(rio)
library(tsibble)
library(lubridate)
library(forecast)
library(anchors)

# 1-ая часть:

# Попробуем оценить как на ввп/душу населения влияет структура этого ввп. Для этого оставим
# переменные ввп на душу населения, переменные процента сельского хозяйства, индустрии и оказания услуг в ввп 
# Также оставим индексы агрокультуры и показатели международной торговли.
# Еще можно оставить контрольные переменные отражающие вклад в человеческий капитал:
# Расходы на образование и здравоохранение.

mydata <- read.table("C:/Users/Archaedas/Desktop/econometri_dz/country_profile_variables.csv", header=TRUE,
                     sep=",")

# Также можно посмотреть зависимость от квадрата долей с/х, индустрии и сервиса услуг
# Создадим бинарную переменную: положительный ли торговый баланс (1-да, 0-нет)
# Создадим переменную взаимодействия: произведение затрат на здравоохранение и образование

replace.value(mydata, Health, from="...", to=as.integer(0), verbose = FALSE)
replace.value(mydata, Education, from="...", to=as.integer(0), verbose = FALSE)
replace.value(mydata, Balance, from="...", to=as.integer(0), verbose = FALSE)
replace.value(mydata, Balpay, from="...", to=as.integer(0), verbose = FALSE)

#mydata$Health <- replace(mydata$Health, mydata$Health == 0, "...")
#mydata$Education <- replace(mydata$Education, mydata$Education == 0, "...")
#mydata$Balance <- replace(mydata$Balance, mydata$Balance == 0, "...")
#mydata$Balpay <- replace(mydata$Balpay, mydata$Balpay == 0, "...")

mydata$sqsh <- mydata$Agri * mydata$Agri
mydata$sqind <- mydata$Indu * mydata$Indu
mydata$sqser <- mydata$Serv * mydata$Serv
mydata$posbal <- 1L * (mydata$Balance >= 0)
mydata$humcapital <- mydata$Health * mydata$Education

# Посмотрим на данные: 

hist(mydata$GDP, main="Histogram for GDP", xlab="percent")
boxplot(mydata$GDP, main="GDP", ylab="percent")
stat.desc(mydata$GDP)

hist(mydata$Agri, main="Histogram for Agriculture", xlab="percent")
boxplot(mydata$Agri, main="Agriculture", ylab="percent")
stat.desc(mydata$Agri)

hist(mydata$Indu, main="Histogram for Industry", xlab="percent")
boxplot(mydata$Indu, main="Industry", ylab="percent")
stat.desc(mydata$Indu)

hist(mydata$Serv, main="Histogram for Service", xlab="percent")
boxplot(mydata$Serv, main="Service", ylab="percent")
stat.desc(mydata$Serv)

hist(mydata$Health, main="Histogram for Health", xlab="percent")
boxplot(mydata$Health, main="expences on Health", ylab="percent")
stat.desc(mydata$Health)

hist(mydata$Education, main="Histogram for Education", xlab="percent")
boxplot(mydata$Education, main="Expences on Education", ylab="percent")
stat.desc(mydata$Education)

#
#
#
#
#
#
#
#
#
#
#
#










# РядыРядыРядыРядыРядыРядыРядыРядыыы

# 1)
  
# Первый ряд (AR1) Стационарный, так как решение характеристического уравнения (λ-0.8) < 1
  
n <- 121
  
wn <- ts(rnorm(120, 0, 17))
  
ar1 <- rnorm(1, 2, 1)
  
for(i in 2:n){
 ar1[i] <- ar1[i - 1] * 0.8 + wn[i]
}
  
timeser1 <- ts(ar1)
  
plot(timeser1[1:i], main="Y_t=0.8*Y_{t-1}+ε_t", bty = "l", type = "l", ylab = "y =AR (1)", xlab = "id")
  
# Второй ряд (AR3). Стационарный: решения уравнения (λ^3-0.1*λ^2-0.2*λ-0.3)
# https://www.wolframalpha.com/input/?i=x%5E3-0.1*x%5E2-0.2*x-0.3 меньше единицы по модулю. 
  
n <- 123
  
wn <- ts(rnorm(120, 0, 17))
  
ar2 <- rnorm(1, 2, 1)
ar2[1] <- rnorm(1, 2, 1)
ar2[2] <- rnorm(1, 2, 1)
ar2[3] <- rnorm(1, 2, 1)
  
for(i in 4:n){
  ar2[i] <- ar2[i - 1] * 0.1 + ar2[i - 2] * 0.2 + ar2[i - 3] * 0.3 + wn[i]
}
  
timeser2 <- ts(ar2)
  
plot(timeser2[1:i], main="Y_t=0.1*Y_{t-1}+0.2*Y_{t-2}+0.3*Y_{t-3}+ε_t", bty = "l", type = "l", ylab = "y = AR (3)", xlab = "id")
  
# Третий ряд (MA2) Стационарный, все эпсилоны распределены одинаково.
  
n <- 123
  
wn2 <- ts(rnorm(123, 0, 17))
  
ma1 <- 0
ma1[1] <- 0
  
for(i in 3:n){
  ma1[i] <- wn2[i] + wn2[i - 1] * 1.2 + wn2[i - 2] * 2
}
  
timeser3 <- ts(ma1)
  
plot(timeser3[1:i], main="Y_t=ε_t+1.2*ε_{t-1}+2*ε_{t-2}", bty = "l", type = "l", ylab = "y = MA(2)", xlab = "id")
  
# 2)
  
# ARIMA (0 1 2)
  
n <- 123
  
wn3 <- ts(rnorm(123, 0, 17))
  
mart <- 0
mart[1] <- 0
  
for(i in 3:n){
  mart[i] <- wn3[i] + wn3[i - 1] * 4
}
  
timeser4 <- ts(cumsum(mart))

# График для ΔΥ

plot(mart, main="ΔY", bty = "l", type = "l", ylab = "y ~ AR (0, 1, 2)", xlab = "id")
  
# ARIMA (0 0 0)
  
  
  
# ARIMA (3 0 0)
  
# 3) Случайное блуждание. Ряд не стационарный (λ-1=0) 
  
n <- 121
  
wn4 <- ts(rnorm(120, 0, 17))

rw <- rnorm(1, 0, 1)
rw[1] <- 0
            
for(i in 2:n){
rw[i] <- rw[i - 1] + wn4[i]
}

timeser5 <- ts(rw)

plot(timeser5[1:i], main="Случайное блуждание", bty = "l", type = "l", ylab = "y = Random Walking", xlab = "id")

# 4) 1-ый ряд из 1-го пункта задает AR(1)

Acf(timeser1, main="ACF for AR(1)")
Pacf(timeser1, main="PACF for AR(1)")

Acf(timeser5, main="ACF for Random Walking")
Pacf(timeser5, main="PACF for Random Walking")

# Автокорреляция стационарного ряда убывает экспоненциально, случайного блуждания линейно.

# 5) Генерируем стационарный ряд из 120 переменных, в одном случае берем посчитанные 120
# в другом 100 посчитанных и интервал для 20 предсказанных:

n <- 123

set.seed(1)
wn5 <- ts(rnorm(123, 0, 17))

aima <- rnorm(1, 0, 1)
aima[1] <- 0.5
aima[2] <- 0.4
aima[3] <- 0.3

for(i in 4:n){
  aima[i] <- 0.5 * aima[i - 1] + 0.2 * aima[i - 2] + wn5[i] + 2 * wn5[i - 1] + wn5[i - 2] + 2 * wn5[i - 3]
}

set.seed(1)
timeser6 <- ts(aima)

plot(forecast(timeser6[1:100],h=20), main="Predict", bty = "l", type = "l", col='red', ylab = "x = AR (2, 0, 3)", xlab = "id")
lines(timeser6[1:120], col='blue')

# При зафиксированной выборке посчитанные значения почти не выходят за границы 95% интервала.
