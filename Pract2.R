#### ПЗ2  18/09/2021  Васильева Анастасия

# Задаем случайное число
x = 2 #runif(1,max=10)
x
k = c(0:10)

# Считаем ряд Тейлора
teil = cumsum(((-1)^k*x^(2*k+1))/factorial(2*k+1))

# Строим график сходимости
par(mfrow=c(1,2))
plot(0:10,teil, xlab = "x", ylab = "sin(x)", title(x))
abline(h=sin(x),col='red')

# Логарифм ряда Тейлора
plot(0:10, log(teil), xlab = "x", ylab = "log(sin(x))")
abline(h=log(sin(x)),col='red')

