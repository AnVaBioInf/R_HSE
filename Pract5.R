#1)Создайте матрицы x0 и y0 размером 1000*1000. 
#Заполните матрицы так, что в х0 все строки
#одинаковы и содержат последовательные числа от -2 до 1. 
#В y0 одинаковы колонки и они содержат равномерно распределенные числа 
#от -1 до 1
n = 1000
v_x = seq(-2,1,length.out = n)
v_y = seq(-1,1,length.out = n)
x0 = matrix(v_x,ncol=n,nrow=n,byrow=T)
y0 = matrix(v_y,ncol=n,nrow=n)

#2) Сделайте матрицы x и y равные x0 и y0 соответственно
x = x0
y = y0

#3) Пересчитайте матрицы x и y по формуле 
#(обратите внимание на то, что новые значения x и y зависят от СТАРЫХ x и y):
#  x = x^2-y^2 + x0
#  y = 2*x*y + y0
func1 = function(x,y){
          for(i in 1:20){
          x1 = x^2-y^2 + x0
          y1 = 2*x*y + y0
          x=x1
          y=y1
          }
          return(list(x,y))
}

l = func1(x,y)

x=l[[1]]
y=l[[2]]

#5) Выполните:
#  z = t(abs(x^2 + y^2))
#  z[!is.na(z)] = rank(z[!is.na(z)])
#  image(z^3,col=rev(terrain.colors(1000)))

z = t(abs(x^2 + y^2))
z[!is.na(z)] = rank(z[!is.na(z)])
image(z^3,col=rev(terrain.colors(1000)))

