#7. Пьяный идет по мосту шириной l шагов. Каждый шаг пьяный смещается случайно на
#один шаг в право или влево. Если пьяный переступит через край моста — он падает и
#умирает. Напишите функцию моделирующую движение пьяного вплоть до падения.
#Ширина моста и положение пьяного — начальные параметры. Нарисуйте на одном
#графике 100 разных траекторий.

record_of_positions=list()

func1 = function(i, 
                 width=width_val, 
                 position=position_val, 
                 new_position=new_position_val){
          vec=c()
          for(j in 1:10000000){
            new_position = new_position+sample(c(1,-1),1)
            vec[j]=new_position
            if(new_position == 0| new_position == width){
              break
            }
          }
          return(vec)
          }

for (i in 1:100){
  width_val = sample(20:100,1)
  position_val = sample(1:width_val,1)
  new_position_val = position_val
  record_of_positions[[i]] = func1(i)}

plot(1, xlim=c(1,mean(lengths(record_of_positions))), ylim=c(1,100))
for(i in 1:100){
    lines(
      record_of_positions[[i]],
      type='l')
}



#Напишите функцию открывающую скобки в выражении (a+b)n
#функция должна, брать на
#вход n и возвращать текст:  fun(3)«a^3+3a^2b+3ab^2+b^3»

# это Бином Ньютона
# найдем биномиальные коэффициенты
# n!/(k!(n-k)!)

n_=2

func2 = function(n=n_){
    step_a = c()
    k=c(0)
    n_min_k = n
    for(i in 1:n){
      if(i==1){
        k[i+1]=1
      }
      else{
        k[i+1]=k[i]*(i)
      }
    }
    for(i in k) koef = k[length(k)]/(k*rev(k))
    for(i in 1:(n+1))step_a[i]=(n+1)-i
    step_b=rev(step_a)
    paste0("fun(",n,")«a^",step_a[1],"+",koef[2],"a^",step_a[2],"b+",
        koef[3],"ab^",step_b[3],"+b^",step_b[4],"»")
  }

func2(2)
