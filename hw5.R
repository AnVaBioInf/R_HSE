my.rbinom = function(n,p=0.5){
  x = sum(runif(n,0,1)<p)
  return(x)
}

# в скобках указываются параметры ф-ии
# можно задать дефолтное значение
# в фигурных скобках то что ф-я делает

# return прерывает выполнение функции и выдает полученное значение

# ВЫЗОВ ФУНКЦИИ
my.rbinom(100) # n
my.rbinom(0.1,100) # n=0.1, т.е 0 бросков
my.rbinom(p=0.1,100) # n=0.1, т.е 0 бросков

my.rbinom(100,0.1) # n=0.1, т.е 0 бросков
# порядок аргументов важен!

plot(1:10,sin(1:10))
plot(x=1:10,y=sin(1:10), col='red')


# классы
x=1:10
class(x)
length(x)
class(x)='my' # my - атрибут

length.my = function(x)runif(1)
length(x)

class(x) = 'integer'
length(x)

text="Hello world"
#substring - много разных подслов из одного слова
#substr работает с векторами слов
#strsplit(text,'l') - разделяет по разделителю
#paste - соединяет эл-ты вместе
# вектора paste(text[1],text[2])
#paste0 -разделитель по умолчанию пустая строка
# paste(text,1:2,5:6) - поэлементно соединяет вектора
# grep - возвращает номера элементов
# grep('rl',text)
#regexpr('l',text) - находит все значентя
gsub('Hello','Good bye', text)
