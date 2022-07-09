###### ДЗ1 15.09.2021 Васильева Анастасия

# Читаем файл hw1.txt
df = read.table('hw1.txt')

# Выясняем есть ли зависимость между экспрессиями в какой-либо паре генов
# Находим коэффициенты корреляции Спирмана (т.к. распределения ненормальные)
cor(df, method = 'sp') 
# Проверяем статистическую значимость корреляции для генеральной совокупности. H0:Kkorr=0
cor.test(df$CPY14, df$RMD4, method = 'sp') # с вероятностью 2.2у-16 гепотеза о том, что зависимости между экспрессией генов верна, т.е. с вероятностью ~1 есть положительная корреляция = 0.82
cor.test(df$CPY14, df$htVWQ, method = 'sp')# c вероятностью ~0.6 есть отрицательная корреляция = 0.081
cor.test(df$RMD4, df$htVWQ, method = 'sp') # c вероятностью ~0.6 есть отрицательная корреляция = 0.088 
# Пара генов со значимой зависимостью (p>0.95): CPY14-RMD4

# Выясняем есть ли отличия в средней экспрессии между какой либо парой генов 
# Применяем критерий Wilcoxon-Matt-Whitney (т.к. распределения ненормальные). H0 -  mu(x)=mu(y) 
wilcox.test(df$CPY14, df$RMD4)$p.value # conf.int - nonparametric confidence interval and an estimator for the difference of the location parameters x-y
wilcox.test(df$CPY14, df$htVWQ)$p.value
wilcox.test(df$RMD4, df$htVWQ)$p.value
# Список пар генов со значимыми отличиями среднего (p>0.95) RMD4-htVWQ, CPY14-RMD4, CPY14-htVWQ,

# Код необходимый для получения картинок иллюстрирующих все вышеперечисленное
par(mfrow=c(2,3))
# Иллюстрации экспрессии генов 
hist(df$CPY14, col='yellow', xlab='CPY14')
hist(df$RMD4, col='green', xlab='RMD4')
hist(df$htVWQ, col='red', xlab='htVWQ')

# Иллюстрация корреляции генов
plot(df$CPY14, df$RMD4, xlab='CPY14', ylab = 'RMD4')
abline(lm(df$RMD4 ~ df$CPY14), col='red') # методом наименьших квадратов (lm) строим линию
plot(df$CPY14, df$htVWQ, xlab='CPY14', ylab = 'htVWQ')
abline(lm(df$htVWQ ~ df$CPY14), col='red')
plot(df$RMD4, df$htVWQ, xlab='RMD4', ylab = 'htVWQ')
abline(lm(df$htVWQ ~ df$RMD4), col='red')

# BoxPlot для иллюстрации средних
par(mfrow=c(1,2))
boxplot(df)