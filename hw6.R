library('plyr')

# Отберите из baseball записли для команд у которых есть хотябы 200 записей.
teams = split(baseball, baseball$team)

# sapply применяем к каждому элементам листа, ltngth к каждому 
# элементу листа в листе
number_of_records = lapply(teams, function(x) length(x[[1]])) # количество записей для каждой команды
teams_filtered = unlist(number_of_records[number_of_records >= 200]) # команды с более чем 200 записей
baseball_filtered = teams[names(teams_filtered)] # извлекаем из списка команды с более чем 200 записями

# Рассчитайте матрицу переходов игроков между командами — для каждой пары
# команд в этой матрице указано сколько есть игроков игравших в обоих командах.
# сколько 
count_shared_players = function(x){
  matr = matrix(0, nrow=32, ncol=32)
  colnames(matr) = names(x)
  rownames(matr) = names(x)
  for(i in c(1:length(x))){
    for(j in c(1:length(x))){
      matr[i,j]=sum(as.numeric(x[[i]] %in% x[[j]]))
    } 
  }
  return(matr)
}

team_players = sapply(baseball_filtered, function(x) unique(x[['id']]))
shared_players = count_shared_players(team_players)

# Поделите каждую ячейку полученной матрицы на размер (количество игроков
# которые есть в baseball) меньшей (из пары) команды.
numb_players_team = lengths(team_players)

find_smaller_team = function(x){
  matr2 = matrix(0, nrow=32, ncol=32)
  colnames(matr2) = names(x)
  rownames(matr2) = names(x)
  for(i in c(1:length(x))){
    for(j in c(1:length(x))){
      if((x[i]<x[j])==TRUE){
        matr2[i,j]=x[i]
      } else {
        matr2[i,j]=x[j]}
    } 
  }
  return(matr2)
}

smaller_teams = find_smaller_team(numb_players_team)
norm_matr = shared_players/smaller_teams

# Получите матрицу расстояний вычтя из единицы матрицу полученную на
# предыдущем шаге.
dist_matr = 1-norm_matr

#Проведите многомерное шкалирование и отрисуйте его результаты (m — матрица
#полученная на предыдущем этапе)
mds = cmdscale(dist_matr,k=2)
plot(mds,pch=19, cex=numb_players_team/200) # lwd - размер точек
text(mds,rownames(mds),adj=c(1.2,1.2),col='red')

# Модифицируйте код из предыдущего пункта так, чтобы размер точек был
# пропорционален размеру команд.