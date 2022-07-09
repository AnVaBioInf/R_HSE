library('plyr')
library("RColorBrewer")

# list of teams and info about them
teams = split(baseball, baseball$team)

# list of number of games played for each column
numb_of_games_played = c()
for(i in unique(baseball$team)){
  numb_of_games_played[i]=length(teams[[i]][[2]])
}

# colors
bbgor = colorRamp(c('blue','grey','red'))
sy=(numb_of_games_played-min(numb_of_games_played))/
  (max(numb_of_games_played)-min(numb_of_games_played))
cols=apply(bbgor(sy),1,function(x)rgb(x[1],x[2],x[3],maxColorValue = 255))

# plotting
j=1
plot(1,xlim=c(1860,2020),ylim=c(0,135), xlab="Year", ylab="Team", t='n')
for(i in names(numb_of_games_played)){
  lines(x=teams[[i]][[2]], y=rep(j, numb_of_games_played[j]), t='l',
        lwd=numb_of_games_played[j]/360,
        col=cols[j]
        )
  j=j+1
}

##
# Какая самая старая из команд существовавших на 2005 год?
new = split(baseball, baseball$year)
teams_existed_in_2005 = new[['2005']][['team']]

baseball[(baseball[['team']] %in% teams_existed_in_2005),]

# ответ : CHN
