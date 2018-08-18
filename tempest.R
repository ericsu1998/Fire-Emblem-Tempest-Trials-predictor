#Helper functions
predicted.rank = function(model, score, start.time, time){
  coefs = coef(model)
  hrs.from.start = difftime(time, start.time, units="hours")
  return(coefs["(Intercept)"] + 
           score*coefs["Score"] + 
           hrs.from.start*coefs["Hrs.From.Start"])
}

predicted.score = function(model, rank, start.time, time){
  coefs = coef(model)
  hrs.from.start = difftime(time, start.time, units="hours")
  return((rank - coefs["(Intercept)"] - 
          hrs.from.start*coefs["Hrs.From.Start"]) / 
          coefs["Score"])
}

time.diff.hrs = function(start, end){
  return(as.numeric(difftime(end, start, units="hours")))  
}

target.rank.stats = function(rank){
  target.score = predicted.score(model=rank.vs.score.time,
                                 rank=rank,
                                 start.time=start.time,
                                 time=end.time)
  games.to.target.score = (curr.score - target.score) / rank.increase.per.game
  print(paste("Score needed to get top", rank, ":", round(target.score)))
  print(paste("Games needed to get top", rank, ":", round(games.to.target.score)))
}

#Initialize variables
end.time = as.POSIXct("2018-08-24 00:00:00 EDT") #Change this to not be hard-coded
target.rank = 1000
game.score = 705
#Read data, do some preprocessing
tempest.file = readline(prompt="Enter name of tempest file (ex. Tempest.csv): ")
tempest.data = read.csv(tempest.file)
starting.score = head(tempest.data$Score, 1)
tempest.data$Time = strptime(tempest.data$Time, "%Y-%m-%dT%H:%M")
start.time = tempest.data$Time[1]
tempest.data$Hrs.From.Start = difftime(tempest.data$Time,
                                       start.time,
                                       units="hours")
tempest.data$Hrs.From.Start = as.numeric(tempest.data$Hrs.From.Start)
tempest.data$Score.Game = tempest.data$Score/game.score

#Plot Score and Rank vs. Time
options(scipen=999) #Disable scientific notation
par(mar=c(5, 4, 4, 6) + 0.1)
plot(tempest.data$Time, tempest.data$Rank, type="l", col="red",
     xlab="Time", ylab="Rank")
par(new=TRUE)
plot(tempest.data$Time, tempest.data$Score, xlab="", ylab="",axes=FALSE,
     col="blue", type="l")
mtext("Score",side=4,line=4)
axis(4, ylim=range(tempest.data$Score),las=1)
options(scipen=0) #Reset scientific notation to default

#Linear models
rank.vs.score.time = lm(Rank ~ Score + Hrs.From.Start, 
                        data=tempest.data)
rank.vs.score.time.interpretable = 
  lm(Rank ~ Score.Game + Hrs.From.Start, 
     data=tempest.data)
print(coefficients(rank.vs.score.time.interpretable))

score.vs.time = lm(Score ~ Hrs.From.Start, data=tempest.data)

#Predictions and interesting stuff
curr.rank = tail(tempest.data$Rank, 1)
curr.score = tail(tempest.data$Score, 1)

end.rank = predicted.rank(model=rank.vs.score.time, 
                          score=curr.score,
                          start.time=start.time,
                          time=end.time)

hrs.to.end = time.diff.hrs(start.time, end.time)

score.per.hr = coefficients(score.vs.time)[2]
time.left = hrs.to.end - tail(tempest.data$Hrs.From.Start, 1)
predicted.end.score = curr.score + time.left * score.per.hr
end.rank.2 = predicted.rank(model=rank.vs.score.time, 
                            score=predicted.end.score,
                            start.time=start.time,
                            time=end.time)
rank.increase.per.game = coefficients(rank.vs.score.time.interpretable)["Score.Game"]

curr.time = tail(tempest.data$Hrs.From.Start, 1)
hrs.per.day = 24
pts.per.day = (curr.score - starting.score)/curr.time * hrs.per.day
games.per.day = round(pts.per.day/game.score)
print(paste("Current rank:", curr.rank))
print(paste("Current score:", curr.score))
print(paste("Pts per day:", round(pts.per.day)))
print(paste("Games per day:", (games.per.day)))
print(paste("Predicted end score:", round(predicted.end.score)))
print(paste("Predicted end rank (current score):", round(end.rank)))
print(paste("Predicted end rank (predicted score):", round(end.rank.2)))
target.rank.stats(rank=5000)
target.rank.stats(rank=1000)

