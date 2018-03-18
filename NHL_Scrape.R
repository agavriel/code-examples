#Re-worked Single Game NHL Scraper for post-game charts.
setwd("C:/Users/abg68/Desktop/R/NHL Scrape/20172018")

#Read in libraries
library(jsonlite)
library(data.table)
library(nhlscrapr)
library(tidyverse)
library(ggrepel)
library(grid)
library(reshape)

#Season
season <- "20172018"

#Game code
gcode <- 21105
jsonGameCode <- paste0(20170, gcode) #replace 20160 with correct season yr start

#Start with json page

#Store json page as object
jsonWebPage <- paste0("https://statsapi.web.nhl.com/api/v1/game/", jsonGameCode, "/feed/live?site=en_nhl")

#read json into a list object
pbplist <- fromJSON(jsonWebPage, simplifyDataFrame = TRUE)

#Store date
gameDate <- pbplist$gameData$datetime$dateTime
gameDate <- substr(gameDate, 1, 10)

#Data frame for events in game
gameEvents <- pbplist$liveData$plays$allPlays$result

#Row number identifier
gameEvents$rownumber <- 1:nrow(gameEvents)

#Drop strength columns. Unhelpful, and is not 1D list. breaks merging.
gameEvents <- subset(gameEvents, select = -strength)

#Grab event descriptions
eventsDescription <- pbplist$liveData$plays$allPlays$about

#Row number identifier
eventsDescription$rownumber <- 1:nrow(eventsDescription)

#Drop goals columns. Is not 1D. breaks merging.
eventsDescription <- subset(eventsDescription, select = -goals)

#merge
jsonDF <- dplyr::left_join(gameEvents, eventsDescription, by = "rownumber")

#Grab coordinates
gameCoords <- pbplist$liveData$plays$allPlays$coordinates

#apply identifier for merging
gameCoords$rownumber <- 1:nrow(gameCoords)

#merge
jsonDF <- dplyr::left_join(jsonDF, gameCoords, by = "rownumber")

#Clean environment
rm(eventsDescription, gameCoords, gameEvents)

#NHL Scraper time

#Store read List file into object
List <- process.single.game (season=season,
                             gcode=gcode,
                             rdata.folder="nhlr-data",
                             verbose=TRUE,
                             wait=0)

#Convert List to Data Frame
PBP2 <-  rbindlist(lapply(List, as.data.table), fill = TRUE)

#Remove "Change" or Period end
PBP <- subset(PBP2, etype != "CHANGE" & etype != "PEND")

#From JSON file, remove line-items not in NHL Scrape
jsonDF <- jsonDF[!jsonDF$event %in% c("Game Scheduled", "Period Ready", "Period Start", "Stoppage", "Period End", "Period Official", "Game End"),]
jsonDF$rownumber <- 1:nrow(jsonDF)

#put coordinates from JSON on to nhlscrapr df
PBP$rownumber <- 1:nrow(PBP)

#Select only necessary cols
jsonDFMerge <- subset(jsonDF, select = c(rownumber, x, y))

#Merge
pbpDone <- dplyr::left_join(PBP, jsonDFMerge, by = "rownumber")

#Drop excess cols
pbpDone <- subset(pbpDone, select = -c(number, pos, lastfirst, last, first, numlast, numfirstlast, V1))

#Store team
awayTeam <- unique(pbpDone$awayteam)
homeTeam <- unique(pbpDone$hometeam)

#Subset 5v5 play in its own df
even <- subset(pbpDone, a6 == "" & h6 == "" &
                 home.skaters == 6 &
                 away.skaters == 6)

#Subset shot attempt data
evShots <- subset(even, etype == "BLOCK" |
                    etype == "SHOT" |
                    etype == "MISS" |
                    etype == "GOAL")

#Shots for home team or away team
evShots$HomeFor <- ifelse(evShots$ev.team == homeTeam, 1, 0)
evShots$AwayFor <- ifelse(evShots$ev.team == awayTeam, 1, 0)

######Can we loop the aggregation process this year?######

#Manual Aggregate for Away Team
a1 <- aggregate(HomeFor ~ a1, data = evShots, sum)
a2 <- aggregate(HomeFor ~ a2, data = evShots, sum)
a3 <- aggregate(HomeFor ~ a3, data = evShots, sum)
a4 <- aggregate(HomeFor ~ a4, data = evShots, sum)
a5 <- aggregate(HomeFor ~ a5, data = evShots, sum)
a6 <- aggregate(HomeFor ~ a6, data = evShots, sum)

a12 <- aggregate(AwayFor ~ a1, data = evShots, sum)
a22 <- aggregate(AwayFor ~ a2, data = evShots, sum)
a32 <- aggregate(AwayFor ~ a3, data = evShots, sum)
a42 <- aggregate(AwayFor ~ a4, data = evShots, sum)
a52 <- aggregate(AwayFor ~ a5, data = evShots, sum)
a62 <- aggregate(AwayFor ~ a6, data = evShots, sum)

#Rename Cols to Player
colnames(a1)[1] <- "Player"
colnames(a2)[1] <- "Player"
colnames(a3)[1] <- "Player"
colnames(a4)[1] <- "Player"
colnames(a5)[1] <- "Player"
colnames(a6)[1] <- "Player"

colnames(a12)[1] <- "Player"
colnames(a22)[1] <- "Player"
colnames(a32)[1] <- "Player"
colnames(a42)[1] <- "Player"
colnames(a52)[1] <- "Player"
colnames(a62)[1] <- "Player"

#Row Bind and then Push Together
a1 <- dplyr::left_join(a1, a12, by = "Player")
a2 <- dplyr::left_join(a2, a22, by = "Player")
a3 <- dplyr::left_join(a3, a32, by = "Player")
a4 <- dplyr::left_join(a4, a42, by = "Player")
a5 <- dplyr::left_join(a5, a52, by = "Player")
a6 <- dplyr::left_join(a6, a62, by = "Player")

Away <- dplyr::bind_rows(a1, a2)
Away <- dplyr::bind_rows(Away, a3)
Away <- dplyr::bind_rows(Away, a4)
Away <- dplyr::bind_rows(Away, a5)
Away <- dplyr::bind_rows(Away, a6)

#Aggregate
AwayFor <- aggregate(AwayFor ~ Player, data = Away, sum)
AwayAgainst <- aggregate(HomeFor ~ Player, data = Away, sum)

#Bind
Away <- dplyr::left_join(AwayFor, AwayAgainst, by = "Player")

#Calculate Differential
Away$Differential <- Away$AwayFor - Away$HomeFor

#Remove team for Plot
AwayPlot <- subset(Away, Player != unique(evShots$awayteam))

#Manual Aggregate for Home Team
h1 <- aggregate(HomeFor ~ h1, data = evShots, sum)
h2 <- aggregate(HomeFor ~ h2, data = evShots, sum)
h3 <- aggregate(HomeFor ~ h3, data = evShots, sum)
h4 <- aggregate(HomeFor ~ h4, data = evShots, sum)
h5 <- aggregate(HomeFor ~ h5, data = evShots, sum)
h6 <- aggregate(HomeFor ~ h6, data = evShots, sum)

h12 <- aggregate(AwayFor ~ h1, data = evShots, sum)
h22 <- aggregate(AwayFor ~ h2, data = evShots, sum)
h32 <- aggregate(AwayFor ~ h3, data = evShots, sum)
h42 <- aggregate(AwayFor ~ h4, data = evShots, sum)
h52 <- aggregate(AwayFor ~ h5, data = evShots, sum)
h62 <- aggregate(AwayFor ~ h6, data = evShots, sum)

#Rename Cols to Player
colnames(h1)[1] <- "Player"
colnames(h2)[1] <- "Player"
colnames(h3)[1] <- "Player"
colnames(h4)[1] <- "Player"
colnames(h5)[1] <- "Player"
colnames(h6)[1] <- "Player"

colnames(h12)[1] <- "Player"
colnames(h22)[1] <- "Player"
colnames(h32)[1] <- "Player"
colnames(h42)[1] <- "Player"
colnames(h52)[1] <- "Player"
colnames(h62)[1] <- "Player"

#Row Bind and then Push Together
h1 <- dplyr::left_join(h1, h12, by = "Player")
h2 <- dplyr::left_join(h2, h22, by = "Player")
h3 <- dplyr::left_join(h3, h32, by = "Player")
h4 <- dplyr::left_join(h4, h42, by = "Player")
h5 <- dplyr::left_join(h5, h52, by = "Player")
h6 <- dplyr::left_join(h6, h62, by = "Player")

Home <- dplyr::bind_rows(h1, h2)
Home <- dplyr::bind_rows(Home, h3)
Home <- dplyr::bind_rows(Home, h4)
Home <- dplyr::bind_rows(Home, h5)
Home <- dplyr::bind_rows(Home, h6)

#Aggregate
HomeFor <- aggregate(HomeFor ~ Player, data = Home, sum)
HomeAgainst <- aggregate(AwayFor ~ Player, data = Home, sum)

#Bind
Home <- dplyr::left_join(HomeFor, HomeAgainst, by = "Player")

#Calculate Differential
Home$Differential <- Home$HomeFor - Home$AwayFor

#Remove team for Plot
HomePlot <- subset(Home, Player != unique(evShots$hometeam))

####Clean up Environment#####
rm(a1, a12, a2, a22, a3, a32, a4, a42, a5, a52, a6, a62, h1, h12, h2, h22, h3, h32, h4, h42, h5, h52, h6, h62)

#Replace blank with Team totals
Home$Player <- sub("^$", homeTeam, Home$Player)
Away$Player <- sub("^$", awayTeam, Away$Player)

#Team Totals
homeTeamSAF <- sum(evShots$HomeFor)
homeTeamSAA <- sum(evShots$AwayFor)
awayTeamSAF <- sum(evShots$AwayFor)
awayTeamSAA <- sum(evShots$HomeFor)

#Calculate Off Ice metrics for relSA% calculations
Home$offIceFor <- homeTeamSAF - Home$HomeFor
Home$offIceAgainst <- homeTeamSAA - Home$AwayFor

Away$offIceFor <- awayTeamSAF - Away$AwayFor
Away$offIceAgainst <- awayTeamSAA - Away$HomeFor

#Calculate relSA%
Home$relSA. <- (Home$HomeFor / (Home$HomeFor + Home$AwayFor)) -
  (Home$offIceFor / (Home$offIceFor + Home$offIceAgainst))

Away$relSA. <- (Away$AwayFor / (Away$AwayFor + Away$HomeFor)) -
  (Away$offIceFor / (Away$offIceFor + Away$offIceAgainst))

#Merge with HomePlot for ease and remove team line
HomePlot <- dplyr::left_join(HomePlot, Home, by = "Player")
HomePlot <- na.omit(HomePlot)

AwayPlot <- dplyr::left_join(AwayPlot, Away, by = "Player")
AwayPlot <- na.omit(AwayPlot)

#Merge
AwayPlot$Team <- "Away"
HomePlot$Team <- "Home"

shotattemptPlot <- dplyr::bind_rows(AwayPlot, HomePlot)

#Work in Time On Ice

#Get the IDs into a dataframe and pull unique then loop somehow?

#List object of away players
awayPlayers <- pbplist$liveData$boxscore$teams$away$players

#Data table
awayPlayersdf <- rbindlist(lapply(awayPlayers, as.data.table), fill = TRUE)

#Produces multiple rows for each player, isolate numeric (ID) rows
awayPlayersdf$person <- as.numeric(awayPlayersdf$person)
awayPlayersdf <- na.omit(awayPlayersdf)

#Create ID numeric
awayPlayersdf$playerID <- as.character(paste0("ID", awayPlayersdf$person))

awayIDList <- awayPlayersdf$playerID

#List object of home players
homePlayers <- pbplist$liveData$boxscore$teams$home$players

#Data table
homePlayersdf <- rbindlist(lapply(homePlayers, as.data.table), fill = TRUE)

#Produces multiple rows for each player, isolate numeric (ID) rows
homePlayersdf$person <- as.numeric(homePlayersdf$person)
homePlayersdf <- na.omit(homePlayersdf)

#Create ID numeric
homePlayersdf$playerID <- as.character(paste0("ID", homePlayersdf$person))

homeIDList <- homePlayersdf$playerID

#Substring to pull evenTimeOnIce
awayPlayersdf$evTOI <- sub(".*(evenTimeOnIce.*), (power.*)", "\\1", awayPlayersdf$stats)

#Substring to pull PP TOI
awayPlayersdf$ppTOI <- sub(".*(powerPlayTimeOnIce.*), (short.*)", "\\1", awayPlayersdf$stats)

#Substring to pull SH TOI
awayPlayersdf$pkTOI <- sub(".*(shortHandedTimeOnIce.*)", "\\1", awayPlayersdf$stats)

#Continue substringing

#Removes everything up to evTOI through the "
awayPlayersdf$evMIN <- gsub("evenTimeOnIce = \"", "", awayPlayersdf$evTOI)
awayPlayersdf$ppMIN <- gsub("powerPlayTimeOnIce = \"", "", awayPlayersdf$ppTOI)
awayPlayersdf$pkMIN <- gsub("shortHandedTimeOnIce = \"", "", awayPlayersdf$pkTOI)

#replace " with blanks
awayPlayersdf$evMIN <- gsub("\"", "", awayPlayersdf$evMIN)
awayPlayersdf$ppMIN <- gsub("\"", "", awayPlayersdf$ppMIN)
awayPlayersdf$pkMIN <- gsub("\"", "", awayPlayersdf$pkMIN)

#remove NULL and goalies
awayPlayersdf <- subset(awayPlayersdf, position != "G" &
                          position != "N/A")

#Delimit at ":"
#awayPlayersdf$evMIN <- separate(data = awayPlayersdf, col = evMIN, into = c("evMIN", "evSEC"))

#Separates the seconds from the minutes
awayPlayersdf$evSEC <- ifelse(nchar(awayPlayersdf$evMIN) == 5,
                              substr(awayPlayersdf$evMIN, 4, 5),
                              substr(awayPlayersdf$evMIN, 3, 4))

awayPlayersdf$ppSEC <- ifelse(nchar(awayPlayersdf$ppMIN) == 5,
                              substr(awayPlayersdf$ppMIN, 4, 5),
                              substr(awayPlayersdf$ppMIN, 3, 4))

awayPlayersdf$pkSEC <- ifelse(nchar(awayPlayersdf$pkMIN) == 6,
                              substr(awayPlayersdf$pkMIN, 4, 5),
                              substr(awayPlayersdf$pkMIN, 3, 4))

#Removes everything after the colon to isolate minutes
awayPlayersdf$evMIN <- gsub( ":.*$", "", awayPlayersdf$evMIN)
awayPlayersdf$ppMIN <- gsub( ":.*$", "", awayPlayersdf$ppMIN)
awayPlayersdf$pkMIN <- gsub( ":.*$", "", awayPlayersdf$pkMIN)

#Continue substringing

#Substring to pull evenTimeOnIce
homePlayersdf$evTOI <- sub(".*(evenTimeOnIce.*), (power.*)", "\\1", homePlayersdf$stats)

#Substring to pull PP TOI
homePlayersdf$ppTOI <- sub(".*(powerPlayTimeOnIce.*), (short.*)", "\\1", homePlayersdf$stats)

#Substring to pull SH TOI
homePlayersdf$pkTOI <- sub(".*(shortHandedTimeOnIce.*)", "\\1", homePlayersdf$stats)

#Continue substringing

#Removes everything up to evTOI through the "
homePlayersdf$evMIN <- gsub("evenTimeOnIce = \"", "", homePlayersdf$evTOI)
homePlayersdf$ppMIN <- gsub("powerPlayTimeOnIce = \"", "", homePlayersdf$ppTOI)
homePlayersdf$pkMIN <- gsub("shortHandedTimeOnIce = \"", "", homePlayersdf$pkTOI)

#replace " with blanks
homePlayersdf$evMIN <- gsub("\"", "", homePlayersdf$evMIN)
homePlayersdf$ppMIN <- gsub("\"", "", homePlayersdf$ppMIN)
homePlayersdf$pkMIN <- gsub("\"", "", homePlayersdf$pkMIN)

#remove NULL and goalies
homePlayersdf <- subset(homePlayersdf, position != "G" &
                          position != "N/A")

#Delimit at ":"
#homePlayersdf$evMIN <- separate(data = homePlayersdf, col = evMIN, into = c("evMIN", "evSEC"))

#Separates the seconds from the minutes
homePlayersdf$evSEC <- ifelse(nchar(homePlayersdf$evMIN) == 5,
                              substr(homePlayersdf$evMIN, 4, 5),
                              substr(homePlayersdf$evMIN, 3, 4))

homePlayersdf$ppSEC <- ifelse(nchar(homePlayersdf$ppMIN) == 5,
                              substr(homePlayersdf$ppMIN, 4, 5),
                              substr(homePlayersdf$ppMIN, 3, 4))

homePlayersdf$pkSEC <- ifelse(nchar(homePlayersdf$pkMIN) == 6,
                              substr(homePlayersdf$pkMIN, 4, 5),
                              substr(homePlayersdf$pkMIN, 3, 4))

#Removes everything after the colon to isolate minutes
homePlayersdf$evMIN <- gsub( ":.*$", "", homePlayersdf$evMIN)
homePlayersdf$ppMIN <- gsub( ":.*$", "", homePlayersdf$ppMIN)
homePlayersdf$pkMIN <- gsub( ":.*$", "", homePlayersdf$pkMIN)

#As numeric
awayPlayersdf$evMIN <- as.numeric(awayPlayersdf$evMIN)
awayPlayersdf$evSEC <- as.numeric(awayPlayersdf$evSEC)
awayPlayersdf$ppMIN <- as.numeric(awayPlayersdf$ppMIN)
awayPlayersdf$ppSEC <- as.numeric(awayPlayersdf$ppSEC)
awayPlayersdf$pkMIN <- as.numeric(awayPlayersdf$pkMIN)
awayPlayersdf$pkSEC <- as.numeric(awayPlayersdf$pkSEC)

#As numeric
homePlayersdf$evMIN <- as.numeric(homePlayersdf$evMIN)
homePlayersdf$evSEC <- as.numeric(homePlayersdf$evSEC)
homePlayersdf$ppMIN <- as.numeric(homePlayersdf$ppMIN)
homePlayersdf$ppSEC <- as.numeric(homePlayersdf$ppSEC)
homePlayersdf$pkMIN <- as.numeric(homePlayersdf$pkMIN)
homePlayersdf$pkSEC <- as.numeric(homePlayersdf$pkSEC)


#Calculate total TOI
awayPlayersdf$evTOI <- awayPlayersdf$evMIN * 60 + awayPlayersdf$evSEC
awayPlayersdf$ppTOI <- awayPlayersdf$ppMIN * 60 + awayPlayersdf$ppSEC
awayPlayersdf$pkTOI <- awayPlayersdf$pkMIN * 60 + awayPlayersdf$pkSEC

homePlayersdf$evTOI <- homePlayersdf$evMIN * 60 + homePlayersdf$evSEC
homePlayersdf$ppTOI <- homePlayersdf$ppMIN * 60 + homePlayersdf$ppSEC
homePlayersdf$pkTOI <- homePlayersdf$pkMIN * 60 + homePlayersdf$pkSEC

#Isolate player numbers for merging
AwayPlot$jerseyNumber <- gsub("[^0-9]","", AwayPlot$Player)
HomePlot$jerseyNumber <- gsub("[^0-9]","", HomePlot$Player)


#Merge
vars2 <- c("jerseyNumber", "evTOI", "ppTOI", "pkTOI")

#As data frame, not table
awayPlayersdf <- as.data.frame(awayPlayersdf)
homePlayersdf <- as.data.frame(homePlayersdf)

mergeAway <- awayPlayersdf[vars2]
mergeHome <- homePlayersdf[vars2]

awayDF <- dplyr::left_join(AwayPlot, mergeAway, by = "jerseyNumber")
homeDF <- dplyr::left_join(HomePlot, mergeHome, by = "jerseyNumber")

#Merge so plots can be looped and coded once

shotAttemptPlot <- dplyr::bind_rows(awayDF, homeDF)

#Replace with team code

shotAttemptPlot$Team <- ifelse(shotAttemptPlot$Team == "Away", awayTeam, homeTeam)

#Calculate per 60 with function for practice functioning
perSixty <- function(metric, timeOnIce) {
  metric*3600 / timeOnIce
}

#Shot Attempts for per 60
shotAttemptPlot$SAF60 <- ifelse(shotAttemptPlot$Team == awayTeam,
                                perSixty(shotAttemptPlot$AwayFor.x, shotAttemptPlot$evTOI),
                                perSixty(shotAttemptPlot$HomeFor.x, shotAttemptPlot$evTOI))

#Shot Attempts against per 60
shotAttemptPlot$SAA60 <- ifelse(shotAttemptPlot$Team == awayTeam,
                                perSixty(shotAttemptPlot$HomeFor.x, shotAttemptPlot$evTOI),
                                perSixty(shotAttemptPlot$AwayFor.x, shotAttemptPlot$evTOI))

#Faceoff calculations
#Subset full PBP by faceoff events
Faceoffs <- subset(PBP2, etype == "FAC")

#5v5 Only
Faceoffs <- subset(Faceoffs, home.skaters == 6 & away.skaters == 6)
Faceoffs <- subset(Faceoffs, a6 == "" & h6 == "")
Faceoffs$a6 <- unique(Faceoffs$awayteam)
Faceoffs$h6 <- unique(Faceoffs$hometeam)

#Add logic for Away Zone faceoff
Faceoffs$awayzone <- ifelse(Faceoffs$homezone == "Neu", "Neu", 
                            ifelse(Faceoffs$homezone == "Off", "Def",
                                   ifelse(Faceoffs$homezone == "Def", "Off", "")))

#Add 1 everywhere for Faceoff count
Faceoffs$Count <- as.numeric(1)

#Subset Away Team Faceoffs
FAway <- subset(Faceoffs, select = c("a1", "a2", "a3", "a4", "a5", "awayzone"))

#Reshape Data
FAway <- melt(FAway, id=c("awayzone"))

#Subset Home Team Faceoffs
FHome <- subset(Faceoffs, select = c("h1", "h2", "h3", "h4", "h5", "homezone"))

#Reshape Data
FHome <- melt(FHome, id=c("homezone"))

#Begin code for trended shots
shotsDF <- subset(PBP, etype == "SHOT" |
                    etype == "BLOCK" |
                    etype == "MISS" |
                    etype == "GOAL")

#Subset for Goals, Shots

ShotChart <- subset(jsonDF, eventTypeId == "MISSED_SHOT" |
                      eventTypeId == "SHOT" |
                      eventTypeId == "GOAL")

pbpNoBlocks <- subset(shotsDF, etype != 'BLOCK')

#Row numbers to merge
ShotChart$mergeRow <- 1:nrow(ShotChart)
pbpNoBlocks$mergeRow <- 1:nrow(pbpNoBlocks)

#Merge
ShotChart <- dplyr::left_join(ShotChart, pbpNoBlocks, by = "mergeRow")

#Set Coords as numeric
ShotChart$xcoord <- as.numeric(ShotChart$x)
ShotChart$ycoord <- as.numeric(ShotChart$y)

#Make Away Team Right & Home Team Left

#Home Team Negative
ShotChart$xcoord2 <- ifelse(ShotChart$ev.team == ShotChart$hometeam,
                            ifelse(ShotChart$xcoord > 0, 
                                   ShotChart$xcoord * -1,
                                   ShotChart$xcoord),
                            ShotChart$xcoord)
#Away Team Positive
ShotChart$xcoord2 <- ifelse(ShotChart$ev.team == ShotChart$awayteam,
                            ifelse(ShotChart$xcoord < 0 ,
                                   ShotChart$xcoord * -1,
                                   ShotChart$xcoord),
                            ShotChart$xcoord2)

#YCoord Home Team
ShotChart$ycoord2 <- ifelse(ShotChart$ev.team == homeTeam,
                            ifelse(ShotChart$period.x == 2,
                                   ShotChart$ycoord, 
                                   ShotChart$ycoord * -1),
                            ShotChart$ycoord)

#YCoord Away Team
ShotChart$ycoord2 <- ifelse(ShotChart$ev.team == awayTeam,
                            ifelse(ShotChart$period.x == 2,
                                   ShotChart$ycoord,
                                   ShotChart$ycoord * -1),
                            ShotChart$ycoord2)

#THEMES
my_theme <- theme(plot.background = element_rect(fill = "whitesmoke"),
                  panel.background = element_rect(fill = "whitesmoke"),
                  panel.grid.major = element_line(colour = "gray80"),
                  panel.grid.minor=element_blank())

my_theme2 <- theme(plot.background = element_rect(fill = "whitesmoke"),
                   panel.background = element_rect(fill = "whitesmoke"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank(),
                   legend.position = "bottom",
                   axis.title.y = element_blank())



#Loop plot for scatter shot attempts and relSA

for(team in unique(shotAttemptPlot$Team)) {
  #Subset
  teamPlot <- subset(shotAttemptPlot, Team == team)
  
  #Get Max and Min for annotations
  xMax <- max(teamPlot$SAF60) + 2
  xMin <- min(teamPlot$SAF60) - 2
  yMax <- max(teamPlot$SAA60) + 2
  yMin <- min(teamPlot$SAA60) - 2
  
  #Plot Titles
  title <- paste0("5v5 Shot Attempts per 60 ", team, " ", gameDate)
  
  #RelSA
  title2 <- paste0("5v5 Relative Shot Attempt Percentage ", team, " ", gameDate)
  
  #Shot Attempt Plot
  saPlot <- ggplot(teamPlot, aes(x = SAF60, y = SAA60)) + geom_label_repel(aes(label = Player)) +
    geom_abline(intercept = 0, slope = -1, colour = "grey50", linetype = "dashed") +
    geom_point(data = teamPlot, aes(size = evTOI)) +
    scale_y_continuous(trans = "reverse") + 
    scale_size_continuous("TOI") +
    annotate("text", x = xMin, y = yMin, label = "Bad O\nGood D", colour = "red", size = 3) +
    annotate("text", x = xMax, y = yMin, label = "Good O\nGood D", colour = "red", size = 3) +
    annotate("text", x = xMin, y = yMax, label = "Bad O\nBad D", colour = "red", size = 3) +
    annotate("text", x = xMax, y = yMax, label = "Good O\n Bad D", colour = "red", size = 3) +
    my_theme +
    labs(x = "Shot Attempts For per 60",
         y = "Shot Attempts Against per 60 (Inverted)",
         title = title,
         caption = "@nerdhockeyAG\nsource = NHL PBP")
  print(saPlot)
  ggsave(saPlot, filename = paste0("Shot-Attempts_", team, "_", gameDate, ".png"), width = 11, height = 7)
  
  #Relative Shot Attempt Percentage Plot
  SArel <- ggplot(teamPlot, aes(reorder(factor(Player), relSA.), relSA.)) + 
    geom_bar(stat="identity") +
    coord_flip() + 
    labs(y = "Relative Shot Attempt Percentage",
         title = title2,
         caption = "@nerdhockeyAG\nsource = NHL PBP") +
    my_theme2
  print(SArel)
  ggsave(SArel, filename = paste0("RelSA_", team, "_", gameDate, ".png"), width = 11, height = 7)
  
  #reshape data for TOI plot
  toiVars <- c("Player", "evTOI", "ppTOI", "pkTOI")
  toiPlot <- teamPlot[toiVars]
  
  toiPlot <- melt(toiPlot, id=c("Player"))
  
  #reorder
  toiPlot <- toiPlot[order(toiPlot$variable, -toiPlot$value),]
  
  #subset ev toi only for ranking
  evTOIPlot <- subset(toiPlot, variable == "evTOI")
  
  #Rank
  evTOIPlot$rank <- 1:nrow(evTOIPlot)
  
  #remove everything but rank and player
  evVars <- c("Player", "rank")
  evTOIPlot <- evTOIPlot[evVars]
  
  #Join back to toi plot
  toiPlot <- dplyr::left_join(toiPlot, evTOIPlot, by = "Player")
  
  #order on rank
  toiPlot <- toiPlot[order(toiPlot$rank),]
  

  #plot
  palette <- c("grey0", "grey50", "grey80")
  toiVis <- ggplot(toiPlot, aes(reorder(factor(Player), -rank), value, fill = factor(variable, levels = c("pkTOI", "ppTOI", "evTOI")))) +
    geom_bar(colour ="black", stat = "identity") + coord_flip() +
    my_theme2 +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.background = element_rect(fill="whitesmoke")) +
    labs(title = paste0("Time on Ice Summary ", team, " ", gameDate),
         caption = "@nerdhockeyAG\nsource = NHL PBP") +
    scale_fill_manual("Strength", values = palette)
  print(toiVis)
  ggsave(toiVis, filename = paste0("Time-on-ice", team, "_", gameDate, ".png"), width = 11, height = 7)
  
}

#Faceoff plots [uses old code, worth it to fix?]

#Add Home/Away designation for loop
FAway$Team <- awayTeam
FHome$Team <- homeTeam

#Data not set up clean enough for a loop. Copying and pasting old code.
#Away
palette <- c("grey0", "grey50", "grey80")
AwayFaceoffPlot <- ggplot(FAway, aes(x=reorder(value, table(value)[value]), fill = awayzone)) + 
  geom_bar(colour = "black") + coord_flip()
AwayFaceoffPlot <- AwayFaceoffPlot + labs(y = "Faceoff Count",
                                          title = paste0("5v5 Faceoffs ", awayTeam, " ", gameDate),
                                          caption = "@nerdhockeyAG\nsource = NHL PBP") +
  my_theme2 +
  scale_fill_manual("", values = palette)
AwayFaceoffPlot
ggsave(paste0("Away-Faceoffs_", gameDate, ".png"), width = 11, height = 7)

#Home
HomeFaceoffPlot <- ggplot(FHome, aes(x=reorder(value, table(value)[value]), fill = homezone)) + 
  geom_bar(colour = "black") + coord_flip()
HomeFaceoffPlot <- HomeFaceoffPlot + labs(y = "Faceoff Count",
                                          title = paste0("5v5 Faceoffs ", homeTeam, " ", gameDate),
                                          caption = "@nerdhockeyAG\nsource = NHL PBP") +
  my_theme2 +
  scale_fill_manual("", values = palette)
HomeFaceoffPlot
ggsave(paste0("Home-Faceoffs_", gameDate, ".png"), width = 11, height = 7)


#Shot Counter
shotsDF$HomeFor <- ifelse(shotsDF$ev.team == homeTeam, 1, 0)
shotsDF$AwayFor <- ifelse(shotsDF$ev.team == awayTeam, 1, 0)

#Aggregate by minute?
shotsDF$Minute <- shotsDF$seconds / 60

#cumsum shot attempts
shotsDF$homeShots <- cumsum(shotsDF$HomeFor)
shotsDF$awayShots <- cumsum(shotsDF$AwayFor)

#Etra code, but cleaner plot, subset goals by team
homeGoals <- subset(shotsDF, etype == "GOAL" & ev.team == homeTeam)
awayGoals <- subset(shotsDF, etype == "GOAL" & ev.team == awayTeam)

#Gross plot but should work
trendedShotsPlot <- ggplot(shotsDF, aes(Minute, homeShots)) + geom_step(aes(colour = "red")) +
  geom_step(data = shotsDF, aes(Minute, awayShots, colour = "blue")) +
  geom_point(data = homeGoals, aes(Minute, homeShots), colour = "red", size = 4) +
  geom_point(data = awayGoals, aes(Minute, awayShots), colour = "blue", size = 4) +
  scale_colour_manual(name = 'Team', 
                      values =c('red'='red','blue'='blue'), labels = c(awayTeam, homeTeam)) +
  geom_vline(xintercept = c(20, 40, 60)) +
  my_theme2 +
  labs(y = "Shot Count",
       title = paste0(homeTeam, " vs ", awayTeam, " Trended Shots, ", gameDate),
       subtitle = "All Situations",
       caption = "@nerdhockeyAG\nSource = NHL PBP") 
trendedShotsPlot
ggsave(paste0("Trended-Shots_", gameDate, ".png"), width = 11, height = 7)

#Heatmap plot
#Subset goals

goalsPlot <- subset(ShotChart, eventTypeId == "GOAL")
ShotChart <- subset(ShotChart, eventTypeId != "GOAL")

#Remove shootout goals if applicable
goalsPlot <-  subset(goalsPlot, a1 != "")

#Plot
image <- png::readPNG("Rink.png")
ShotPlot <- ggplot(ShotChart, aes(xcoord2, ycoord2)) + 
  annotation_custom(rasterGrob(image, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(data = ShotChart, aes(xcoord2, ycoord2, shape = ev.team), size = 4, alpha = 0.8) +
  geom_point(data = goalsPlot, aes(xcoord2, ycoord2, shape = ev.team), size = 4, colour = "green") +
  scale_x_continuous(breaks=seq(-100, 100, 20)) +
  scale_y_continuous(breaks=seq(-55, 55, 10)) +
  guides(size = FALSE, colour = FALSE) +
  labs(title = paste0("All Situations Shot Map ", awayTeam, " @ ", homeTeam, " ", gameDate),
       subtitle = "No Blocked Shots",
       caption = "@nerdhockeyAG\nSource = NHL PBP") +
  theme(legend.position = "bottom")
ShotPlot
ggsave(paste0("Shot-Location-Chart_", gcode, ".png"), width = 11, height = 7)
