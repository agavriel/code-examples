#Clear Environment
rm(list=ls())

library(nhlscrapr)
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lazyeval)
library(ggrepel)
library(reshape)
library(pacman)
library(png)
library(grid)
library(XML)
setwd("~/Documents/R/NHLPBP")


#Begin Code#

#Game ID from NHL.com
GameID <- 20360
Date <- "12/3/2016"

#Store read List file into object
List <- process.single.game (season="20162017",
                     gcode=GameID,
                     rdata.folder="nhlr-data",
                     verbose=TRUE)

#Convert List to Data Frame
PBP2 <-  rbindlist(lapply(List, as.data.table), fill = TRUE)

#Remove "Change"
PBP <- subset(PBP2, etype != "CHANGE")

########BEGIN SHOT ATTEMPT CALCS##########

#5v5 Corsi
EV <- subset(PBP, home.skaters == 6 & away.skaters == 6)
#Corsi
EV <- subset(EV, etype == "BLOCK" |
               etype == "SHOT" |
               etype == "MISS" |
               etype == "GOAL")
#Removes Empty Net extra attackers
EV <- subset(EV, a6 == "" & h6 == "")
EV$a6 <- unique(EV$awayteam)
EV$h6 <- unique(EV$hometeam)

#Remove excess cols
EV <- subset(EV, select = -c(38:45) )

#Home & Away Shots
EV$HomeFor <- ifelse(EV$ev.team == EV$hometeam, 1, 0)
EV$AwayFor <- ifelse(EV$ev.team == EV$awayteam, 1, 0)

#Store Rangers Shots
NYRFor <- sum(ifelse(EV$ev.team == "NYR", 1, 0))
NYRAgainst <- sum(ifelse(EV$ev.team != "NYR", 1, 0))

#Manual Aggregate for Away Team
a1 <- aggregate(HomeFor ~ a1, data = EV, sum)
a2 <- aggregate(HomeFor ~ a2, data = EV, sum)
a3 <- aggregate(HomeFor ~ a3, data = EV, sum)
a4 <- aggregate(HomeFor ~ a4, data = EV, sum)
a5 <- aggregate(HomeFor ~ a5, data = EV, sum)
a6 <- aggregate(HomeFor ~ a6, data = EV, sum)

a12 <- aggregate(AwayFor ~ a1, data = EV, sum)
a22 <- aggregate(AwayFor ~ a2, data = EV, sum)
a32 <- aggregate(AwayFor ~ a3, data = EV, sum)
a42 <- aggregate(AwayFor ~ a4, data = EV, sum)
a52 <- aggregate(AwayFor ~ a5, data = EV, sum)
a62 <- aggregate(AwayFor ~ a6, data = EV, sum)

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
AwayPlot <- subset(Away, Player != unique(EV$awayteam))

#Manual Aggregate for Home Team
h1 <- aggregate(HomeFor ~ h1, data = EV, sum)
h2 <- aggregate(HomeFor ~ h2, data = EV, sum)
h3 <- aggregate(HomeFor ~ h3, data = EV, sum)
h4 <- aggregate(HomeFor ~ h4, data = EV, sum)
h5 <- aggregate(HomeFor ~ h5, data = EV, sum)
h6 <- aggregate(HomeFor ~ h6, data = EV, sum)

h12 <- aggregate(AwayFor ~ h1, data = EV, sum)
h22 <- aggregate(AwayFor ~ h2, data = EV, sum)
h32 <- aggregate(AwayFor ~ h3, data = EV, sum)
h42 <- aggregate(AwayFor ~ h4, data = EV, sum)
h52 <- aggregate(AwayFor ~ h5, data = EV, sum)
h62 <- aggregate(AwayFor ~ h6, data = EV, sum)

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
HomePlot <- subset(Home, Player != unique(EV$hometeam))

####Clean up Environment#####
rm(a1, a12, a2, a22, a3, a32, a4, a42, a5, a52, a6, a62, h1, h12, h2, h22, h3, h32, h4, h42, h5, h52, h6, h62)

#Begin TOI Automation

#Read In Game Event Summary
TOI2 <- readHTMLTable(paste0("http://www.nhl.com/scores/htmlreports/20162017/ES0",GameID,".HTM"),
                      header = T,
                      which = 1)

TOI <- subset(TOI2, V2 == "C" |
                V2 == "R" |
                V2 == "D" |
                V2 == "L")

timevars <- c("V1", "V3", "V15")

EVTOI <- TOI[timevars]

#Delimit
EVTOI <- separate(data = EVTOI, col = V3, into = c("Last", "First"), sep = ",")

#Trim
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
EVTOI$First <- trim(EVTOI$First)
EVTOI$Last <- trim(EVTOI$Last)
EVTOI$V1 <- trim(EVTOI$V1)

#Concatenate to PBP Format
EVTOI$Player <- paste(EVTOI$V1, EVTOI$First, EVTOI$Last)

#Subset player and EV TOI
timevars <- c("Player", "V15")
EVTOI <- EVTOI[timevars]

#Convert MM:SS to decimal (Need to make this cleaner)
EVTOI$V15 <- as.character(EVTOI$V15)

#Delimit on ":"
EVTOI <- separate(data = EVTOI, col = V15, into = c("Minutes", "Seconds"), sep = ":")

#As Numeric
EVTOI$Minutes <- as.numeric(EVTOI$Minutes)
EVTOI$Seconds <- as.numeric(EVTOI$Seconds)

#Convert Seconds to decimal
EVTOI$Seconds <- (EVTOI$Seconds*100/60)

#Round
EVTOI$Seconds <- round(EVTOI$Seconds, digits = 0)

#Paste into decimal format as numeric
EVTOI$EVTOI <- paste0(EVTOI$Minutes, ".", EVTOI$Seconds)

#As Numeric
EVTOI$EVTOI <- as.numeric(EVTOI$EVTOI)

#Subset
timevars <- c("Player", "EVTOI")
EVTOI <- EVTOI[timevars]

#Trim
AwayPlot$Player <- trim(AwayPlot$Player)
HomePlot$Player <- trim(HomePlot$Player)

#Bind
AwayPlot <- dplyr::inner_join(AwayPlot, EVTOI, by = "Player")
HomePlot <- dplyr::inner_join(HomePlot, EVTOI, by = "Player")

#Calculate per 60
AwayPlot$AwayFor60 <- (AwayPlot$AwayFor*60/AwayPlot$EVTOI)
AwayPlot$HomeFor60 <- (AwayPlot$HomeFor*60/AwayPlot$EVTOI)
HomePlot$HomeFor60 <- (HomePlot$HomeFor*60/HomePlot$EVTOI)
HomePlot$AwayFor60 <- (HomePlot$AwayFor*60/HomePlot$EVTOI)

#Minimums for Plot Home Team
hYMin <- min(HomePlot$AwayFor60) - 2
hYMax <- max(HomePlot$AwayFor60) + 2
hXMin <- min(HomePlot$HomeFor60) - 2
hXMax <- max(HomePlot$HomeFor60) + 2

#Minimums for Plot Away Team
aYMin <- min(AwayPlot$HomeFor60) - 2
aYMax <- max(AwayPlot$HomeFor60) + 2
aXMin <- min(AwayPlot$AwayFor60) - 2
aXMax <- max(AwayPlot$AwayFor60) + 2

#Begin relative shot attempt % calcs
##Store Home and away shots for
AwayShots <- sum(EV$AwayFor)
HomeShots <- sum(EV$HomeFor)

##Away Team
#Calculate Off Ice Totals
Away$OIF <- AwayShots - Away$AwayFor
Away$OIA <- HomeShots - Away$HomeFor

#Calculate Player %
Away$SAper <- Away$AwayFor / (Away$AwayFor + Away$HomeFor)

#Calculate Off Ice %
Away$OISAper <- Away$OIF / (Away$OIF + Away$OIA)

#Calculate Realtive
Away$relSAper <- Away$SAper - Away$OISAper

##Home Team
#Calculate Off Ice Totals
Home$OIF <- HomeShots - Home$HomeFor
Home$OIA <- AwayShots - Home$AwayFor

#Calculate Player %
Home$SAper <- Home$HomeFor / (Home$HomeFor + Home$AwayFor)

#Calculate Off Ice %
Home$OISAper <- Home$OIF / (Home$OIF + Home$OIA)

#Calculate Realtive
Home$relSAper <- Home$SAper - Home$OISAper

########END SHOT ATTEMPT CALCS##########

########BEGIN ZONE START SUMMARY##########

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

#############SHOT CHART LOCATION START#################

#Subset for Goals, Shots

ShotChart <- subset(PBP, etype == "SHOT" |
                      etype == "GOAL")

#Set Coords as numeric
ShotChart$xcoord <- as.numeric(ShotChart$xcoord)
ShotChart$ycoord <- as.numeric(ShotChart$ycoord)

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
ShotChart$ycoord2 <- ifelse(ShotChart$ev.team == ShotChart$hometeam,
                            ifelse(ShotChart$period == 2,
                                   ShotChart$ycoord, 
                                   ShotChart$ycoord * -1),
                            ShotChart$ycoord)

#YCoord Away Team
ShotChart$ycoord2 <- ifelse(ShotChart$ev.team == ShotChart$awayteam,
                            ifelse(ShotChart$period == 2,
                                   ShotChart$ycoord,
                                   ShotChart$ycoord * -1),
                            ShotChart$ycoord2)

#Subset Goals
GoalChart <- subset(ShotChart, etype == "GOAL")
ShotChart <- subset(ShotChart, etype != "GOAL")

#############Shots Trended Over Time, All Situations#################

#Subset all shots

AllShots <- subset(PBP2, PBP2$etype == "GOAL" |
                     PBP2$etype == "SHOT" |
                     PBP2$etype == "MISS" |
                     PBP2$etype == "BLOCK")

#Convert Seconds to "MM.SS"
AllShots$Time <- as.numeric(AllShots$seconds/60)

#Shot Counter
AllShots$HomeCount <- cumsum(AllShots$ev.team == unique(AllShots$hometeam))
AllShots$AwayCount <- cumsum(AllShots$ev.team == unique(AllShots$awayteam))

#Subset on Teams
AwayTrend <- subset(AllShots, AllShots$ev.team == unique(AllShots$awayteam))
HomeTrend <- subset(AllShots, AllShots$ev.team == unique(AllShots$hometeam))

#Rename Cols to match for plot
AwayTrend <- plyr::rename(AwayTrend, c("AwayCount" = "Shots"))
HomeTrend <- plyr::rename(HomeTrend, c("HomeCount" = "Shots"))

#Subset Goals for Plot
GoalsTrend <- subset(AllShots, AllShots$etype == "GOAL")

#Subset on Teams
AwayGoals <- subset(GoalsTrend, GoalsTrend$ev.team == unique(GoalsTrend$awayteam))
HomeGoals <- subset(GoalsTrend, GoalsTrend$ev.team == unique(GoalsTrend$hometeam))


#############Plots#################

#Title for Charts
title <- paste(unique(EV$awayteam), "vs", unique(EV$hometeam), Date, "5v5 Only")
title2 <- paste(unique(EV$hometeam), "vs", unique(EV$awayteam), Date, "All Situations, No Missed Shots or Blocks")
title3 <- paste(unique(EV$hometeam), "vs", unique(EV$awayteam), Date, "Trended Shots, All Situations")

#Plot Away Shot Attempts
AwayShotsPlot <- ggplot(AwayPlot, aes(AwayFor60, HomeFor60)) + geom_label_repel(aes(label = Player)) + 
  geom_abline(intercept = 0, slope = -1, colour = "grey50", linetype = "dashed") +
  geom_point(data = AwayPlot, aes(size = EVTOI)) +
  scale_y_continuous(trans = "reverse") + 
  scale_size_continuous("TOI") +
  annotate("text", x = aXMin, y = aYMin, label = "Bad O\nGood D", colour = "red", size = 3) +
  annotate("text", x = aXMax, y = aYMin, label = "Good O\nGood D", colour = "red", size = 3) +
  annotate("text", x = aXMin, y = aYMax, label = "Bad O\nBad D", colour = "red", size = 3) +
  annotate("text", x = aXMax, y = aYMax, label = "Good O\n Bad D", colour = "red", size = 3) +
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
  labs(x = "Shot Attempts For per 60",
       y = "Shot Attempts Against per 60\n(Inverted)",
       title = title)
AwayShotsPlot
ggsave(paste0("Away-Scatter-Shots_", GameID, ".png"), width = 11, height = 7)

#Plot Home Shot Attempts
HomeShotsPlot <- ggplot(HomePlot, aes(HomeFor60, AwayFor60)) + geom_label_repel(aes(label = Player)) + 
  geom_abline(intercept = 0, slope = -1, colour = "grey50", linetype = "dashed") +
  geom_point(data = HomePlot, aes(size = EVTOI)) +
  scale_y_continuous(trans = "reverse") + 
  scale_size_continuous("TOI") +
  annotate("text", x = hXMin, y = hYMin, label = "Bad O\nGood D", colour = "red", size = 3) +
  annotate("text", x = hXMax, y = hYMin, label = "Good O\nGood D", colour = "red", size = 3) +
  annotate("text", x = hXMin, y = hYMax, label = "Bad O\nBad D", colour = "red", size = 3) +
  annotate("text", x = hXMax, y = hYMax, label = "Good O\n Bad D", colour = "red", size = 3) +
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
  labs(x = "Shot Attempts For per 60",
       y = "Shot Attempts Against per 60\n(Inverted)",
       title = title)
HomeShotsPlot
ggsave(paste0("Home-Scatter-Shots_", GameID, ".png"), width = 11, height = 7)


#Relative Shot Attempt % Charts
#Away Team
##Remove Team
AwaySAPlot <- subset(Away, OIF != 0)


AwaySArel <- ggplot(AwaySAPlot, aes(reorder(factor(Player), relSAper), relSAper)) + 
  geom_bar(stat="identity") +
  coord_flip() + 
  labs(y = "Relative Shot Attempt Percentage",
       title = title) +
  theme(axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
AwaySArel
ggsave(paste0("Away-Shot-Attempt-Percentage_", GameID, ".png"), width = 11, height = 7)

#Home Team
##Remove Team
HomeSAPlot <- subset(Home, OIF != 0)

HomeSArel <- ggplot(HomeSAPlot, aes(reorder(factor(Player), relSAper), relSAper)) + 
  geom_bar(stat="identity") +
  coord_flip() + 
  labs(y = "Relative Shot Attempt Percentage",
       title = title) +
  theme(axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
HomeSArel
ggsave(paste0("Home-Shot-Attempt-Percentage_", GameID, ".png"), width = 11, height = 7)

####Faceoff Charts

#Away
palette <- c("grey0", "grey50", "grey80")
AwayFaceoffPlot <- ggplot(FAway, aes(x=reorder(value, table(value)[value]), fill = awayzone)) + 
  geom_bar() + coord_flip()
AwayFaceoffPlot <- AwayFaceoffPlot + labs(y = "Faceoff Count",
                                          title = title) +
  theme(axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom") +
  scale_fill_manual("", values = palette)
AwayFaceoffPlot
ggsave(paste0("Away-Faceoffs_", GameID, ".png"), width = 11, height = 7)

#Home
FHome <- FHome[with(FHome, order(value, homezone)),]
HomeFaceoffPlot <- ggplot(FHome, aes(value, fill = homezone)) +
  geom_bar() + coord_flip()
HomeFaceoffPlot <- HomeFaceoffPlot + labs(y = "Faceoff Count",
                                          title = title) +
  theme(axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom") +
  scale_fill_manual("", values = palette)
HomeFaceoffPlot
ggsave(paste0("Home-Faceoffs_", GameID, ".png"), width = 11, height = 7)


####Shot Location Chart
image <- png::readPNG("Rink.png")
ShotPlot <- ggplot(ShotChart, aes(xcoord2, ycoord2)) + 
  annotation_custom(rasterGrob(image, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -100, 100, -Inf, Inf) +
  geom_point(aes(shape = factor(ev.team), size = 2)) +
  geom_point(data=GoalChart, aes(xcoord2, ycoord2, colour = etype, shape = factor(ev.team), size = 2)) +
  scale_x_continuous(breaks=seq(-100, 100, 20)) +
  scale_y_continuous(breaks=seq(-55, 55, 10)) +
  scale_colour_manual("", values = c("GOAL" = "green")) +
  guides(size = FALSE, colour = FALSE) +
  scale_shape_discrete("") +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = title2)
ShotPlot
ggsave(paste0("Shot-Location-Chart_", GameID, ".png"), width = 11, height = 7)

####Trended Shots Chart
trendplot <- ggplot(AwayTrend, aes(Time, Shots)) + geom_line(aes(colour=ev.team)) +
  geom_point(data=AwayTrend[AwayTrend$etype == "GOAL",], aes(colour=ev.team), size=4) +
  geom_line(data=HomeTrend, aes(colour=ev.team)) + 
  geom_point(data=HomeTrend[HomeTrend$etype == "GOAL",], aes(colour=ev.team), size=4) +
  labs(y = "All Shot Attempts",
       title = title3) +
  scale_colour_manual(values = c("red", "blue"), name = "") +
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom")
trendplot
ggsave(paste0("Trended-Shots_", GameID, ".png"), width = 11, height = 7)

