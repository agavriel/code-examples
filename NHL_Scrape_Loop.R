library(jsonlite)
library(data.table)
library(plyr)
library(dplyr)
library(nhlscrapr)
library(ggplot2)

#GameIDs for season
GameIDs <- 20001:21230

#drop broken games
#20047 no json pbp
#20070 no json pbp
broken <- c(20047, 20070, 20120, 
            20168, 20275, 20286, 
            20289, 20389, 20477, 
            20496, 20497, 20567, 
            20628, 20639, 20726,
            20766, 20777, 20822,
            20831, 20924, 20929,
            21096, 21110, 21146,
            21176, 21182)

GameIDs <- GameIDs[! GameIDs %in% broken]

#Empty List
dataList <- list()

for(GameID in GameIDs) {

jsonWebPage <- paste0("https://statsapi.web.nhl.com/api/v1/game/20160", GameID, "/feed/live?site=en_nhl")
pbplist <- fromJSON(jsonWebPage, simplifyDataFrame = TRUE)

gameData <- pbplist[5]
liveData <- pbplist[6]

#Can make subsets of list their own object
plays <- liveData$liveData$plays

#Data frame for events
result <- plays$allPlays$result

#apply identifier for future merging
result$rownumber <- 1:nrow(result)

#Drop strength columns. Unhelpful, and is not 1D list. breaks merging.
result <- subset(result, select = -strength)

#Data frame for event description
about <- plays$allPlays$about

#apply identifier for merging
about$rownumber <- 1:nrow(about)

#Drop goals columns. Is not 1D. breaks merging.
about <- subset(about, select = -goals)

#merge
jsondf <- dplyr::left_join(result, about, by = "rownumber")

#Data frame for coordinates
coordinates <- plays$allPlays$coordinates

#apply identifier for merging
coordinates$rownumber <- 1:nrow(coordinates)

#merge
jsondf <- dplyr::left_join(jsondf, coordinates, by = "rownumber")


#Store read List file into object
List <- process.single.game (season="20162017",
                             gcode=GameID,
                             rdata.folder="nhlr-data",
                             verbose=TRUE,
                             wait=0)

#Convert List to Data Frame
PBP2 <-  rbindlist(lapply(List, as.data.table), fill = TRUE)

#Remove "Change" or Period end
PBP <- subset(PBP2, etype != "CHANGE" & etype != "PEND")

#From JSON file, remove line-items not in NHL Scrape
jsondf <- jsondf[!jsondf$event %in% c("Game Scheduled", "Period Ready", "Period Start", "Stoppage", "Period End", "Period Official", "Game End"),]
jsondf$rownumber <- 1:nrow(jsondf)

#put coordinates from JSON on to nhlscrapr df
PBP$rownumber <- 1:nrow(PBP)

#Select only necessary cols
jsondfMerge <- subset(jsondf, select = c(rownumber, x, y))

#Merge
pbpDone <- dplyr::left_join(PBP, jsondfMerge, by = "rownumber")

#Drop excess cols
pbpDone <- subset(pbpDone, select = -c(number, pos, lastfirst, last, first, numlast, numfirstlast, V1))

#Append to list
dataList[[GameID]] <- pbpDone

}

#Create data frame from Loop list
bigdata <- do.call(rbind, dataList)