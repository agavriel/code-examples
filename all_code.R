Week <- 11 #Remember week two to add logic to append to current data set, not replace
Segment <- 733 #fantasy sharks url specific
Season <- 2021

library(rvest)
library(data.table)
library(tidyverse)
library(lpSolve)
library(nflfastR)
library(RcppRoll)

##Mirror Excel TRIM
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Read in last week's data
last_week <- readRDS("~/Code/base_scrapes/player_projections.rds")

#Remove current week if re-run
last_week$remove_check <- ifelse(last_week$Week == Week, 1, 0)
last_week <- last_week %>% 
  filter(remove_check != 1) %>% 
  plyr::rename(c("Actual Points" = "Actual_Points")) %>% 
  #Remove all the columns that our mapping files merge
  #Can we figure out a way to 
  select(-remove_check, -Ownership, -Actual_Points)
         

#Save current player_projections as last week to archive
#save rds file to read in for next week
saveRDS(last_week, file = paste0("player_projections_", Week-1, ".rds"))
#Save to shiny app directory as well
setwd("~/Code/nfl_dfs_app")
saveRDS(last_week, file = paste0("player_projections_", Week-1, ".rds"))

#Reverse wd
setwd("~/Code/base_scrapes")

#Merge draftkings actual results (actual points and ownership)
last_week_dk <- read.csv("last_week_gpp.csv", stringsAsFactors = FALSE)
#last_week_dk$Week <- Week-1
last_week_dk <- last_week_dk %>% 
  select(Week, Player, X.Drafted, FPTS) %>% 
  na.omit() %>% 
  plyr::rename(c("Player" = "Name", "X.Drafted" = "Ownership", "FPTS" = "Actual Points")) %>% 
  mutate(Ownership = as.numeric(sub("%","", Ownership))/100)

#Merge to last week player file
last_week <- dplyr::left_join(
  last_week, last_week_dk, by = c("Name", "Week"))

# air yards ---------------------------------------------------------------

games_2020 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2021.rds'))
air_yards <- games_2020 %>% 
  filter(play_type == "pass") %>% 
  select(week, posteam, air_yards, passer_player_id,
         passer_player_name, receiver_player_id, receiver_player_name) %>% 
  na.omit()

#Add count column for attempts
air_yards$attempt <- 1

#Aggregate by team
air_yards_team <- air_yards %>% 
  group_by(posteam, week) %>% 
  summarize_each(sum, air_yards, attempt)

#Aggregate  by QB
air_yards_passer <- air_yards %>% 
  group_by(passer_player_id, passer_player_name, posteam, week) %>% 
  summarize_each(sum, air_yards, attempt)

air_yards_passer <- plyr::rename(air_yards_passer,
                                   c("passer_player_id" = "ayID",
                                     "passer_player_name" = "ayName"))

air_yards_passer$ADOT <- round(air_yards_passer$air_yards / air_yards_passer$attempt ,2)

#Aggregate by Receiver
air_yards_receiver <- air_yards %>% 
  group_by(receiver_player_id, receiver_player_name, posteam, week) %>% 
  summarize_each(sum, air_yards, attempt)

air_yards_receiver <- plyr::rename(air_yards_receiver,
                                   c("receiver_player_id" = "ayID",
                                     "receiver_player_name" = "ayName"))

air_yards_receiver$ADOT <- round(air_yards_receiver$air_yards / air_yards_receiver$attempt, 2)

#Merge team data for share
air_yards_receiver <- air_yards_receiver %>% 
  left_join(air_yards_team, by = c("posteam", "week"))

air_yards_receiver$ay_share <-
  round(air_yards_receiver$air_yards.x / air_yards_receiver$air_yards.y, 2)

air_yards_receiver$tar_share <-
  round(air_yards_receiver$attempt.x / air_yards_receiver$attempt.y, 2)

#Keep player and ADOT and row_bind
air_yards_passer <- air_yards_passer %>% 
  ungroup() %>% 
  select(week, ayID, ayName, posteam, attempt, air_yards, ADOT)

air_yards_receiver <- air_yards_receiver %>% 
  plyr::rename(c("air_yards.x" = "air_yards",
                 "attempt.x" = "attempt")) %>% 
  ungroup() %>% 
  select(week, ayID, ayName, posteam, attempt, air_yards, ADOT, ay_share, tar_share)

air_yards_players <- dplyr::bind_rows(air_yards_passer, air_yards_receiver)

#Rolling three weeks team
air_yards_team <- air_yards_team %>% 
  arrange(week, posteam) %>% 
  group_by(posteam) %>% 
  mutate(roll_tm_attempts = roll_sum(attempt, 3, align = "right", fill = NA)) %>% 
  ungroup()

#3 game cumulative sum metrics
air_yards_players <- air_yards_players %>%
  arrange(week, ayID, ayName) %>%
  group_by(ayID, ayName, posteam) %>%
  dplyr::left_join(select(air_yards_team, week, posteam, roll_tm_attempts), by = c("posteam", "week")) %>% 
  mutate(roll_attempts = roll_sum(attempt, 3, align = "right", fill = NA)) %>%
  mutate(roll_ay = roll_sum(air_yards, 3, align = "right", fill = NA)) %>% 
  mutate(roll_adot = roll_ay/roll_attempts) %>%
  mutate(roll_tar_share = roll_attempts / roll_tm_attempts) %>% 
  ungroup()

#air_yards_players$Week <- Week - 1

#Hold 'til data cleanse


# Fantasy Pros ------------------------------------------------------------


#QBs

fpqbs <- read_html(paste0("https://www.fantasypros.com/nfl/projections/qb.php?week=", Week))
fpqbs <- fpqbs %>% html_table(fill=TRUE)
fp_qbs <-  rbindlist(lapply(fpqbs, as.data.table), fill = TRUE)

#Remove excess rows
fp_qbs <- fp_qbs[-c(1:2), ]

#Remove excess cols
fp_qbs <- fp_qbs[, -c(12:15)]

#Remove NAs
fp_qbs <- na.omit(fp_qbs)

#Rename columns
fp_qbs <- plyr::rename(fp_qbs, c("X1" = "Name",
                                   "X2" = "Pass.Attempts",
                                   "X3" = "Completions",
                                   "X4" = "Pass.Yards",
                                   "X5" = "Pass.TDs",
                                   "X6" = "Interceptions",
                                   "X7" = "RshAtt",
                                   "X8" = "RshYds",
                                   "X9" = "RshTDs",
                                   "X10" = "Fumbles",
                                   "X11" = "Points"))

#Start cleaning player name to remove team abbreviation
#Substring function

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Split team to own column
fp_qbs$Team <- substrRight(fp_qbs$Name, 3)

#Trim new team column
fp_qbs$Team <- trim(fp_qbs$Team)

#Remove team from player name then trim
fp_qbs$Name <- substr(fp_qbs$Name, 1, nchar(fp_qbs$Name)-3)
fp_qbs$Name <- trim(fp_qbs$Name)

#Add position as factor
fp_qbs$position <- "QB"

#As numeric for days
fp_qbs$Pass.Attempts <- as.numeric(fp_qbs$Pass.Attempts)
fp_qbs$Completions <- as.numeric(fp_qbs$Completions)
fp_qbs$Pass.Yards <- as.numeric(fp_qbs$Pass.Yards)
fp_qbs$Pass.TDs <- as.numeric(fp_qbs$Pass.TDs)
fp_qbs$Interceptions <- as.numeric(fp_qbs$Interceptions)
fp_qbs$RshAtt <- as.numeric(fp_qbs$RshAtt)
fp_qbs$RshYds <- as.numeric(fp_qbs$RshYds)
fp_qbs$RshTDs <- as.numeric(fp_qbs$RshTDs)
fp_qbs$Fumbles <- as.numeric(fp_qbs$Fumbles)


#Calculate DK Points (this will be the only column we keep)
fp_qbs$DKPoints <- fp_qbs$Pass.TDs*4 +
  fp_qbs$Pass.Yards*0.04 +
  ifelse(fp_qbs$Pass.Yards >=300, 3, 0) +
  fp_qbs$Interceptions*-1 +
  fp_qbs$RshTDs*6 +
  fp_qbs$RshYds *0.1 +
  ifelse(fp_qbs$RshYds >= 100, 3, 0) +
  fp_qbs$Fumbles * -1


#Running Backs

fprbs <- read_html(paste0("https://www.fantasypros.com/nfl/projections/rb.php?week=", Week))
fprbs <- fprbs %>% html_table(fill=TRUE)
fp_rbs <-  rbindlist(lapply(fprbs, as.data.table), fill = TRUE)

#Remove excess rows
fp_rbs <- fp_rbs[-c(1:2), ]

#Remove excess cols
fp_rbs <- fp_rbs[, -c(10:13)]

#Remove NAs
fp_rbs <- na.omit(fp_rbs)

#Rename columns
fp_rbs <- plyr::rename(fp_rbs, c("X1" = "Name",
                                 "X2" = "RshAtt",
                                 "X3" = "RshYds",
                                 "X4" = "RshTDs",
                                 "X5" = "Receptions",
                                 "X6" = "RecYds",
                                 "X7" = "RecTDs",
                                 "X8" = "Fumbles",
                                 "X9" = "Points"))

#Start cleaning player name to remove team abbreviation
#Substring function

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Split team to own column
fp_rbs$Team <- substrRight(fp_rbs$Name, 3)

#Trim new team column
fp_rbs$Team <- trim(fp_rbs$Team)

#Remove team from player name then trim
fp_rbs$Name <- substr(fp_rbs$Name, 1, nchar(fp_rbs$Name)-3)
fp_rbs$Name <- trim(fp_rbs$Name)

#Add position as factor
fp_rbs$position <- "RB"

#As numeric for days
fp_rbs$RshAtt <- as.numeric(fp_rbs$RshAtt)
fp_rbs$RshYds <- as.numeric(fp_rbs$RshYds)
fp_rbs$RshTDs <- as.numeric(fp_rbs$RshTDs)
fp_rbs$Receptions <- as.numeric(fp_rbs$Receptions)
fp_rbs$RecYds <- as.numeric(fp_rbs$RecYds)
fp_rbs$RecAvg <- as.numeric(fp_rbs$RecAvg)
fp_rbs$RecTDs <- as.numeric(fp_rbs$RecTDs)
fp_rbs$Fumbles <- as.numeric(fp_rbs$Fumbles)


#Calculate DK Points (this will be the only column we keep)
fp_rbs$DKPoints <- fp_rbs$RshTDs*6 +
  fp_rbs$RshYds *0.1 +
  ifelse(fp_rbs$RshYds >= 100, 3, 0) +
  fp_rbs$RecTDs * 6 +
  fp_rbs$RecYds * 0.1 +
  ifelse(fp_rbs$RecYds >= 100, 3, 0) +
  fp_rbs$Receptions +
  fp_rbs$Fumbles * -1



#WRs

fpwrs <- read_html(paste0("https://www.fantasypros.com/nfl/projections/wr.php?week=", Week))
fpwrs <- fpwrs %>% html_table(fill=TRUE)
fp_wrs <-  rbindlist(lapply(fpwrs, as.data.table), fill = TRUE)

#Remove excess rows
fp_wrs <- fp_wrs[-c(1:2), ]

#Remove excess cols
fp_wrs <- fp_wrs[, -c(10:13)]

#Remove NAs
fp_wrs <- na.omit(fp_wrs)

#Rename columns
fp_wrs <- plyr::rename(fp_wrs, c("X1" = "Name",
                                 "X5" = "RshAtt",
                                 "X6" = "RshYds",
                                 "X7" = "RshTDs",
                                 "X2" = "Receptions",
                                 "X3" = "RecYds",
                                 "X4" = "RecTDs",
                                 "X8" = "Fumbles",
                                 "X9" = "Points"))

#Start cleaning player name to remove team abbreviation
#Substring function

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Split team to own column
fp_wrs$Team <- substrRight(fp_wrs$Name, 3)

#Trim new team column
fp_wrs$Team <- trim(fp_wrs$Team)

#Remove team from player name then trim
fp_wrs$Name <- substr(fp_wrs$Name, 1, nchar(fp_wrs$Name)-3)
fp_wrs$Name <- trim(fp_wrs$Name)

#Add position as factor
fp_wrs$position <- "WR"

#As numeric for days
fp_wrs$RshAtt <- as.numeric(fp_wrs$RshAtt)
fp_wrs$RshYds <- as.numeric(fp_wrs$RshYds)
fp_wrs$RshTDs <- as.numeric(fp_wrs$RshTDs)
fp_wrs$Receptions <- as.numeric(fp_wrs$Receptions)
fp_wrs$RecYds <- as.numeric(fp_wrs$RecYds)
fp_wrs$RecAvg <- as.numeric(fp_wrs$RecAvg)
fp_wrs$RecTDs <- as.numeric(fp_wrs$RecTDs)
fp_wrs$Fumbles <- as.numeric(fp_wrs$Fumbles)


#Calculate DK Points (this will be the only column we keep)
fp_wrs$DKPoints <- fp_wrs$RshTDs*6 +
  fp_wrs$RshYds *0.1 +
  ifelse(fp_wrs$RshYds >= 100, 3, 0) +
  fp_wrs$RecTDs * 6 +
  fp_wrs$RecYds * 0.1 +
  ifelse(fp_wrs$RecYds >= 100, 3, 0) +
  fp_wrs$Receptions +
  fp_wrs$Fumbles * -1



#TEs

fptes <- read_html(paste0("https://www.fantasypros.com/nfl/projections/te.php?week=", Week))
fptes <- fptes %>% html_table(fill=TRUE)
fp_tes <-  rbindlist(lapply(fptes, as.data.table), fill = TRUE)

#Remove excess rows
fp_tes <- fp_tes[-c(1:2), ]

#Remove excess cols
fp_tes <- fp_tes[, -c(7:10)]

#Remove NAs
fp_tes <- na.omit(fp_tes)

#Rename columns
fp_tes <- plyr::rename(fp_tes, c("X1" = "Name",
                                 "X2" = "Receptions",
                                 "X3" = "RecYds",
                                 "X4" = "RecTDs",
                                 "X5" = "Fumbles",
                                 "X6" = "Points"))

#Start cleaning player name to remove team abbreviation
#Substring function

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Split team to own column
fp_tes$Team <- substrRight(fp_tes$Name, 3)

#Trim new team column
fp_tes$Team <- trim(fp_tes$Team)

#Remove team from player name then trim
fp_tes$Name <- substr(fp_tes$Name, 1, nchar(fp_tes$Name)-3)
fp_tes$Name <- trim(fp_tes$Name)

#Add position as factor
fp_tes$position <- "TE"

#As numeric for days
fp_tes$Receptions <- as.numeric(fp_tes$Receptions)
fp_tes$RecYds <- as.numeric(fp_tes$RecYds)
fp_tes$RecTDs <- as.numeric(fp_tes$RecTDs)
fp_tes$Fumbles <- as.numeric(fp_tes$Fumbles)


#Calculate DK Points (this will be the only column we keep)
fp_tes$DKPoints <- fp_tes$RecTDs * 6 +
  fp_tes$RecYds * 0.1 +
  ifelse(fp_tes$RecYds >= 100, 3, 0) +
  fp_tes$Receptions +
  fp_tes$Fumbles * -1

#Remove Taysom Hill, TE
fp_tes <- subset(fptes, Name != "Taysom Hill")

#Rename "DK Points" to "FP Points" - stupid oversight just fix now

fp_qbs <- plyr::rename(fp_qbs, c("DKPoints" = "FPPoints"))
fp_rbs <- plyr::rename(fp_rbs, c("DKPoints" = "FPPoints"))
fp_wrs <- plyr::rename(fp_wrs, c("DKPoints" = "FPPoints"))
fp_tes <- plyr::rename(fp_tes, c("DKPoints" = "FPPoints"))

#Create merge files that only have player name, position, and FP points
fpQBsMerge <- fp_qbs[, c(1, 12:14)]
fpRBsMerge <- fp_rbs[, c(1, 10, 11, 13)]
fpWRsMerge <- fp_wrs[, c(1, 10, 11, 13)]
fpTEsMerge <- fp_tes[, c(1, 7:9)]

#Bind time
fp_dk <- dplyr::bind_rows(fpQBsMerge, fpRBsMerge)
fp_dk <- dplyr::bind_rows(fp_dk, fpWRsMerge)
fp_dk <- dplyr::bind_rows(fp_dk, fpTEsMerge)

#Rename players (edit 2020 - try to take care of this in player mapping w index)
# fp_dk[fp_dk$Name=="Todd Gurley", "Name"] <- "Todd Gurley II"
# fp_dk[fp_dk$Name=="Mitch Trubisky", "Name"] <- "Mitchell Trubisky"
# fp_dk[fp_dk$Name=="Ted Ginn", "Name"] <- "Ted Ginn Jr."
# fp_dk[fp_dk$Name=="Will Fuller", "Name"] <- "Will Fuller V"
# fp_dk[fp_dk$Name=="Duke Johnson", "Name"] <- "Duke Johnson Jr."
# fp_dk[fp_dk$Name=="Terrelle Pryor", "Name"] <- "Terrelle Pryor Sr."
# fp_dk[fp_dk$Name=="Marvin Jones", "Name"] <- "Marvin Jones Jr."
# fp_dk[fp_dk$Name=="Ronald Jones II", "Name"] <- "Ronald Jones"
# fp_dk[fp_dk$Name=="CJ Ham", "Name"] <- "C.J. Ham"
# fp_dk[fp_dk$Name=="D.K. Metcalf", "Name"] <- "DK Metcalf"
# fp_dk[fp_dk$Name=="Devante Parker", "Name"] <- "DeVante Parker"
# fp_dk[fp_dk$Name=="DJ Chark", "Name"] <- "DJ Chark Jr."



fp_dk$Team <- trim(fp_dk$Team)
fp_dk$Week <- Week
fp_dk$Site <- "Fantasy Pros"

#fp_dk$merge_name <- (paste0(fp_dk$Name,"_",fp_dk$Team))

#Remove scrub Chris Thompson on JAX
#Column to get rid of this game
# fp_dk$ct_remove <- paste0(fp_dk$Name,"_", fp_dk$position)
# fp_dk <- subset(fp_dk, ct_remove != "Chris Thompson_WR")

#Remove Cordarrelle Patterson RB since he is WR on DK

fp_dk <- fp_dk %>% 
  mutate(cp_check = ifelse(Name == "Cordarrelle Patterson" &
                             position == "RB", 1, 0),
         ab_check = ifelse(Name == "Andrew Beck" &
                             position == "RB", 1, 0),
         df_check = ifelse(Name == "Demetric Felton" &
                             position == "WR", 1, 0),
         jj_check = ifelse(Name == "Juwan Johnson" &
                             position == "TE", 1, 0),
         rg_check = ifelse(Name == "Reggie Gilliam" &
                             position == "TE", 1, 0),
         tm_check = ifelse(Name == "Ty Montgomery" &
                             position == "WR", 1, 0),
         gr_check = ifelse(Name == "Giovanni Ricci" &
                             position == "TE", 1, 0),
         ag_check = ifelse(Name == "A.J. Green" &
                             Team == "CLE", 1, 0),
         jf_check = ifelse(Name == "Jody Fortson" &
                             position == "WR", 1, 0),
         tw_check = ifelse(Name == "Trevon Wesco" &
                             position == "TE", 1, 0)
         ) %>% 
  filter(cp_check != 1) %>%
  filter(ab_check != 1) %>% 
  filter(df_check != 1) %>% 
  filter(jj_check != 1) %>% 
  filter(rg_check != 1) %>% 
  filter(tm_check != 1) %>% 
  filter(gr_check != 1) %>% 
  filter(ag_check != 1) %>%
  filter(jf_check != 1) %>% 
  filter(tw_check != 1) %>% 
  select(-cp_check, -ab_check, -df_check, -jj_check, -rg_check,
         -tm_check, -gr_check, -ag_check, - jf_check, -tw_check)
  

#Clean environment
rm(fp_qbs, fp_rbs, fp_tes, fp_wrs, fpqbs, fpQBsMerge, fprbs, fpRBsMerge, fptes,
   fpwrs, fpWRsMerge, fpTEsMerge)

#Pull as csv if needed
write.csv(fp_dk, file = paste0("fp_dk", Week, ".csv"))




# NumberFire --------------------------------------------------------------


#Base URL
#https://www.numberfire.com/nfl/daily-fantasy/daily-football-projections

numberFire <- read_html("https://www.numberfire.com/nfl/daily-fantasy/daily-football-projections")
numberFire <- numberFire %>% html_table(fill=TRUE)
numberFire <-  rbindlist(lapply(numberFire, as.data.table), fill = TRUE)

#Remove first three rows
numberFire <- numberFire[-1:-3, ]

#Remove first Column
numberFire <- numberFire[,-1]

#Make First Row Header
names(numberFire) <- as.matrix(numberFire[1, ])
numberFire <- numberFire[-1, ]
#numberFire[] <- lapply(numberFire, function(x) type.convert(as.character(x)))

#Rename duplicate columns
colnames(numberFire)[6] <- "Pass.Yards"
colnames(numberFire)[7] <- "Pass.Touchdowns"
colnames(numberFire)[8] <- "Interceptions"
colnames(numberFire)[10] <- "Rush.Yards"
colnames(numberFire)[11] <- "Rush.Touchdowns"
colnames(numberFire)[12] <- "Receptions"
colnames(numberFire)[13] <- "Rec.Yards"
colnames(numberFire)[14] <- "Rec.Touchdowns"

#Remove E St. Brown because he breaks A St. Brown
numberFire <- numberFire %>% 
  mutate(e_st_brown = case_when(
    str_detect(Player, regex("Equanimeous St. Brown", ignore_case=TRUE)) ~ "Remove",
    TRUE ~ "Keep"
  )) %>% 
  filter(e_st_brown == "Keep") %>% 
  select(-e_st_brown)


#Clean Player Name
#Separate at space
numberFire <- separate(numberFire, Player, into = c("Initial", "Last", "First",
                                                    "Last2", "Position", "Opp",
                                                    "Team", "Day", "Hour",
                                                    "Minutes"),
                       sep = "\\s+", 
                       remove = FALSE)

#Concatenate Name
numberFire$Name <- paste0(numberFire$First, " ", numberFire$Last)

#Remove unnecessary columns
numberFire <- numberFire[ , c(-1:-5, -7, -9:-15)]

#Read in csv for showdown slates
#numberFire <- read_csv("nf12_thur_manual.csv")

#As numeric all columns for calculations
numberFire$Rush.Touchdowns <- as.numeric(numberFire$Rush.Touchdowns)
numberFire$Rush.Yards <- as.numeric(numberFire$Rush.Yards)
numberFire$Pass.Yards <- as.numeric(numberFire$Pass.Yards)
numberFire$Pass.Touchdowns <- as.numeric(numberFire$Pass.Touchdowns)
numberFire$Interceptions <- as.numeric(numberFire$Interceptions)
numberFire$Rec.Touchdowns <- as.numeric(numberFire$Rec.Touchdowns)
numberFire$Rec.Yards <- as.numeric(numberFire$Rec.Yards)
numberFire$Receptions <- as.numeric(numberFire$Receptions)

#Calculate DK Points
numberFire$nfDK.Points <- 
  round(numberFire$Rush.Touchdowns * 6 +
          numberFire$Rush.Yards * 0.1 +
          ifelse(numberFire$Rush.Yards >= 100, 3, 0) +
          numberFire$Rec.Touchdowns * 6 +
          numberFire$Rec.Yards * 0.1 +
          ifelse(numberFire$Rec.Yards >= 100, 3, 0) +
          numberFire$Receptions +
          numberFire$Pass.Touchdowns * 4 +
          numberFire$Pass.Yards * 0.04 +
          ifelse(numberFire$Pass.Yards >=300, 3, 0) +
          numberFire$Interceptions * -1, 2)

#Rearrange columns and remove excess
numberFire <- numberFire[, c(13, 2, 1, 14)]

#Trim Team and Position just in case there are spare spaces
##Mirror Excel TRIM
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
numberFire$Team <- trim(numberFire$Team)
numberFire$Position <- trim(numberFire$Position)

#Remove everyone under 4 expected points
#numberFire <- subset(numberFire, nfDK.Points >= 4)

# #Start Cleaning Player Names (edit 2020 - try to take care of this in player mapping w index)
numberFire[numberFire$Name=="Le Bell", "Name"] <- "Le'Veon Bell"
numberFire[numberFire$Name=="Schuster Smith", "Name"] <- "JuJu Smith-Schuster"
numberFire[numberFire$Name=="T Hilton", "Name"] <- "T.Y. Hilton"
numberFire$Name[numberFire$Name=="D Moore"] <- "D.J. Moore"
numberFire[numberFire$Name=="D Metcalf", "Name"] <- "DK Metcalf"
numberFire[numberFire$Name=="T Hockenson", "Name"] <- "T.J. Hockenson"
numberFire[numberFire$Name=="T Yeldon", "Name"] <- "T.J. Yeldon"
numberFire[numberFire$Name=="O Howard", "Name"] <- "O.J. Howard"
numberFire[numberFire$Name=="Jr Ginn", "Name"] <- "Ted Ginn Jr."
numberFire[numberFire$Name=="Tre Smith", "Name"] <- "Tre'Quan Smith"
numberFire[numberFire$Name=="C Prosise", "Name"] <- "C.J. Prosise"
numberFire[numberFire$Name=="T Logan", "Name"] <- "T.J. Logan"
numberFire[numberFire$Name=="De Thomas", "Name"] <- "De'Anthony Thomas"
numberFire[numberFire$Name=="Leary O", "Name"] <- "Nick O'Leary"
numberFire[numberFire$Name=="Whiteside Arcega", "Name"] <- "JJ Arcega-Whiteside"
numberFire[numberFire$Name=="Marvin Jones", "Name"] <- "Marvin Jones Jr."
numberFire[numberFire$Name=="Will Fuller", "Name"] <- "Will Fuller V"
numberFire[numberFire$Name=="Scantling Valdes", "Name"] <- "Marquez Valdes-Scantling"
numberFire[numberFire$Name=="Odell Beckham", "Name"] <- "Odell Beckham Jr."
numberFire[numberFire$Name=="Todd Gurley", "Name"] <- "Todd Gurley II"

numberFire$Team <- trim(numberFire$Team)
numberFire$Week <- Week
numberFire$Site <- "numberFire"

#Removing Ryan Griffin the QB
numberFire <- numberFire %>% 
  mutate(rg_check = ifelse(Name == "Ryan Griffin" &
                             Position == "QB", 1, 0)) %>% 
  filter(rg_check != 1) %>% 
  select(-rg_check)

#Pull as csv if needed
write.csv(numberFire, file = paste0("numberFire_dk", Week, ".csv"))





# CBS ---------------------------------------------------------------------

# #QB
# page <- read_html(paste0("https://www.cbssports.com/fantasy/football/stats/QB/", Season, "/", Week, "/projections/ppr"))
# qbs <- page %>%
#   html_nodes('table') %>%
#   as.character()
# 
# grouped_header <- page %>%
#   html_nodes('.TableBase-headGroupTr') %>%
#   as.character()
# 
# qbs <- qbs %>%
#   gsub(grouped_header, "", ., perl=T) %>%
#   read_html() %>%
#   html_table()
# cbs_qbs <-  rbindlist(lapply(qbs, as.data.table), fill = TRUE)
# 
# #RB
# page <- read_html(paste0("https://www.cbssports.com/fantasy/football/stats/RB/", Season, "/", Week, "/projections/ppr"))
# rbs <- page %>%
#   html_nodes('table') %>%
#   as.character()
# 
# grouped_header <- page %>%
#   html_nodes('.TableBase-headGroupTr') %>%
#   as.character()
# 
# rbs <- rbs %>%
#   gsub(grouped_header, "", ., perl=T) %>%
#   read_html() %>%
#   html_table()
# 
# cbs_rbs <- rbindlist(lapply(rbs, as.data.table), fill = TRUE)
# 
# #WR
# page <- read_html(paste0("https://www.cbssports.com/fantasy/football/stats/WR/", Season, "/", Week, "/projections/ppr"))
# wrs <- page %>%
#   html_nodes('table') %>%
#   as.character()
# 
# grouped_header <- page %>%
#   html_nodes('.TableBase-headGroupTr') %>%
#   as.character()
# 
# wrs <- wrs %>%
#   gsub(grouped_header, "", ., perl=T) %>%
#   read_html() %>%
#   html_table()
# 
# 
# cbs_wrs <- rbindlist(lapply(wrs, as.data.table), fill = TRUE)
# 
# #TE
# page <- read_html(paste0("https://www.cbssports.com/fantasy/football/stats/TE/", Season, "/", Week, "/projections/ppr"))
# tes <- page %>%
#   html_nodes('table') %>%
#   as.character()
# 
# grouped_header <- page %>%
#   html_nodes('.TableBase-headGroupTr') %>%
#   as.character()
# 
# tes <- tes %>%
#   gsub(grouped_header, "", ., perl=T) %>%
#   read_html() %>%
#   html_table()
# 
# cbs_tes <- rbindlist(lapply(tes, as.data.table), fill = TRUE)
# 
# #Trim how stupid the player name column shows
# cbs_qbs$Player <- gsub("\\s+", " ", cbs_qbs$Player)
# cbs_rbs$Player <- gsub("\\s+", " ", cbs_rbs$Player)
# cbs_wrs$Player <- gsub("\\s+", " ", cbs_wrs$Player)
# cbs_tes$Player <- gsub("\\s+", " ", cbs_tes$Player)
# 
# #Separate at space
# cbs_qbs <- separate(cbs_qbs, Player, into = c("Initial", "Last Name",
#                                               "Pos", "Team", "First",
#                                               "Last", "Position", "Franchise"),
#                     remove = FALSE)
# 
# cbs_rbs <- separate(cbs_rbs, Player, into = c("Initial", "Last Name",
#                                               "Pos", "Team", "First",
#                                               "Last", "Position", "Franchise"),
#                     remove = FALSE)
# 
# cbs_wrs <- separate(cbs_wrs, Player, into = c("Initial", "Last Name",
#                                               "Pos", "Team", "First",
#                                               "Last", "Position", "Franchise"),
#                     remove = FALSE)
# 
# cbs_tes <- separate(cbs_tes, Player, into = c("Initial", "Last Name",
#                                               "Pos", "Team", "First",
#                                               "Last", "Position", "Franchise"),
#                     remove = FALSE)
# 
# #Concatenate First and Last Name
# cbs_qbs$Name <- paste0(cbs_qbs$First, " ", cbs_qbs$Last)
# cbs_rbs$Name <- paste0(cbs_rbs$First, " ", cbs_rbs$Last)
# cbs_wrs$Name <- paste0(cbs_wrs$First, " ", cbs_wrs$Last)
# cbs_tes$Name <- paste0(cbs_tes$First, " ", cbs_tes$Last)
# 
# #Rename columns, QBs
# colnames(cbs_qbs)[15] <- "Pass.TDs"
# colnames(cbs_qbs)[13] <- "Pass.Yards"
# colnames(cbs_qbs)[16] <- "Interceptions"
# colnames(cbs_qbs)[19] <- "RshYds"
# colnames(cbs_qbs)[21] <- "RshTDs"
# colnames(cbs_qbs)[22] <- "Fumbles"
# 
# #Rename columns, RBs
# colnames(cbs_rbs)[12] <- "RshYds"
# colnames(cbs_rbs)[14] <- "RshTDs"
# colnames(cbs_rbs)[16] <- "Receptions"
# colnames(cbs_rbs)[17] <- "RecYds"
# colnames(cbs_rbs)[20] <- "RecTDs"
# colnames(cbs_rbs)[21] <- "Fumbles"
# 
# #Rename columns, WRs
# colnames(cbs_wrs)[18] <- "RshYds"
# colnames(cbs_wrs)[20] <- "RshTDs"
# colnames(cbs_wrs)[12] <- "Receptions"
# colnames(cbs_wrs)[13] <- "RecYds"
# colnames(cbs_wrs)[16] <- "RecTDs"
# colnames(cbs_wrs)[21] <- "Fumbles"
# 
# #Rename columns, TEs
# colnames(cbs_tes)[12] <- "Receptions"
# colnames(cbs_tes)[13] <- "RecYds"
# colnames(cbs_tes)[16] <- "RecTDs"
# colnames(cbs_tes)[17] <- "Fumbles"
# 
# #Calc DK Points, QBs
# cbs_qbs$DKPoints <- cbs_qbs$Pass.TDs*4 +
#   cbs_qbs$Pass.Yards*0.04 +
#   ifelse(cbs_qbs$Pass.Yards >=300, 3, 0) +
#   cbs_qbs$Interceptions*-1 +
#   cbs_qbs$RshTDs*6 +
#   cbs_qbs$RshYds *0.1 +
#   ifelse(cbs_qbs$RshYds >= 100, 3, 0) +
#   cbs_qbs$Fumbles * -1
# 
# #Calc DK Points, RBs
# cbs_rbs$DKPoints <- cbs_rbs$RshTDs*6 +
#   cbs_rbs$RshYds *0.1 +
#   ifelse(cbs_rbs$RshYds >= 100, 3, 0) +
#   cbs_rbs$RecTDs * 6 +
#   cbs_rbs$RecYds * 0.1 +
#   ifelse(cbs_rbs$RecYds >= 300, 3, 0) +
#   cbs_rbs$Receptions +
#   cbs_rbs$Fumbles * -1
# 
# #Calc DK Points, WRs
# cbs_wrs$DKPoints <- cbs_wrs$RshTDs*6 +
#   cbs_wrs$RshYds *0.1 +
#   ifelse(cbs_wrs$RshYds >= 100, 3, 0) +
#   cbs_wrs$RecTDs * 6 +
#   cbs_wrs$RecYds * 0.1 +
#   ifelse(cbs_wrs$RecYds >= 300, 3, 0) +
#   cbs_wrs$Receptions +
#   cbs_wrs$Fumbles * -1
# 
# #Calc DK Points, TEs
# cbs_tes$DKPoints <- cbs_tes$RecTDs * 6 +
#   cbs_tes$RecYds * 0.1 +
#   ifelse(cbs_tes$RecYds >= 300, 3, 0) +
#   cbs_tes$Receptions +
#   cbs_tes$Fumbles * -1
# 
# #Keep relevant vars on each data frame, Name, Team, Position and Points
# relVars <- c("Name", "Pos", "Team", "DKPoints")
# 
# cbs_qbs <- cbs_qbs[, relVars, with = FALSE]
# cbs_rbs <- cbs_rbs[, relVars, with = FALSE]
# cbs_wrs <- cbs_wrs[, relVars, with = FALSE]
# cbs_tes <- cbs_tes[, relVars, with = FALSE]
# 
# #Quick clean of position column due to sloppy Name clean
# cbs_qbs$Pos <- as.factor("QB")
# cbs_wrs$Pos <- as.factor("WR")
# cbs_rbs$Pos <- as.factor("RB")
# cbs_tes$Pos <- as.factor("TE")
# 
# #Bind
# cbs_full <- dplyr::bind_rows(cbs_qbs, cbs_rbs)
# cbs_full <- dplyr::bind_rows(cbs_full, cbs_wrs)
# cbs_full <- dplyr::bind_rows(cbs_full, cbs_tes)
# 
# #Match names to DK
# cbs_full[cbs_full$Name=="Odell Beckham", "Name"] <- "Odell Beckham Jr."
# cbs_full[cbs_full$Name=="Steve Smith", "Name"] <- "Steve Smith Sr."
# cbs_full[cbs_full$Name=="Marvin Jones", "Name"] <- "Marvin Jones Jr."
# cbs_full[cbs_full$Name=="Duke Johnson", "Name"] <- "Duke Johnson Jr."
# cbs_full[cbs_full$Name=="Cecil Shorts", "Name"] <- "Cecil Shorts III"
# cbs_full[cbs_full$Name=="Josh Bellamy", "Name"] <- "Joshua Bellamy"
# cbs_full[cbs_full$Name=="T.J. Jones", "Name"] <- "TJ Jones"
# cbs_full[cbs_full$Name=="Terrelle Pryor", "Name"] <- "Terrelle Pryor Sr."
# cbs_full[cbs_full$Name=="Will Fuller", "Name"] <- "Will Fuller V"
# cbs_full[cbs_full$Name=="Todd Gurley", "Name"] <- "Todd Gurley II"
# cbs_full[cbs_full$Name=="Ted Ginn", "Name"] <- "Ted Ginn Jr."
# cbs_full[cbs_full$Name=="PIT JuJu", "Name"] <- "JuJu Smith-Schuster"
# cbs_full[cbs_full$Name=="Le Veon", "Name"] <- "Le'Veon Bell"
# cbs_full[cbs_full$Name=="T Y", "Name"] <- "T.Y. Hilton"
# cbs_full[cbs_full$Name=="O J", "Name"] <- "O.J. Howard"
# cbs_full$Name[cbs_full$Name=="D J" & cbs_full$Team=="CAR"] <- "D.J. Moore"
# cbs_full$Name[cbs_full$Name=="T J" & cbs_full$Team=="DET"] <- "T.J. Hockenson"
# cbs_full[cbs_full$Name=="D K", "Name"] <- "DK Metcalf"
# cbs_full$Name[cbs_full$Name=="C J" & cbs_full$Team=="DET"] <- "C.J. Anderson"
# cbs_full$Name[cbs_full$Name=="T J" & cbs_full$Team=="BUF"] <- "T.J. Yeldon"
# cbs_full$Name[cbs_full$Name=="C J" & cbs_full$Team=="CIN"] <- "C.J. Uzomah"
# cbs_full$Name[cbs_full$Name=="D J" & cbs_full$Team=="JAC"] <- "D.J. Chark Jr."
# cbs_full[cbs_full$Name=="PHI J", "Name"] <- "JJ Arcega-Whiteside"
# cbs_full[cbs_full$Name=="MIA Nick", "Name"] <- "Nick O'Leary"
# cbs_full[cbs_full$Name=="JAC James", "Name"] <- "James O'Shaughnessy"
# cbs_full[cbs_full$Name=="IND Mo", "Name"] <- "Mo Alie-Cox"
# cbs_full[cbs_full$Name=="D Ernest", "Name"] <- "D'Ernest Johnson"
# cbs_full$Name[cbs_full$Name=="C J" & cbs_full$Team=="MIN"] <- "C.J. Ham"
# cbs_full[cbs_full$Name=="BAL Robert", "Name"] <- "Robert Griffin III"
# cbs_full[cbs_full$Name=="Tre Quan", "Name"] <- "Tre'Quan Smith"
# cbs_full[cbs_full$Name=="Irv Smith", "Name"] <- "Irv Smith Jr."
# cbs_full$Team[cbs_full$Name=="JuJu Smith-Schuster" & cbs_full$Team=="WR"] <- "PIT"
# cbs_full[cbs_full$Name=="GB Marquez", "Name"] <- "Marquez Valdes-Scantling"
# cbs_full$Team[cbs_full$Name=="Marquez Valdes-Scantling" & cbs_full$Team=="WR"] <- "GB"
# cbs_full$Name[cbs_full$Name=="A J" & cbs_full$Team=="TEN"] <- "A.J. Brown"
# cbs_full$Team[cbs_full$Name=="James O'Shaughnessy" & cbs_full$Team=="TE"] <- "JAC"
# cbs_full$Name[cbs_full$Name=="J K" & cbs_full$Team=="BAL"] <- "J.K. Dobbins"
# cbs_full$Name[cbs_full$Name=="J D" & cbs_full$Team=="WAS"] <- "J.D. McKissic"
# cbs_full$Name[cbs_full$Name=="A J" & cbs_full$Team=="GB"] <- "AJ Dillon"
# cbs_full$Name[cbs_full$Name=="LV Henry" & cbs_full$Team=="WR"] <- "Henry Ruggs III"
# cbs_full$Name[cbs_full$Name=="A J" & cbs_full$Team=="CIN"] <- "AJ Green"

# fantasy sharks ----------------------------------------------------------

#hold base
#https://www.fantasysharks.com/apps/bert/forecasts/projections.php?League=&Position=53&scoring=2&Segment=697&uid=4

#flex positions
fs_flex <- read_html(
  paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?League=-1&Position=53&scoring=16&Segment=", Segment, "&uid=4"))
fs_flex <- fs_flex %>% html_table(fill=TRUE)
fs_flex <-  rbindlist(lapply(fs_flex, as.data.table), fill = TRUE)

#Quick clean step 1
fs_flex <- fs_flex %>% 
  select(Player, Pts) %>% 
  filter(!grepl("Tier", Pts)) %>% 
  filter(!grepl("Pts", Pts)) %>% 
  na.omit()

#QBs
fs_qb <- read_html(
  paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?League=-1&Position=1&scoring=16&Segment=", Segment, "&uid=4"))
fs_qb <- fs_qb %>% html_table(fill=TRUE)
fs_qb <-  rbindlist(lapply(fs_qb, as.data.table), fill = TRUE)

#Quick clean step 1
fs_qb <- fs_qb %>% 
  setNames(make.names(names(.), unique = TRUE)) %>% 
  select(Player, Pts) %>% 
  filter(!grepl("Tier", Pts)) %>% 
  filter(!grepl("Points Awarded", Player)) %>% 
  filter(!grepl("Pts", Pts)) %>% 
  #Trash QB, just brings a dupe, so knock him out
  filter(Player != "Griffin, Ryan") %>% 
  na.omit()

#Combine
fantasy_sharks <- bind_rows(fs_qb, fs_flex)
fantasy_sharks$Week <- Week
fantasy_sharks$Site <- "fantasysharks"

#Some players come in with a capital 'R' at the end of their name
#Probably due to having a note on them or something
#This removes that

fantasy_sharks$Player <- sub("R$", "", fantasy_sharks$Player)

# nfl.com -----------------------------------------------------------------
#hold base
#https://fantasy.nfl.com/research/projections

#all positions, first 500 players
offset <- c(1, 26, 51, 76, 101, 126, 151, 176, 201, 226, 251, 276, 301, 326, 351, 376, 401, 426, 451, 476)

nfl_list <- list()

for(page_id in offset) {
  
  stats <- read_html(paste0("https://fantasy.nfl.com/research/projections?offset=", page_id, "&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2021&statType=weekProjectedStats&statWeek=", Week))
  stats <- stats %>% html_table(fill=TRUE)
  stats <-  rbindlist(lapply(stats, as.data.table), fill = TRUE)
  
  #Append to list
  nfl_list[[page_id]] <- stats
  
}

#Create data frame from Loop list
nfl_all <- do.call(rbind, nfl_list)

#Clean
nfl_all <- nfl_all %>% 
  setNames(make.names(names(.), unique = TRUE))

#Clean the dataframe and calculate DK Points
nfl_all <- nfl_all %>% 
  filter(!grepl("Player", V1)) %>% 
  mutate_at(grep("^(Passing|Rushing|Receiving)",colnames(.)),funs(as.numeric)) %>% 
  plyr::rename(c("V1" = "Player",
                 "V2" = "Opp",
                 "Passing" = "pass_yards",
                 "Passing.1" = "pass_td",
                 "Passing.2" = "pass_int",
                 "Rushing" = "rush_yards",
                 "Rushing.1" = "rush_td",
                 "Receiving" = "receptions",
                 "Receiving.1" = "rec_yards",
                 "Receiving.2" = "rec_td")) %>% 
  select(-Ret, -Misc, -Misc.1, -Fum, -Fantasy) %>% 
  replace(is.na(.), 0) %>% 
  mutate(nfl_points = 
           (pass_yards * .04) +
           (pass_td * 4) +
           (pass_int * -1) +
           (rush_yards * 0.1) +
           (rush_td * 6) +
           (receptions) +
           (rec_yards * 0.1) +
           (rec_td * 6) +
           ifelse(pass_yards >= 300, 3, 0) +
           ifelse(rush_yards >= 100, 3, 0) +
           ifelse(rec_yards >= 100, 3, 0)
  )

#Player name cleaning
#Separate name into first name, last name, extras and remove extras
nfl_all <- nfl_all %>% 
  separate(Player, into = c("first_name", "last_name", "position", "extras"), sep = "\\s",
           extra = "merge") %>% 
  select(-extras)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
nfl_all$first_name <- trim(nfl_all$first_name)
nfl_all$last_name <- trim(nfl_all$last_name)

nfl_all$Name <- paste0(nfl_all$first_name, " ", nfl_all$last_name)
nfl_all$Site <- "NFL.com"
nfl_all$Week <- Week



# Data Cleanse ------------------------------------------------------------


library(tidyverse)

#Final data cleanse 2020 shiny app

#Need cols
#Index, Week, Player Name, Team, Position, Site(?)

#Read in player map
dim_players <- read.csv("player_map.csv")


#DK Data
player_data <- read.csv("DKSalaries.csv", stringsAsFactors = FALSE)

player_data$Week <- Week

#Remove QB Ryan Griffin
player_data <- player_data %>% 
  mutate(rg_check = ifelse(Name == "Ryan Griffin" &
                             Position == "QB", 1, 0)) %>% 
  filter(rg_check != 1) %>% 
  select(-rg_check)

# #Create ID
# player_data$DK.Name <- paste0(player_data$Name, "_",
#                               player_data$TeamAbbrev)

#Merge index
player_data <- dplyr::left_join(
  player_data, dim_players,
  by = "Name"
)

#Keep the correct columns
player_data <- player_data %>% 
  select(Index, Week, Name, Position, Roster.Position,
         TeamAbbrev, FP.Name, NF.Name, 
         #CBS.Name, 
         FS.Name, NFL.Name, 
         Air.Yards.ID,
         Salary) %>% 
  #Remove Josh Johnson (duplicate breaks outliers)
  filter(Name != "Josh Johnson")

#Merge numberfire points
player_data <- player_data %>% 
  left_join(select(numberFire, Name, nfDK.Points), by = c("NF.Name" = "Name"))

#Merge fantasy pros points
player_data$FP.Name <- as.character(player_data$FP.Name)
fp_dk$FP.Name <- as.character(fp_dk$FP.Name)

player_data <- player_data %>% 
  left_join(select(fp_dk, Name, FPPoints), by = c("FP.Name" = "Name"))

#Merge cbs points
# player_data <- player_data %>%
#   left_join(select(cbs_full, Name, DKPoints), by = c("CBS.Name" = "Name"))
# player_data <- plyr::rename(player_data, c("DKPoints" = "CBS_DKPoints"))

#Merge fantasy sharks points
player_data <- player_data %>% 
  left_join(select(fantasy_sharks, Player, Pts), by = c("FS.Name" = "Player"))

player_data <- plyr::rename(player_data, c("Pts" = "FS.Points"))
player_data$FS.Points <- as.numeric(player_data$FS.Points)

#Merge nfl.com points
player_data <- player_data %>% 
  left_join(select(nfl_all, Name, nfl_points), by = c("NFL.Name" = "Name"))

#Get Average
player_data$proj_points <- round((player_data$FPPoints + 
                                    player_data$nfDK.Points +
                                    player_data$FS.Points +
                                    player_data$nfl_points) / 4 ,2)

#at this point, I should find the largest outlier in the players projections and remove it from the pool

#Raw dataframe, find distance from mean
outliers_df <- player_data %>% 
  select(Name, nfDK.Points, FPPoints, FS.Points, nfl_points, proj_points) %>% 
  #Find difference from mean via abs value
  mutate(nf_outlier = abs(nfDK.Points - proj_points),
         fp_outlier = abs(FPPoints - proj_points),
         fs_outlier = abs(FS.Points - proj_points),
         nfl_outlier = abs(nfl_points - proj_points))

#Split into two dataframes, merge on player and site, hold this here for now
projections <- outliers_df %>% 
  select(Name, nfDK.Points, FPPoints, FS.Points, nfl_points) %>% 
  rename(numberfire = nfDK.Points,
         fantasypros = FPPoints,
         fantasysharks = FS.Points,
         nfl = nfl_points) %>% 
  pivot_longer(!Name, names_to = "site", values_to = "projected_points")

#Outliers
outliers <- outliers_df %>% 
  select(Name, nf_outlier, fp_outlier, fs_outlier, nfl_outlier) %>% 
  rename(numberfire = nf_outlier,
         fantasypros = fp_outlier,
         fantasysharks = fs_outlier,
         nfl = nfl_outlier) %>% 
  pivot_longer(!Name, names_to = "site", values_to = "distance") %>% 
  group_by(Name) %>% 
  mutate(outlier_rank = order(order(distance, decreasing = TRUE)))

#Merge the shits, remove the top ranked outlier, reshape to wide
outliers_df <- projections %>% 
  left_join(outliers, by = c("Name", "site")) %>% 
  filter(outlier_rank != 1) %>% 
  select(Name, site, projected_points) %>%
  pivot_wider(names_from = site, values_from = projected_points)

#Merge back to player data
player_data <- player_data %>% 
  left_join(outliers_df, by = "Name")

#Get the new average
player_data$proj_points_clean <- 
  rowMeans(player_data[,17:20], na.rm=TRUE)

#Value
player_data$value <- round(player_data$proj_points_clean / (player_data$Salary/1000),2)

#Confidence Intervals
#Calculate standard deviation
player_data$sd <- apply(player_data[,18:21], 1, sd, na.rm=TRUE)

#95% Confidence Interval
player_data$ceiling <- round(qnorm(0.975, player_data$proj_points_clean, player_data$sd),2)
player_data$floor <- round(qnorm(0.025, player_data$proj_points_clean, player_data$sd),2)

#Over/Under & Spread
#oddsshark not available in NY anymore to even view, source of O/U may vary
player_data$TeamAbbrev[player_data$TeamAbbrev=="ARI" & player_data$Week == Week] <- paste("ARI", )
player_data$TeamAbbrev[player_data$TeamAbbrev=="ATL" & player_data$Week == Week] <- paste("ATL", 47.5, -2)
player_data$TeamAbbrev[player_data$TeamAbbrev=="BAL" & player_data$Week == Week] <- paste("BAL", 44.5, -6)
player_data$TeamAbbrev[player_data$TeamAbbrev=="BUF" & player_data$Week == Week] <- paste("BUF", 49.5, -7)
player_data$TeamAbbrev[player_data$TeamAbbrev=="CAR" & player_data$Week == Week] <- paste("CAR", 43, -3.5)
player_data$TeamAbbrev[player_data$TeamAbbrev=="CHI" & player_data$Week == Week] <- paste("CHI", 44.5, 6)
player_data$TeamAbbrev[player_data$TeamAbbrev=="CIN" & player_data$Week == Week] <- paste("CIN", 50.5, -1)
player_data$TeamAbbrev[player_data$TeamAbbrev=="CLE" & player_data$Week == Week] <- paste("CLE", 42.5, -12)
player_data$TeamAbbrev[player_data$TeamAbbrev=="DAL" & player_data$Week == Week] <- paste("DAL", 56.5, 2.5)
player_data$TeamAbbrev[player_data$TeamAbbrev=="DET" & player_data$Week == Week] <- paste("DET", 42.5, 12)
player_data$TeamAbbrev[player_data$TeamAbbrev=="DEN" & player_data$Week == Week] <- paste("DEN", )
player_data$TeamAbbrev[player_data$TeamAbbrev=="GB" & player_data$Week == Week] <- paste("GB", 47, -1)
player_data$TeamAbbrev[player_data$TeamAbbrev=="HOU" & player_data$Week == Week] <- paste("HOU", 44.5, 10)
player_data$TeamAbbrev[player_data$TeamAbbrev=="IND" & player_data$Week == Week] <- paste("IND", 49.5, 7)
player_data$TeamAbbrev[player_data$TeamAbbrev=="JAX" & player_data$Week == Week] <- paste("JAX", 45, 6.5)
player_data$TeamAbbrev[player_data$TeamAbbrev=="KC" & player_data$Week == Week] <- paste("KC", 56.5, -2.5)
player_data$TeamAbbrev[player_data$TeamAbbrev=="LAC" & player_data$Week == Week] <- paste("LAC", )
player_data$TeamAbbrev[player_data$TeamAbbrev=="LAR" & player_data$Week == Week] <- paste("LAR", )
player_data$TeamAbbrev[player_data$TeamAbbrev=="LV" & player_data$Week == Week] <- paste("LV", 50.5, 1) 
player_data$TeamAbbrev[player_data$TeamAbbrev=="MIA" & player_data$Week == Week] <- paste("MIA", 44.5, -3.5)
player_data$TeamAbbrev[player_data$TeamAbbrev=="MIN" & player_data$Week == Week] <- paste("MIN", 47, 1)
player_data$TeamAbbrev[player_data$TeamAbbrev=="NE" & player_data$Week == Week] <- paste("NE", )
player_data$TeamAbbrev[player_data$TeamAbbrev=="NO" & player_data$Week == Week] <- paste("NO", 42.5, 2.5)
player_data$TeamAbbrev[player_data$TeamAbbrev=="NYG" & player_data$Week == Week] <- paste("NYG", )
player_data$TeamAbbrev[player_data$TeamAbbrev=="NYJ" & player_data$Week == Week] <- paste("NYJ", 44.5, 3.5)
player_data$TeamAbbrev[player_data$TeamAbbrev=="PIT" & player_data$Week == Week] <- paste("PIT", )
player_data$TeamAbbrev[player_data$TeamAbbrev=="PHI" & player_data$Week == Week] <- paste("Phi", 42.5, -2.5)
player_data$TeamAbbrev[player_data$TeamAbbrev=="SF" & player_data$Week == Week] <- paste("SF", 45, -6.5)
player_data$TeamAbbrev[player_data$TeamAbbrev=="SEA" & player_data$Week == Week] <- paste("SEA", 47.5, 2)
player_data$TeamAbbrev[player_data$TeamAbbrev=="TB" & player_data$Week == Week] <- paste("TB", )
player_data$TeamAbbrev[player_data$TeamAbbrev=="TEN" & player_data$Week == Week] <- paste("TEN", 44.5, -10)
player_data$TeamAbbrev[player_data$TeamAbbrev=="WAS" & player_data$Week == Week] <- paste("WAS", 43, 3.5)

#Split to columns
player_data <- separate(data = player_data, col = TeamAbbrev, into = c("TeamAbbrev", "oddssharkOU", "Spread"), sep = " ")

#Trim
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
player_data$TeamAbbrev <- trim(player_data$TeamAbbrev)
player_data$oddssharkOU <- trim(player_data$oddssharkOU)

player_data$oddssharkOU <- as.numeric(player_data$oddssharkOU)
player_data$Spread <- as.numeric(player_data$Spread)

player_data$implied_points <- (player_data$oddssharkOU/2) +
  ((player_data$Spread*-1)/2)

#Build air yards data base for scatter chart
air_yards_db <- dplyr::left_join(
  air_yards_players, dim_players, 
  by = c("ayID" = "Air.Yards.ID")
)

#Merge THIS WEEK'S dk salaries to new ay data
ay_salary_merge <- player_data %>% 
  select(Week, Name, Air.Yards.ID, Salary, Position, proj_points_clean) %>% 
  filter(Week == Week)

#STARTING WEEK 6 AFTER NOTICING ERROR IN WEEK 5
#ID max week by player
#Noticed that players who did not appear in prior week (Bye, COVID, etc...)
#were not showing up in air yards plot
air_yards_db <- air_yards_db %>%
  group_by(ayID, ayName) %>%
  mutate(max_week = max(week))

#Flag
air_yards_db$week_flag <-
  ifelse(air_yards_db$week == air_yards_db$max_week, 1, 0)

air_yards_db <- air_yards_db %>% 
  filter(week_flag == 1) %>%  #Need to update this every week?
  dplyr::left_join(ay_salary_merge,
  by = c("ayID" = "Air.Yards.ID")
)

#Read in old air yards db to new?

# # #Only for week 1, merge Air Yards ID to last_week set
# # last_week <- last_week %>%
# #   select(-Air.Yards.ID)
# # 
# last_week <- last_week %>%
#   dplyr::left_join(select(
#     dim_players, Index, Air.Yards.ID),
#     by = "Index")
# 
# last_week <- last_week %>%
#   dplyr::left_join(select(
#     air_yards_players, ayID, attempt, air_yards, ADOT, ay_share, tar_share),
#     by = c("Air.Yards.ID" = "ayID"))

#Append to last week data
player_data <- player_data %>%
  dplyr::bind_rows(last_week)

#Merge last week air yards << CLEAN FOR WEEK 3
# player_data <- player_data %>% 
#   dplyr::left_join(air_yards_players, by = )

#Append new ay database to old << CLEAN FOR WEEK 3
# air_yards <- readRDS("~/Code/base_scrapes/air_yards.rds")
# Remove current week in case of re-run?
# air_yards <- air_yards %>% 
#   filter(week != Week)
# archive
# saveRDS(air_yards, file = paste0("air_yards_", Week-1, ".rds"))

air_yards_db <- air_yards_db %>% 
  plyr::rename(c("week" = "Week.x", "Week" = "Week.y"))

#10/2/2021 - couldn't figure out why this was necessary?
# air_yards <- air_yards %>% 
#   dplyr::bind_rows(air_yards_db)



#save rds file to read in for next week
saveRDS(player_data, file = "player_projections.rds")
saveRDS(air_yards, file = "air_yards.rds")
#Save to shiny app directory as well
setwd("~/Code/nfl_dfs_app")
shiny_player_data <- player_data %>% 
  select(-Index, -FP.Name, -NF.Name, 
         #-CBS.Name,
         -FS.Name, -NFL.Name, 
         #-Air.Yards.ID)
  ) %>% 
  drop_na(proj_points)
saveRDS(shiny_player_data, file = "player_projections_clean.rds")
#10/2/2021 changed from manipulating the raw air_yards df to air_yards_db
shiny_air_yards <- air_yards_db %>% 
  select(-Name.x, -FP.Name, -NF.Name, #-CBS.Name,
         -FS.Name, -NFL.Name, -ayID)
saveRDS(shiny_air_yards, file = "shiny_air_yards.rds")

#Reverse wd
setwd("~/Code/base_scrapes")

#write.csv(player_data, file = "qa.csv")

#Simulation data set
sim_df <- readRDS("~/Code/base_scrapes/player_projections.rds")
sim_df <- sim_df %>% 
  drop_na(proj_points)

#Function copied and pasted from
#https://amiles.netlify.app/2019/09/building-optimal-daily-fantasy-lineups-in-r/

generate_lineup <- function(n){
  
  cur_week <- Week
  
  pred_sal <- sim_df %>%
    select(Name, Week, TeamAbbrev, Position, proj_points_clean, Salary, sd) %>% 
    na.omit(proj_points_clean) %>% 
    group_by(Name) %>%
    filter(Week == cur_week) %>%
    mutate(sal_max=max(Salary)) %>%
    #remove
     filter(Name != "D'Ernest Johnson" & Name != "Tim Boyle" &
              Name != "Deebo Samuel" & Name != "Tyler Lockett" &
              Name != "Darren Waller" & Name != "Marquise Brown" &
              Name != "Darrel Williams") %>%
    group_by(Name) %>%
    mutate(pts_pred = rnorm(1, proj_points_clean, sd),
           lineup=n) %>%
    select(-sal_max)

  #Set objective
  obj <- pred_sal$pts_pred
  
  #Reads the df and categorizes players into position (binary 1,0)
  mat <- rbind(t(model.matrix(~ Position + 0,pred_sal)), t(model.matrix(~ Position + 0,pred_sal)), rep(1, nrow(pred_sal)), pred_sal$Salary)
  
  #Direction of constraints
  #QB, RB, TE, WR, QB, RB, TE, WR, Total, Salary
  dir <- c("=","<=","<=","<=", "=",">=",">=",">=","=","<=")
  
  #Constraints
  rhs <- c(0,3,1,2,0,2,0,1,4,21500)
  
  result <- lp("max", obj, mat, dir, rhs, all.bin = TRUE)   
  
  results <- pred_sal[which(result$solution == 1),]
  
  return(results)

}

sim_lu <- map_df(1:10000, generate_lineup) %>%
  select(lineup, Name, TeamAbbrev, Position, proj_points_clean, pts_pred, sd, Salary)
  
  
sim_lu_merge <- sim_lu %>% 
  group_by(lineup) %>% 
  summarize_each(sum, pts_pred) %>% 
  arrange(desc(pts_pred))

sim_lu <- sim_lu %>% 
  dplyr::left_join(
    sim_lu_merge, by = "lineup"
  ) %>% 
  plyr::rename(c("pts_pred.x" = "pts_pred",
                 "pts_pred.y" = "lineup_points"))

#Save one-off run as csv
write.csv(sim_lu, file = "jordan_11.csv")


#Save rds to show up in app for plots
#Save to shiny app directory as well
setwd("~/Code/nfl_dfs_app")
saveRDS(sim_lu, file = "lineup_simulator.rds")

#Reverse wd
setwd("~/Code/base_scrapes")
write.csv(sim_lu, file = paste0("lineup_sims_week_", Week))

#Print sim counter in console
sim_lu %>% 
  count(Name, sort = TRUE) %>% 
  filter(n >= 1000) %>% 
  print(n = 50)
  


# Strength Test -----------------------------------------------------------

#read in player set
eval_df <- readRDS("~/Code/base_scrapes/player_projections.rds")

#for this run, let's do quick clean and merge of actuals
eval_df <- eval_df %>% 
  plyr::rename(c("Actual Points" = "Actual_Points")) %>% 
  select(-Ownership, -Actual_Points) %>% 
  dplyr::left_join(last_week_dk, by = c("Name", "Week"))

#Set limits
point_limit <- 5

eval_df <- eval_df %>% 
  filter(proj_points >= point_limit) %>% 
  plyr::rename(c("Actual Points" = "AP")) %>% 
  drop_na(FPPoints, AP)
  

#r-squared
#(cor(eval_df$nfDK.Points, eval_df$AP)^2)
(cor(eval_df$FPPoints, eval_df$AP)^2)
#(cor(eval_df$FS.Points, eval_df$AP)^2)
#(cor(eval_df$nfl_points, eval_df$AP)^2)

#(cor(eval_df$proj_points, eval_df$AP)^2)


# red zone ----------------------------------------------------------------

red_zone <- games_2020 %>% 
  filter(posteam != side_of_field &
           yardline_100 <= 20) %>% 
  select(posteam, week, passer_player_name, receiver_player_name, rusher_player_name) %>% 
  mutate(count = 1)

red_zone %>% 
    filter(posteam == "NE" | posteam == "ATL") %>% 
  subset(week >= 7) %>% 
    count(receiver_player_name, sort = TRUE)
