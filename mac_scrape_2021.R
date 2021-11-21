#Take 2, loop function.

#Packages
library(tidyverse)
library(rvest)

#Start with one game to test but we'll update to loop a list of game URLs

#Game links
game_links <- c(
  #12/6/21 Bowling Green
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGGA87POOzUzXutRjxmH0tkGuk2i7kMC2yHZBAWirJhT0%2fbckrTD7Ig9iJzdAfCgXK&path=mbball",
  
  #12/15/20 Miami (OH)
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGyHpAay4cIOTfY2rfvt%2fUqSRhlFpjkNu0lm6IyGZNA7CU6kwhqvtgu7hzakeXUZ6S&path=mbball",
  
  #12/19/20 Cuse (need to figure out OT)
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGDI%2fTYreTerbj3Q%2fBjfKhrf5mtcEqCUuLtNSFhJe1N0kMPqOXRl1bxaq9eK%2baaq3l&path=mbball",
  
  #1/9/21 Ball State
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGwoS6b%2fb9UTYuPuZKnIp5H5PpB1R95KBWHn0GKySoyrPBVuCRsmqN0T0tk3ajAKJQ&path=mbball",
  
  #1/12/21 Western Mich
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGY1uKgZk1MG3DwYe%2fvpXWBW5n2QIY%2bR9w1z9ekDPwPeJePIQwi0sgMUj7bpEtVdRg&path=mbball",
  
  #1/15/21 Bowling Green
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGBLLZx0BMkUBrVmqhMn7aUzImNVUYOk27uT8D1zkwfhDUX1WfHQOIM9kL6CwlIosT&path=mbball",
  
  #1/19/21 Kent State
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRG3RNUgTgxGZbYvQAWQzFOeNhP5KhZt8Dm5XrUk5aV5Xzkh57YQ54JqmbERJm%2bun1S&path=mbball",
  
  #1/21/21 EMU
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGqt8KQg4uIkjzXwX%2buwIN84kfFvSeN0Chz2YkQHE3HUplhF4ucoFc0Ef00%2bZAuDa6&path=mbball",
  
  #1/29/21 Ohio
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGFr8ct65AjSVJCGcps9XP65S7KZEv9ossu65XvRonvmzLBnyquzfJMFMu4FuMMFF7&path=mbball",
  
  #2/2/21 Ball State
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGmvbD3BSHT8lisTSjfl0DQvrdSc53GyhJsCQyMWdtwcJxT%2bEPhNoNwq%2bDK4yQs%2fVO&path=mbball",
  
  #2/6/21 Miami (OH)
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGscl%2b8StoWhF7kgi7TLwQuY4XDqJkQHIAI6A0UZuZf5yLUXtgyum6bZMu26MDXCdC&path=mbball",
  
  #2/12/21 WMU (2/13/21, no pbp?)
  #"https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGFb3p%2f4FuOpm4TcoJNbBxlsebDGQTu8QCvbvrjrjAn0834Tf5m9Xw5NWlSkalgpzs&path=mbball"
  
  #2/19/2021 Toledo
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGjBgA3fIvqzJhaSYCeue%2bkafAFWMS1FZis2xlNVxmilPJgkRYmMgL6pIduJgs3Jo6&path=mbball",
  
  #2/23/2021 @ NIU
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGDHtvaaqqN574yHOZLSSHH42T6yWQf524dfic0JNyl9KXYS%2fLTsce%2fYgDrGTe1Fdn&path=mbball",
  
  #2/25/2021 vs CMU
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGZOuKoKSE1%2fnC75GPQHskV%2bjSDyhmU70lL5zt9BslHNE%2fi%2f5kELDysoaqK5sClYPk&path=mbball",
  
  #2/27/2021 @ Ohio
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRG7FBXRhiMuQvxUgDwgO6rxm0tE3%2fb5brWUW3sQJd47kucC2hCVcg0S9HxwI9SYQhd&path=mbball",
  
  #3/2/2021 vs Akron
  "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGiwzL%2bZ3NAsmngpIUQgaNqbA9x8nckU1C97uZDBGou5D6SFRidvbGyRAp2aFAAiDJ&path=mbball",
  
  #3/5/2021 vs Kent St (Senior Night)
  "https://getsomemaction.com/boxscore.aspx?id=7Gx48kkB20iPlOppZhxXHy%2f5Zy4VtNXkGzuHdKHXRgkOw0pduZ0zZ9dLA8PyalFluCBCpHtNipVxMoZWikX6fw49h1T4iSptLk9q4OdFvjF39k6vjjbIL2o7XUZcyztYYlMFJiJcWK9OhnnlcL0rmw%3d%3d&path=mbball"
  
)

#Hold shot types
shot_types <- c("JUMPER", 'LAYUP', '3PTR', 'FT', 'DUNK')

#Per 40 minutes function
per_40 <- function(x, y){
  round((x/y)*40, 2)
}

per_100 <- function(x, y){
  round((x/y)*100, 2)
}

#Objects for storage to bind loop into
pbp_list <- list()
box_score_list <- list()

for(game_link in game_links) {
  
  #One game test
  #game_link <- c("https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGFb3p%2f4FuOpm4TcoJNbBxlsebDGQTu8QCvbvrjrjAn0834Tf5m9Xw5NWlSkalgpzs&path=mbball")
  
  #Scrape
  game_list <- read_html(game_link)
  game_list <- game_list %>% 
    html_table(fill=TRUE)
  
  #Hold game info
  #Box scores to identify starters and final counts
  home_box <- game_list[[5]]
  away_box <- game_list[[2]]
  game_info <- game_list[[1]]
  away_team <- game_info[1,1]
  home_team <- game_info[2,1]
  
  game_id <- paste0(game_info[1,1], "_at_", game_info[2,1])
  dim_teams <- tribble(
    ~Team, ~Team_Name,
    "Home", home_team,
    "Away", away_team
  )
  
  #Clean
  rm(away_team, home_team)
  
  #Identify starters and create a row to bind to our pbp with this info
  hbox_score_clean <- 
    home_box %>% 
    mutate(Index = paste0("H", row_number()),
           Team = "Home") %>% 
    select(Player, GS, Index, Team) %>% 
    mutate(Player = gsub('[0-9]+', "", Player),
           Player = str_trim(str_to_upper(Player)))
  
  abox_score_clean <- 
    away_box %>% 
    mutate(Index = paste0("A", row_number()),
           Team = "Away") %>% 
    select(Player, GS, Index, Team) %>% 
    mutate(Player = gsub('[0-9]+', "", Player),
           Player = str_trim(str_to_upper(Player)))
  
  rosters <- hbox_score_clean %>% 
    bind_rows(abox_score_clean) %>% 
    left_join(dim_teams, by = "Team")
  
  starters <-
    rosters %>% 
    filter(GS == "*") %>% 
    #Match player name to play by play (remove numbers and capitalize)
    mutate(Player = gsub('[0-9]+', "", Player),
           Player = str_trim(str_to_upper(Player))) %>% 
    #First half, 20 minutes remaining
    mutate(Half = "First",
           Time_Remaining = "20:00",
           Time_Remaining_Numeric = as.numeric(20),
           Event = "Player_In") %>%
    #Reshape wide
    select(Time_Remaining, Half, Time_Remaining_Numeric, Team, Event, Player)
  
  
  #                                           #
  #         Start Play-By-Play (pbp)          #
  #                                           #
  
  #Play by play will be used for analysis of who is on court
  first_half <-  game_list[[8]]
  names(first_half) <- make.unique(names(first_half))
  names(first_half) <- gsub(" ", "_", names(first_half))
  
  #Quick cleanse before append
  first_half <- first_half %>% 
    select(Time_Remaining, Game_Score, Game_Score.1) %>%
    mutate(Half = "First")
  
  second_half <-  game_list[[9]]
  names(second_half) <- make.unique(names(second_half))
  names(second_half) <- gsub(" ", "_", names(second_half))
  
  #Quick cleanse before append
  second_half <- second_half %>% 
    select(Time_Remaining, Game_Score, Game_Score.1) %>%
    mutate(Half = "Second") 
  
  #Build and fill game start and half ends, bind to pbp
  half_markers <- tribble(
    ~Time_Remaining, ~Description, ~Half, ~Game_ID,
    "20:00", "Game Start", "First", game_id,
    "00:00", "First Half End", "First", game_id,
    "20:00", "Second Half Start", "Second", game_id,
    "00:00", "Second Half End", "Second", game_id
  )
  
  #Fill in the data frame
  pbp <- dplyr::bind_rows(first_half, second_half) %>%
    mutate(Time_Remaining = ifelse(Time_Remaining == "--", NA, Time_Remaining),
           Game_ID = game_id) %>%
    fill(Time_Remaining) %>% 
    plyr::rename(c("Game_Score.1" = "Description")) %>% 
    bind_rows(half_markers)
  
  rm(first_half, second_half, half_markers)
  
  #PBP Clean up
  pbp <- pbp %>% 
    #Events
    mutate(Event = case_when(
      str_detect(Description, regex("JUMPER", ignore_case=TRUE)) ~ "Jumper",
      str_detect(Description, regex("LAYUP", ignore_case=TRUE)) ~ "Layup",
      str_detect(Description, regex("3PTR", ignore_case=TRUE)) ~ "3ptr",
      str_detect(Description, regex("FT", ignore_case=TRUE)) ~ "FT",
      str_detect(Description, regex("DUNK", ignore_case=TRUE)) ~ "Dunk",
      str_detect(Description, regex("REBOUND", ignore_case=TRUE)) ~ "Rebound",
      str_detect(Description, regex("ASSIST", ignore_case=TRUE)) ~ "Assist",
      str_detect(Description, regex("TURNOVER", ignore_case=TRUE)) ~ "Turnover",
      str_detect(Description, regex("STEAL", ignore_case=TRUE)) ~ "Steal",
      str_detect(Description, regex("FOUL", ignore_case=TRUE)) ~ "Foul",
      str_detect(Description, regex("SUB OUT", ignore_case=TRUE)) ~ "Player_Out",
      str_detect(Description, regex("SUB IN", ignore_case=TRUE)) ~ "Player_In",
      str_detect(Description, regex("TIMEOUT", ignore_case=TRUE)) ~ "Timeout",
      str_detect(Description, regex("BLOCK", ignore_case=TRUE)) ~ "Block",
      TRUE ~ "Unclassified")) %>% 
    
    #Good or miss
    mutate(Result = case_when(
      str_detect(Description, regex("GOOD", ignore_case=TRUE)) ~ "Good",
      str_detect(Description, regex("MISS", ignore_case=TRUE)) ~ "Miss",
      TRUE ~ "Not a shot"
    )) %>% 
    
    #Extract parenthetical info and remove from Description
    mutate(Location = str_extract(string = Description, pattern = "(?<=\\().*(?=\\))")) %>% 
    mutate(Description = gsub("\\s*\\([^\\)]+\\)", "", as.character(Description))) %>% 
    
    #Event player
    mutate(Player = ifelse(word(Description, -1)=="TEAM",
                           "No Assigned Player",
                           str_trim(sub('.+by(.+)', '\\1', Description)))) %>% 
    
    #Convert MM:SS to Decimal number
    mutate(Time_Remaining_Numeric =
             sapply(strsplit(Time_Remaining,":"),
                    function(x) {
                      x <- as.numeric(x)
                      x[1]+x[2]/60
                    }
             )) %>% 
    #Rebound location
    mutate(Rebound_Loc = ifelse(Event == "Rebound",
                                ifelse(
                                  str_detect(Description, regex("OFF", ignore_case=TRUE)),
                                  "Off", "Def"), NA)) %>% 
    
    #Arrange to calculate time elapsed between events
    arrange(Half, -Time_Remaining_Numeric) %>% 
    left_join(select(rosters, Player, Team, Team_Name), by = c("Player"))
  
  #                                                                     #
  #         Visual Break, Moving from PBP Cleaning to Sub Logic         #
  #                                                                     #
  
  
  #Subs home team
  subs_home <- pbp %>% 
    select(Time_Remaining, Half, Time_Remaining_Numeric, Team, Event, Player) %>% 
    bind_rows(starters) %>% 
    subset(Team == "Home") %>% 
    subset(Event == "Player_Out" | Event == "Player_In") %>% 
    arrange(Half, desc(Time_Remaining)) %>% 
    pivot_wider(names_from = Player, values_from = Event) %>% 
    fill(names(.))
  
  #Loop to replace any instance of "Player_In" with the player col header
  for (i in 1:length(subs_home)) {
    subs_home[[i]] <- str_replace(subs_home[[i]], "Player_In", colnames(subs_home)[i])
  }
  
  #subs_home continued
  subs_home <- subs_home %>% 
    #Replace all instances of "Player_Out" with NA to help concat
    na_if("Player_Out") %>% 
    unite("On_Court_Home", 5:ncol(.), na.rm = TRUE, remove = FALSE)
  
  #Subs away team
  subs_away <- pbp %>% 
    select(Time_Remaining, Half, Time_Remaining_Numeric, Team, Event, Player) %>% 
    bind_rows(starters) %>% 
    subset(Team == "Away") %>% 
    subset(Event == "Player_Out" | Event == "Player_In") %>% 
    arrange(Half, desc(Time_Remaining)) %>% 
    pivot_wider(names_from = Player, values_from = Event) %>% 
    fill(names(.))
  
  #Loop to replace any instance of "Player_In" with the player col header
  for (i in 1:length(subs_away)) {
    subs_away[[i]] <- str_replace(subs_away[[i]], "Player_In", colnames(subs_away)[i])
  }
  
  #subs_away continued
  subs_away <- subs_away %>% 
    #Replace all instances of "Player_Out" with NA to help concat
    na_if("Player_Out") %>% 
    unite("On_Court_Away", 5:ncol(.), na.rm = TRUE, remove = FALSE)
  
  #Put in a fake line for the 20 minute mark to get
  starters_bind <- starters %>% 
    select(Time_Remaining, Time_Remaining_Numeric, Half) %>% 
    unique()
  
  #Merge the subs to the pbp
  pbp <- pbp %>% 
    #Bind the fake starters row so we get the 20 min on court tandems
    bind_rows(starters_bind) %>% 
    arrange(Half, -Time_Remaining_Numeric) %>%
    group_by(Half) %>% 
    mutate(Time_Elapsed = lag(Time_Remaining_Numeric) - Time_Remaining_Numeric) %>% 
    ungroup() %>% 
    mutate(Time_Elapsed = replace_na(Time_Elapsed, 0)) %>% 
    #Join Home on court
    left_join(select(subs_home, Time_Remaining, Half, On_Court_Home), 
              by = c("Time_Remaining", "Half")) %>% 
    #Join Away on court
    left_join(select(subs_away, Time_Remaining, Half, On_Court_Away), 
              by = c("Time_Remaining", "Half")) %>% 
    arrange(Half, desc(Time_Remaining)) %>% 
    fill(On_Court_Home) %>% fill(On_Court_Away) %>% 
    #Separate into H1:H5 and A1:A5
    separate(On_Court_Home, c("H1", "H2", "H3", "H4", "H5"), sep = "_", remove = FALSE) %>% 
    separate(On_Court_Away, c("A1", "A2", "A3", "A4", "A5"), sep = "_", remove = FALSE)
  
  #Scoreboard
  pbp <- pbp %>%
    #Field goals
    mutate(FGM_home = ifelse(Team == "Home" & Result == "Good" & Event != "FT", 1, 0),
           FGA_home = ifelse(Team == "Home" & Result == "Miss" & Event != "FT", 1, 0),
           FGM_away = ifelse(Team == "Away" & Result == "Good" & Event != "FT", 1, 0),
           FGA_away = ifelse(Team == "Away" & Result == "Miss" & Event != "FT", 1, 0),
           #Free throws
           FTM_home = ifelse(Team == "Home" & Result == "Good" & Event == "FT", 1, 0),
           FTA_home = ifelse(Team == "Home" & Result == "Miss" & Event == "FT", 1, 0),
           FTM_away = ifelse(Team == "Away" & Result == "Good" & Event == "FT", 1, 0),
           FTA_away = ifelse(Team == "Away" & Result == "Miss" & Event == "FT", 1, 0),
           #Bucket value
           Bucket_Value = ifelse(Result == "Miss", 0,
                                 ifelse(Event == "Jumper" |
                                          Event == "Layup" |
                                          Event == "Dunk", 2,
                                        ifelse(Event == "3ptr", 3,
                                        ifelse(Event == "FT", 1, 0))))
    ) %>%
    #Scoreboard
    mutate(Home_Score = ifelse(Team == "Home", Bucket_Value, 0),
           Away_Score = ifelse(Team == "Away", Bucket_Value, 0)
    ) %>% 
    mutate(Home_Score = replace_na(Home_Score, 0),
           Away_Score = replace_na(Away_Score, 0)) %>% 
    #Running Score
    group_by(Game_ID) %>% 
    mutate(Home_Total = cumsum(Home_Score)) %>% 
    mutate(Away_Total = cumsum(Away_Score))
  
  #Calculate possessions
  pbp <- pbp %>% 
    mutate(home_pos = 
             ifelse(FGM_home == 1, 1,
                    ifelse(FGA_home == 1, 1,
                           ifelse(FTM_home == 1, .4,
                                  ifelse(FTA_home == 1, .4,
                                         ifelse(Team == "Home" & Event == "Rebound" & Rebound_Loc == "Off", -1, 
                                                ifelse(Team == "Home" & Event == "Turnover", 1, 0)
                                         )
                                  )
                           )
                    )
             )
    )
  
  pbp <- pbp %>% 
    mutate(away_pos = 
             ifelse(FGM_away == 1, 1,
                    ifelse(FGA_away == 1, 1,
                           ifelse(FTM_away == 1, .4,
                                  ifelse(FTA_away == 1, .4,
                                         ifelse(Team == "Away" & Event == "Rebound" & Rebound_Loc == "Off", -1, 
                                                ifelse(Team == "Away" & Event == "Turnover", 1, 0)
                                         )
                                  )
                           )
                    )
             )
    )
  
  #replace NA in summary cols
  pbp[, 27:41][is.na(pbp[, 27:41])] <- 0
  
  #Put the pbp into the list, start cleaning up the box scores
  pbp_list[[game_link]] <- pbp
  
  hold_home <- game_info[2,1]
  hold_away <- game_info[1,1]
  
  #Box score clean
  home_box <- home_box %>% 
    drop_na(TOT) %>%  
    filter(Player != "Totals" & Player != "0 TEAM") %>% 
    #Clean player name to match our on_court df
    mutate(Player = gsub('[0-9]+', "", Player),
           Player = str_trim(str_to_upper(Player))) %>% 
    separate(`FGM-FGA`, c("FGM", "FGA"), sep = "-", remove = FALSE) %>% 
    separate(`OFF-DEF`, c("OREB", "DREB"), sep = "-", remove = FALSE) %>% 
    rename(REB = TOT,
           PTS = TP) %>% 
    select(Player, FGM, FGA, OREB, DREB, REB, PF, PTS, A, TO, BLK, STL) %>% 
    mutate(Team = hold_home)
  
  #Away box score
  away_box <- away_box %>% 
    drop_na(TOT) %>%  
    filter(Player != "Totals" & Player != "0 TEAM") %>% 
    #Clean player name to match our on_court df
    mutate(Player = gsub('[0-9]+', "", Player),
           Player = str_trim(str_to_upper(Player))) %>% 
    separate(`FGM-FGA`, c("FGM", "FGA"), sep = "-", remove = FALSE) %>% 
    separate(`OFF-DEF`, c("OREB", "DREB"), sep = "-", remove = FALSE) %>% 
    rename(REB = TOT,
           PTS = TP) %>% 
    select(Player, FGM, FGA, OREB, DREB, REB, PF, PTS, A, TO, BLK, STL) %>% 
    mutate(Team = hold_away)
  
  box_score <- home_box %>% 
    bind_rows(away_box) %>% 
    mutate(game_id = game_id)
  
  box_score_list[[game_id]] <- box_score
  
  #I think the loop needs to end here, to store all the pbp data for every game
  #Then we can do our five-man and player manipulations on just an extra level
  #in the group by for game id. Having a full pbp file is only a good thing.
  
  
}

#Make big data frames of the loop objects and remove what we don't want
pbp_all <- do.call(rbind, pbp_list)
box_score_all <- do.call(rbind, box_score_list)

rm(list= ls()[!(ls() %in% c('pbp_all','box_score_all', 'per_100', 'per_40'))])


#                                                 #
#         Begin PBP Cleaning and Analysis         #
#                                                 #


#                                 #
#         Five man units          #
#                                 #


five_man_h <- pbp_all %>% 
  drop_na(Game_ID) %>% 
  group_by(Game_ID, On_Court_Home) %>% 
  summarize_at(c("Time_Elapsed", "home_pos", "away_pos",
                 "FGM_home", "FGA_home", "FTM_home", "FTA_home",
                 "FGM_away", "FGA_away", "FTM_away", "FTA_away",
                 "Bucket_Value", "Home_Score", "Away_Score"),
               sum) %>% 
  #Grab home team name
  mutate(Game_ID_split = Game_ID) %>% 
  separate(Game_ID_split, into = c("Away", "at", "Home"), sep = "_") %>% 
  select(-at, -Away) %>% 
  rename(Team = Home,
         On_Court = On_Court_Home,
         Points_For = Home_Score,
         Points_Against = Away_Score)

#Repeat for away and bind
five_man_a <- pbp_all %>% 
  drop_na(Game_ID) %>% 
  group_by(Game_ID, On_Court_Away) %>% 
  summarize_at(c("Time_Elapsed", "home_pos", "away_pos",
                 "FGM_home", "FGA_home", "FTM_home", "FTA_home",
                 "FGM_away", "FGA_away", "FTM_away", "FTA_away",
                 "Bucket_Value", "Home_Score", "Away_Score"),
               sum) %>% 
  #Grab away team name
  mutate(Game_ID_split = Game_ID) %>% 
  separate(Game_ID_split, into = c("Away", "at", "Home"), sep = "_") %>% 
  select(-at, -Home) %>% 
  rename(Team = Away,
         On_Court = On_Court_Away,
         Points_For = Away_Score,
         Points_Against = Home_Score)

#Bind it and clean it
five_man <- five_man_h %>% 
  bind_rows(five_man_a)

rm(five_man_a, five_man_h)

#                                     #
#         Individual Players          #
#                                     #

#Edit the PBP for the cols we need so we can use pivot_longer and across
on_court <- pbp_all %>% 
 
  rename(
    #Players
    player_h1 = H1,
    player_h2 = H2,
    player_h3 = H3,
    player_h4 = H4,
    player_h5 = H5,
    player_a1 = A1,
    player_a2 = A2,
    player_a3 = A3,
    player_a4 = A4,
    player_a5 = A5,
    #Metrics
    metric_FGM_h = FGM_home,
    metric_FGA_h = FGA_home,
    metric_FTM_h = FTM_home,
    metric_FTA_h = FTA_home,
    metric_FGM_a = FGM_away,
    metric_FGA_a = FGA_away,
    metric_FTM_a = FTM_away,
    metric_FTA_a = FTA_away,
    metric_BuckVal = Bucket_Value,
    metric_Home_Score = Home_Score,
    metric_Away_Score = Away_Score,
    metric_pos_h = home_pos,
    metric_pos_a = away_pos,
    metric_mins = Time_Elapsed
  ) %>% 
  pivot_longer(cols = starts_with('player_')) %>% 
  group_by(name, value, Game_ID) %>% 
  summarise(across(starts_with('metric'), sum)) %>% 
  #Need to re-aggregate if a player appears in multiple spots in same game
  ungroup() %>% 
  select(-name) %>% 
  group_by(value, Game_ID) %>% 
  summarise(across(starts_with('metric'), sum)) %>% 
  drop_na()

#Need to do another similar aggregate but for the game ID, that way
#We can compare total score to player on court, and then get off and rel

Game_Totals <- pbp_all %>% 
  
  rename(
    #Metrics
    metric_FGM_h = FGM_home,
    metric_FGA_h = FGA_home,
    metric_FTM_h = FTM_home,
    metric_FTA_h = FTA_home,
    metric_FGM_a = FGM_away,
    metric_FGA_a = FGA_away,
    metric_FTM_a = FTM_away,
    metric_FTA_a = FTA_away,
    metric_BuckVal = Bucket_Value,
    metric_Home_Score = Home_Score,
    metric_Away_Score = Away_Score,
    metric_pos_h = home_pos,
    metric_pos_a = away_pos,
    metric_mins = Time_Elapsed
  ) %>% 
  group_by(Game_ID) %>% 
  summarise(across(starts_with('metric'), sum)) %>% 
  drop_na()

#Need to establish home vs away in Game Totals and On Court to differentiate
#Between points for and points against

#Split home away and rebind

#On Court

#Home
on_court_h <- on_court %>% 
  left_join(select(
    box_score_all, Player, Team, game_id),
    by = c("value" = "Player", "Game_ID" = "game_id")) %>% 
  
  #ID home vs away by splitting game_id
  separate(Game_ID, c("Away", "at", "Home"), sep = "_", remove = FALSE) %>% 
  select(-at) %>% 
  mutate(team_flag = ifelse(
    Team == Home, "Home", "Away"
  )) %>% 
  filter(team_flag == "Home") %>% 
  
  #rename cols to for or against instead of home/away
  rename(FGM_for = metric_FGM_h, 
         FGA_for = metric_FGA_h,
         FGM_against = metric_FGM_a,
         FGA_against = metric_FGA_a,
         FTM_for = metric_FTM_h, 
         FTA_for = metric_FTA_h,
         FTM_against = metric_FTM_a, 
         FTA_against = metric_FTA_a,
         Bucket_Value = metric_BuckVal,
         Pts_for = metric_Home_Score,
         Pts_against = metric_Away_Score, 
         Pos_for = metric_pos_h,
         Pos_against = metric_pos_a,
         Minutes = metric_mins,
         Player = value) %>% 
  select(-Away, -Home, -team_flag) %>% 
  relocate(Team, .before = Minutes) %>% 
  
  #Merge totals
  left_join(Game_Totals, by = "Game_ID") %>% 
  
  #Rename friendly, part two
  rename(FGM_for_team = metric_FGM_h, 
         FGA_for_team = metric_FGA_h,
         FGM_against_team = metric_FGM_a,
         FGA_against_team = metric_FGA_a,
         FTM_for_team = metric_FTM_h, 
         FTA_for_team = metric_FTA_h,
         FTM_against_team = metric_FTM_a, 
         FTA_against_team = metric_FTA_a,
         Bucket_Value_team = metric_BuckVal,
         Pts_for_team = metric_Home_Score,
         Pts_against_team = metric_Away_Score, 
         Pos_for_team = metric_pos_h,
         Pos_against_team = metric_pos_a,
         Minutes_game = metric_mins)
  
#Away

on_court_a <- on_court %>% 
  left_join(select(
    box_score_all, Player, Team, game_id),
    by = c("value" = "Player", "Game_ID" = "game_id")) %>% 
  
  #ID home vs away by splitting game_id
  separate(Game_ID, c("Away", "at", "Home"), sep = "_", remove = FALSE) %>% 
  select(-at) %>% 
  mutate(team_flag = ifelse(
    Team == Home, "Home", "Away"
  )) %>% 
  filter(team_flag == "Away") %>% 
  
  #rename cols to for or against instead of home/away
  rename(FGM_against = metric_FGM_h, 
         FGA_against = metric_FGA_h,
         FGM_for = metric_FGM_a,
         FGA_for = metric_FGA_a,
         FTM_against = metric_FTM_h, 
         FTA_against = metric_FTA_h,
         FTM_for = metric_FTM_a, 
         FTA_for = metric_FTA_a,
         Bucket_Value = metric_BuckVal,
         Pts_against = metric_Home_Score,
         Pts_for = metric_Away_Score, 
         Pos_against = metric_pos_h,
         Pos_for = metric_pos_a,
         Minutes = metric_mins,
         Player = value) %>% 
  select(-Away, -Home, -team_flag) %>% 
  relocate(Team, .before = Minutes) %>% 
  
  #Merge totals
  left_join(Game_Totals, by = "Game_ID") %>% 
  
  #Rename friendly, part two
  rename(FGM_against_team = metric_FGM_h, 
         FGA_against_team = metric_FGA_h,
         FGM_for_team = metric_FGM_a,
         FGA_for_team = metric_FGA_a,
         FTM_against_team = metric_FTM_h, 
         FTA_against_team = metric_FTA_h,
         FTM_for_team = metric_FTM_a, 
         FTA_for_team = metric_FTA_a,
         Bucket_Value_team = metric_BuckVal,
         Pts_against_team = metric_Home_Score,
         Pts_for_team = metric_Away_Score, 
         Pos_against_team = metric_pos_h,
         Pos_for_team = metric_pos_a,
         Minutes_game = metric_mins)

#Bind it to one on_court df
on_court <-
  on_court_h %>% 
  bind_rows(on_court_a)

#Last merge/clean, individual slash stats
player_data <- on_court %>% 
  left_join(select(box_score_all, - Team),
            by = c("Player", "Game_ID" = "game_id"))

#As numeric the individual box score cols
player_data[,32:42] <- sapply(player_data[,32:42],as.numeric)

#Replace underscores with spaces
player_data <- player_data %>% 
  mutate(Game_ID = str_replace_all(Game_ID, "_", " "))

#Save data
saveRDS(player_data, "ub_player_data.rds")
saveRDS(five_man, "ub_five-man_data.rds")

#Save to shiny directory
#Save to app wd and reverse wd back
setwd("C:/Users/abg68/Documents/Code/ncaab/bulls_app")
saveRDS(player_data, "ub_player_data.rds")
setwd("~/Code/ncaab/bulls")
