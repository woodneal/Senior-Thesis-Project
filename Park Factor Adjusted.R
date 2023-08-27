library(tidyverse)
library(devtools)
require(devtools)
library(baseballr)
require(baseballr)
library(rsvg)
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(ggplot2)
library(ggimage)
library(na.tools)
library(stringr)
library(ggimage)

setwd("/Users/neal/Desktop/UMass/Thesis\ Project/CSVs/Pitch\ Data/")
format_data <- function(df) {
  # function for appending new variables to the data set
  additional_info <- function(df) {
    # apply additional coding for custom variables
    df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
                                   ifelse(type == "X" & events == "double", 2,
                                          ifelse(type == "X" & events == "triple", 3, 
                                                 ifelse(type == "X" & events == "home_run", 4, 
                                                        ifelse(type == "X" & events == "sac_fly", 5, NA))))))
    df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
                              ifelse(type == "X" & events == "double", 1,
                                     ifelse(type == "X" & events == "triple", 1, 
                                            ifelse(type == "X" & events == "home_run", 1, NA)))))
    df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
    df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
    df$on_1b[is.na(df$on_1b)] <- 0
    df$on_2b[is.na(df$on_2b)] <- 0
    df$on_3b[is.na(df$on_3b)] <- 0
    df <- df %>% 
      mutate(neutral_dates = 
               ifelse(game_date == "2017-08-29" & home_team == "HOU", 1,
                      ifelse(game_date == "2017-08-30" & home_team == "HOU", 1,
                             ifelse(game_date == "2017-08-31" & home_team == "HOU", 1,
                                    ifelse(game_date == "2017-09-11" & home_team == "TB", 1,
                                           ifelse(game_date == "2017-09-12" & home_team == "TB", 1, 
                                                  ifelse(game_date == "2017-09-13" & home_team == "TB", 1, 
                                                         ifelse(game_date == "2018-04-17" & home_team == "MIN", 1, 
                                                                ifelse(game_date == "2018-04-18" & home_team == "MIN", 1, 
                                                                       ifelse(game_date == "2018-05-04" & home_team == "SD", 1, 
                                                                              ifelse(game_date == "2018-05-05" & home_team == "SD", 1,
                                                                                     ifelse(game_date == "2018-05-06" & home_team == "SD", 1,
                                                                                            ifelse(game_date == "2018-05-04" & home_team == "SD", 1,
                                                                                                   ifelse(game_date == "2019-04-13" & home_team == "CIN", 1,
                                                                                                          ifelse(game_date == "2019-04-14" & home_team == "CIN", 1,
                                                                                                                 ifelse(game_date == "2019-05-04" & home_team == "LAA", 1,
                                                                                                                        ifelse(game_date == "2019-05-05" & home_team == "LAA", 1,
                                                                                                                               ifelse(game_date == "2019-06-13" & home_team == "KC", 1, 0)))))))))))))))))) %>%
      mutate(runner_on_1b = ifelse(on_1b != 0, 1, 0)) %>%
      mutate(runner_on_2b = ifelse(on_2b != 0, 1, 0)) %>%
      mutate(runner_on_3b = ifelse(on_3b != 0, 1, 0)) %>%
      mutate(runners_on_base = runner_on_1b + runner_on_2b + runner_on_3b) %>%
      mutate(runs_scored_on_play =
               ifelse(hit_type == 5, str_count(des, "scores"),
                      ifelse(hit_type == 3, str_count(des, "scores"),
                             ifelse(hit_type == 2, str_count(des, "scores"),
                                    ifelse(hit_type == 1, str_count(des, "scores"), 
                                           ifelse(hit_type == 4, runners_on_base+1, NA)))))) %>%
      mutate(runs_Road = ifelse(inning_topbot == "Top" & runs_scored_on_play > 0, runs_scored_on_play, 0)) %>%
      mutate(runs_Home = ifelse(inning_topbot == "Bot" & runs_scored_on_play > 0, runs_scored_on_play, 0))
      
    df <- df %>%
      mutate(spray_angle = round((atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75),1))
    df <- df %>%
      filter(!is.na(game_year))
    return(df)
  }
  df$game_date <- as.character(df$game_date)
  df <- df %>%
    arrange(game_date)
  df <- df %>%
    filter(!is.na(game_date))
  df <- df %>%
    ungroup()
  df <- df %>%
    select(setdiff(names(.), c("error")))
  
  data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  character_columns <- data_base_column_types %>%
    filter(class == "character") %>%
    pull(variable)
  
  numeric_columns <- data_base_column_types %>%
    filter(class == "numeric") %>%
    pull(variable)
  
  integer_columns <- data_base_column_types %>%
    filter(class == "integer") %>%
    pull(variable)
  
  df <- df %>%
    mutate_if(names(df) %in% character_columns, as.character) %>%
    mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
    mutate_if(names(df) %in% integer_columns, as.integer)
  df <- df %>%
    additional_info() %>%
    filter(neutral_dates == 0, !is.na(hit_distance_sc), runs_scored_on_play > 0, game_type == "R") %>%
    select(game_date, game_year, game_pk, NL_Park, AL_Park, home_team, away_team, des, events, stand, bb_type, hit_type, hc_x, hc_y, hit_distance_sc, spray_angle,
           launch_speed, launch_angle, launch_speed_angle, runners_on_base, runs_scored_on_play, runs_Home, runs_Road)
  return(df)
} # Creates new columns to make data easier to sort/filter

# --------------------------------------------------------------------------------------
# Import Format data for each season 
  
data2017 <- read.csv("pitch_combined_2017.csv")                                 # 2017 Season
data2018 <- read.csv("pitch_combined_2018.csv")                                 # 2018 Season
data2019 <- read.csv("pitch_combined_2019.csv")                                 # 2019 Season
all_data <- rbind(data2017,data2018,data2019)                                   # Binds all seasons into single dataframe
all_data1 <- format_data(all_data)

park_factor_processor <- function(season, handedness){
  
  parks <- read.csv("https://raw.githubusercontent.com/nlwood99/Senior-Thesis-Park-Factors/main/Record%20Logo%20Park%20Database.csv")
  record_updated <- parks %>% 
    mutate(homeGP2017 = h_wins2017 + h_loss2017) %>%
    mutate(roadGP2017 = r_wins2017 + r_loss2017) %>%
    mutate(home_winPCT2017 = h_wins2017 / (h_wins2017+h_loss2017)) %>%
    mutate(homeGP2018 = h_wins2018 + h_loss2018) %>%
    mutate(roadGP2018 = r_wins2018 + r_loss2018) %>%
    mutate(home_winPCT2018 = h_wins2018 / (h_wins2018+h_loss2018)) %>%
    mutate(homeGP2019 = h_wins2019 + h_loss2019) %>%
    mutate(roadGP2019 = r_wins2019 + r_loss2019) %>%
    mutate(home_winPCT2019 = h_wins2019 / (h_wins2019+h_loss2019))
  
  calc_data <- all_data %>%
    filter(game_year == season)
  ifelse(handedness = "left", LHB_factor_machine,
         ifelse(handedness = "right", RHB_factor_machine, total_factor_machine))
  
  
}
# --------------------------------------------------------------------------------------
# LHB Factor Process
parks <- read.csv("https://raw.githubusercontent.com/nlwood99/Senior-Thesis-Park-Factors/main/Record%20Logo%20Park%20Database.csv")
record_updated <- parks %>% 
  mutate(homeGP2017 = h_wins2017 + h_loss2017) %>%
  mutate(roadGP2017 = r_wins2017 + r_loss2017) %>%
  mutate(home_winPCT2017 = h_wins2017 / (h_wins2017+h_loss2017)) %>%
  mutate(homeGP2018 = h_wins2018 + h_loss2018) %>%
  mutate(roadGP2018 = r_wins2018 + r_loss2018) %>%
  mutate(home_winPCT2018 = h_wins2018 / (h_wins2018+h_loss2018)) %>%
  mutate(homeGP2019 = h_wins2019 + h_loss2019) %>%
  mutate(roadGP2019 = r_wins2019 + r_loss2019) %>%
  mutate(home_winPCT2019 = h_wins2019 / (h_wins2019+h_loss2019))


LHB_data <- all_data %>%
  filter(stand == "L")

LHBroad_runs_scored <- aggregate(LHB_data$runs_Road, by=list(Team=LHB_data$away_team), FUN=sum)
LHBroad_runs_allowed <- aggregate(LHB_data$runs_Home, by=list(Team=LHB_data$away_team), FUN=sum)
LHB_road_runs_total <- merge(LHBroad_runs_scored, LHBroad_runs_allowed,  by="Team")

LHBhome_runs_scored <- aggregate(LHB_data$runs_Home, by=list(Team=LHB_data$home_team), FUN=sum)
LHBhome_runs_allowed <- aggregate(LHB_data$runs_Road , by=list(Team=LHB_data$home_team), FUN=sum)
LHB_home_runs_total <- merge(LHBhome_runs_scored, LHBhome_runs_allowed,  by="Team")

LHB_all <- merge(LHB_home_runs_total, LHB_road_runs_total, by="Team")
LHB_all <- LHB_all %>%
  mutate(NL_Park = 
           ifelse(Team == "ARI",1,
                  ifelse(Team == "COL",1,
                         ifelse(Team == "LAD",1,
                                ifelse(Team == "SF",1,
                                       ifelse(Team == "SD",1,
                                              ifelse(Team == "CIN",1,
                                                     ifelse(Team == "CHC",1,
                                                            ifelse(Team == "MIL",1,
                                                                   ifelse(Team == "PIT",1,
                                                                          ifelse(Team == "STL",1,
                                                                                 ifelse(Team == "MIA",1,
                                                                                        ifelse(Team == "NYM",1,
                                                                                               ifelse(Team == "PHI",1,
                                                                                                      ifelse(Team == "ATL",1,
                                                                                                             ifelse(Team == "WSH",1,0)))))))))))))))) %>%
  mutate(AL_Park = 
           ifelse(Team == "BAL",1,
                  ifelse(Team == "BOS",1,
                         ifelse(Team == "NYY",1,
                                ifelse(Team == "TOR",1,
                                       ifelse(Team == "TB",1,
                                              ifelse(Team == "DET",1,
                                                     ifelse(Team == "KC",1,
                                                            ifelse(Team == "CLE",1,
                                                                   ifelse(Team == "CWS",1,
                                                                          ifelse(Team == "MIN",1,
                                                                                 ifelse(Team == "OAK",1,
                                                                                        ifelse(Team == "SEA",1,
                                                                                               ifelse(Team == "LAA",1,
                                                                                                      ifelse(Team == "HOU",1,
                                                                                                             ifelse(Team == "TEX",1,0))))))))))))))))
colnames(LHB_all) <- c("Team", "RHT", "OHT", "RAT", "OAT", "NL_Park", "AL_Park")

record_selector <- function(season){
  df <- record_updated %>%
    select(team_city, team_name, Team, logo_url, park_name, paste0("h_wins",season), paste0("h_loss",season), paste0("r_wins",season), paste0("r_loss",season), paste0("homeGP",season), paste0("roadGP",season)) %>%
    merge(LHB_all, by="Team")
  colnames(df) <- c("Team", "team_city", "team_name", "logo_url", "park_name", "h_wins", "h_loss", "r_wins", "r_loss", "homeGP", "roadGP", "RHT", "OHT", "RAT", "OAT", "NL", "AL")
  return(df)
}
combined <- record_selector(2017)

NL_data <- subset(combined, NL == 1)
AL_data <- subset(combined, AL == 1)

# Actual BPF Formula
NLruns_total <- sum(NL_data$RHT, NL_data$OHT)
NLGP_total <- sum(NL_data$h_wins, NL_data$h_loss, NL_data$r_wins, NL_data$r_loss)

RHT <- NL_data$RHT/NL_data$homeGP
OHT <- NL_data$OHT/NL_data$homeGP
RAT <- NL_data$RAT/NL_data$homeGP
OAT <- NL_data$OAT/NL_data$homeGP

NT <- 15
IF <- ((OHT+RHT)/(NL_data$h_wins + NL_data$h_loss)) / ((OAT+RAT)/(NL_data$r_wins + NL_data$r_loss))
IPC <- (18.5 - (NL_data$h_wins/(NL_data$h_wins + NL_data$h_loss))) / (18.5 - (NL_data$r_loss/(NL_data$r_wins + NL_data$r_loss)))
C <- IF / IPC
OPC <- NT / (NT-1+C)
SF <- C*OPC
SF1 <- 1 - ((SF-1)/(NT-1))
RAL <- (NLruns_total/(NLGP_total/2))

TBR <- (((RAT/SF1)+(RHT/SF))*(1+((1-1)/(NT-1))))/RAL
TPR <- (((RAT/SF1)+(RHT/SF))*(1+((TBR-1)/(NT-1))))/RAL
TBR2 <- (((RAT/SF1)+(RHT/SF))*(1+((TPR-1)/(NT-1))))/RAL
TPR2 <- (((RAT/SF1)+(RHT/SF))*(1+((TBR-1)/(NT-1))))/RAL
TBR3 <- (((RAT/SF1)+(RHT/SF))*(1+((TPR2-1)/(NT-1))))/RAL
TPR3 <- (((RAT/SF1)+(RHT/SF))*(1+((TBR2-1)/(NT-1))))/RAL

BPF <- (SF+SF1)/(2+((TPR3-1)/(NT-1)))


NL_calculator <- function(df = NL_data) {
  runs_total <- sum(df$RHT, df$OHT)
  GP_total <- sum(df$h_wins, df$h_loss, df$r_wins, df$r_loss)
  
  RHT <- df$RHT/df$homeGP
  OHT <- df$OHT/df$homeGP
  RAT <- df$RAT/df$homeGP
  OAT <- df$OAT/df$homeGP
  
  NT <- 15
  IF <- ((OHT+RHT)/(df$h_wins + df$h_loss)) / ((OAT+RAT)/(df$r_wins + df$r_loss))
  IPC <- (18.5 - (df$h_wins/(df$h_wins + df$h_loss))) / (18.5 - (df$r_loss/(df$r_wins + df$r_loss)))
  C <- IF / IPC
  OPC <- NT / (NT-1+C)
  SF <- C*OPC
  SF1 <- 1 - ((SF-1)/(NT-1))
  RAL <- (runs_total/(GP_total/2))
  
  TBR <- (((RAT/SF1)+(RHT/SF))*(1+((1-1)/(NT-1))))/RAL
  TPR <- (((RAT/SF1)+(RHT/SF))*(1+((TBR-1)/(NT-1))))/RAL
  TBR2 <- (((RAT/SF1)+(RHT/SF))*(1+((TPR-1)/(NT-1))))/RAL
  TPR2 <- (((RAT/SF1)+(RHT/SF))*(1+((TBR-1)/(NT-1))))/RAL
  TBR3 <- (((RAT/SF1)+(RHT/SF))*(1+((TPR2-1)/(NT-1))))/RAL
  TPR3 <- (((RAT/SF1)+(RHT/SF))*(1+((TBR2-1)/(NT-1))))/RAL
  
  BPF <- (SF+SF1)/(2+((TPR3-1)/(NT-1)))
  return(BPF)
}
NL <- NL_calculator()
NLchart <- data.frame(NL_data$Team, paste(NL_data$team_city, NL_data$team_name), NL_data$logo_url, NL_data$park_name, NL)
colnames(NLchart) <- c("Team_Code", "Team", "logo", "Park", "BPF")

AL_calculator <- function(df = AL_data) {
  runs_total <- sum(df$RHT, df$OHT)
  GP_total <- sum(df$h_wins, df$h_loss, df$r_wins, df$r_loss)
  
  RHT <- df$RHT/df$homeGP
  OHT <- df$OHT/df$homeGP
  RAT <- df$RAT/df$homeGP
  OAT <- df$OAT/df$homeGP
  
  NT <- 15
  IF <- ((OHT+RHT)/(df$h_wins + df$h_loss)) / ((OAT+RAT)/(df$r_wins + df$r_loss))
  IPC <- (18.5 - (df$h_wins/(df$h_wins + df$h_loss))) / (18.5 - (df$r_loss/(df$r_wins + df$r_loss)))
  C <- IF / IPC
  OPC <- NT / (NT-1+C)
  SF <- C*OPC
  SF1 <- 1 - ((SF-1)/(NT-1))
  RAL <- (runs_total/(GP_total/2))
  
  TBR <- (((RAT/SF1)+(RHT/SF))*(1+((1-1)/(NT-1))))/RAL
  TPR <- (((RAT/SF1)+(RHT/SF))*(1+((TBR-1)/(NT-1))))/RAL
  TBR2 <- (((RAT/SF1)+(RHT/SF))*(1+((TPR-1)/(NT-1))))/RAL
  TPR2 <- (((RAT/SF1)+(RHT/SF))*(1+((TBR-1)/(NT-1))))/RAL
  TBR3 <- (((RAT/SF1)+(RHT/SF))*(1+((TPR2-1)/(NT-1))))/RAL
  TPR3 <- (((RAT/SF1)+(RHT/SF))*(1+((TBR2-1)/(NT-1))))/RAL
  
  BPF <- (SF+SF1)/(2+((TPR3-1)/(NT-1)))
  return(BPF)
}
AL <- AL_calculator()
ALchart <- data.frame(AL_data$Team, paste(AL_data$team_city, AL_data$team_name), AL_data$logo_url, AL_data$park_name, AL)
colnames(ALchart) <- c("Team_Code", "Team", "logo", "Park", "BPF")

chart <- rbind(ALchart,NLchart) # this is what is going to be used to create the table 
chart <- aggregate(chart$BBPF, by=list(Team=chart$Team_Code), FUN=mean)  # sort factors largest to smallest 

LHB_factor_machine <- function(season) {
  LHB_data <- all_data %>%
    filter(stand == "L" & game_year == season)
  LHBroad_runs_scored <- aggregate(LHB_data$runs_Road, by=list(Team=LHB_data$away_team), FUN=sum)
  LHBroad_runs_allowed <- aggregate(LHB_data$runs_Home, by=list(Team=LHB_data$away_team), FUN=sum)
  LHB_road_runs_total <- merge(LHBroad_runs_scored, LHBroad_runs_allowed,  by="Team")
  
  LHBhome_runs_scored <- aggregate(all_data$runs_Home, by=list(Team=all_data$home_team), FUN=sum)
  LHBhome_runs_allowed <- aggregate(all_data$runs_Road , by=list(Team=all_data$home_team), FUN=sum)
  LHB_home_runs_total <- merge(LHBhome_runs_scored, LHBhome_runs_allowed,  by="Team")
  
  LHB_all <- merge(LHB_home_runs_total, LHB_road_runs_total, by="Team")
  colnames(LHB_all) <- c("Team", "RHT", "OHT", "RAT", "OAT")
  
  record_selector <- function(season){
    df <- parks %>%
      select(team_city, team_name, Team, logo_url, park_name, paste0("h_wins",season), paste0("h_loss",season), paste0("r_wins",season), paste0("r_loss",season)) %>%
      merge(LHB_all, by="Team")
    return(df)
  }                                      # Picks the proper home & road record based on season input + selections proper columns for calculation
  NL_data <- subset(combined, NL == 1)
  AL_data <- subset(combined, AL == 1)
  
  # Actual BPF Formula Calculations
  NL_calculator <- function(df = NL_data) {
    runs_total <- sum(df$RHT, df$OHT)
    GP_total <- sum(df$h_wins, df$h_loss, df$r_wins, df$r_loss)
    
    RHT <- df$RHT/df$homeGP
    OHT <- df$OHT/df$homeGP
    RAT <- df$RAT/df$homeGP
    OAT <- df$OAT/df$homeGP
    
    NT <- 15
    IF <- ((OHT+RHT)/(df$h_wins + df$h_loss)) / ((OAT+RAT)/(df$r_wins + df$r_loss))
    IPC <- (18.5 - (df$h_wins/(df$h_wins + df$h_loss))) / (18.5 - (df$r_loss/(df$r_wins + df$r_loss)))
    C <- IF / IPC
    OPC <- NT / (NT-1+C)
    SF <- C*OPC
    SF1 <- 1 - ((SF-1)/(NT-1))
    RAL <- (runs_total/(GP_total/2))
    
    TBR <- (((RAT/SF1)+(RHT/SF))*(1+((1-1)/(NT-1))))/RAL
    TPR <- (((RAT/SF1)+(RHT/SF))*(1+((TBR-1)/(NT-1))))/RAL
    TBR2 <- (((RAT/SF1)+(RHT/SF))*(1+((TPR-1)/(NT-1))))/RAL
    TPR2 <- (((RAT/SF1)+(RHT/SF))*(1+((TBR-1)/(NT-1))))/RAL
    TBR3 <- (((RAT/SF1)+(RHT/SF))*(1+((TPR2-1)/(NT-1))))/RAL
    TPR3 <- (((RAT/SF1)+(RHT/SF))*(1+((TBR2-1)/(NT-1))))/RAL
    
    BPF <- (SF+SF1)/(2+((TPR3-1)/(NT-1)))
    return(BPF)
  }
  NL <- NL_calculator()
  NLchart <- data.frame(NL_data$Team, paste(NL_data$team_city, NL_data$team_name), NL_data$logo_url, NL_data$park_name, NL)
  colnames(NLchart) <- c("Team_Code", "Team", "logo", "Park", "BPF")
  
  AL_calculator <- function(df = AL_data) {
    runs_total <- sum(df$RHT, df$OHT)
    GP_total <- sum(df$h_wins, df$h_loss, df$r_wins, df$r_loss)
    
    RHT <- df$RHT/df$homeGP
    OHT <- df$OHT/df$homeGP
    RAT <- df$RAT/df$homeGP
    OAT <- df$OAT/df$homeGP
    
    NT <- 15
    IF <- ((OHT+RHT)/(df$h_wins + df$h_loss)) / ((OAT+RAT)/(df$r_wins + df$r_loss))
    IPC <- (18.5 - (df$h_wins/(df$h_wins + df$h_loss))) / (18.5 - (df$r_loss/(df$r_wins + df$r_loss)))
    C <- IF / IPC
    OPC <- NT / (NT-1+C)
    SF <- C*OPC
    SF1 <- 1 - ((SF-1)/(NT-1))
    RAL <- (runs_total/(GP_total/2))
    
    TBR <- (((RAT/SF1)+(RHT/SF))*(1+((1-1)/(NT-1))))/RAL
    TPR <- (((RAT/SF1)+(RHT/SF))*(1+((TBR-1)/(NT-1))))/RAL
    TBR2 <- (((RAT/SF1)+(RHT/SF))*(1+((TPR-1)/(NT-1))))/RAL
    TPR2 <- (((RAT/SF1)+(RHT/SF))*(1+((TBR-1)/(NT-1))))/RAL
    TBR3 <- (((RAT/SF1)+(RHT/SF))*(1+((TPR2-1)/(NT-1))))/RAL
    TPR3 <- (((RAT/SF1)+(RHT/SF))*(1+((TBR2-1)/(NT-1))))/RAL
    
    BPF <- (SF+SF1)/(2+((TPR3-1)/(NT-1)))
    return(BPF)
  }
  AL <- AL_calculator()
  ALchart <- data.frame(AL_data$Team, paste(AL_data$team_city, AL_data$team_name), AL_data$logo_url, AL_data$park_name, AL)
  colnames(ALchart) <- c("Team_Code", "Team", "logo", "Park", "BPF")
  
  chart <- rbind(ALchart,NLchart)
  
# Need to run calculation & output to table w/ team abv + team name + logo 

}

# --------------------------------------------------------------------------------------
# RHB Factor Process
RHB_factor_machine <- function(season) {
  RHB_data <- calc_data %>%
    filter(stand == "R")
}

# Create Table of final results


# map function to go through 3 seasons of data and put them into the table 
# take average of all three factors for FINAL PF

