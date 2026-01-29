years <- 2018:2025
tourney_teams <- data.frame(season=integer(),
                            home_id=integer(), 
                            home_short_display_name=character(), 
                            stringsAsFactors=FALSE)


#tournament ID 22 = mrach madness tourney
#pulling all tournament games since 2008
for (i in years){
  temp_yr <- load_mbb_schedule(i)
  
  #home
  temp_hm <- temp_yr %>%
    filter(tournament_id == 22) %>%
    dplyr::select(home_id, home_short_display_name)
  #away
  temp_aw <- temp_yr %>%
    filter(tournament_id == 22) %>%
    dplyr::select(away_id, away_short_display_name)
  #binding them together, use.names ignores column names
  temp_tms <- rbind(temp_hm, temp_aw, use.names = FALSE)
  
  #add col for year, sel distinct
  temp_tms <- distinct(
    temp_tms %>%
      mutate(season = i) %>%
      select(season, home_id, home_short_display_name)
  )
  
  tourney_teams <- rbind(tourney_teams, temp_tms)
}

tourney_teams <- tourney_teams %>%
  rename(team_id = home_id,
         team_name = home_short_display_name)


##################################################################################
### Joining in the team stats 
###################################################################################

#all tourney dates the *Monday* before tourney starts
tourney_dates <- as.Date(c("2008-03-17", "2009-03-16","2010-03-15", "2011-03-14", 
                           "2012-03-12", "2013-03-18", "2014-03-17","2015-03-16", "2016-03-14",
                           "2017-03-13", "2018-03-12", "2019-03-18","2021-03-15", "2022-03-14", 
                           "2023-03-13", "2024-03-18", "2025-03-17"))

# create a lookup table with start and end dates per year
#want Jan 1 - start of MM tourney. this is the 2nd half of the season
date_ranges <- tibble(
  year = year(tourney_dates),
  start_date = as.Date(paste0(year(tourney_dates), "-01-01")),
  end_date = tourney_dates
)



all_stats <- map_df(years, function(season_year){
  
  szn_stats <- load_mbb_team_box(seasons = season_year)
  team_game_info <- load_mbb_schedule(seasons = season_year)
  
  
  # filter the dataframe using a join and range check so I get 2nd half of season stats
  #2 rows for each game (1 for each team)
  #gets season stats, filters to 2nd half of season, inner join opponent stats, and left join home/away team stats
  stats_2ndhalf <- szn_stats %>%
    inner_join(date_ranges, by = c('season' = 'year')) %>%
    inner_join(szn_stats, by = c('game_id' = 'game_id', 'opponent_team_id' = 'team_id')) %>%
    left_join(team_game_info, by = c('game_id' = 'id', 'team_id' = 'home_id')) %>%
    left_join(team_game_info, by =c('game_id' = 'id', 'team_id' = 'away_id')) %>%
    #2nd half of season
    filter(game_date.x >= start_date.x & game_date.x <= end_date) %>%
    #trimming the duplicated columns & setting all neutral site games to away for both teams
    mutate(neutral_site = coalesce(neutral_site.x, neutral_site.y),
           notes_headline = coalesce(notes_headline.x, notes_headline.y),
           team_home_away = ifelse(neutral_site == TRUE, "away", team_home_away.x),
           tournament_id = coalesce(tournament_id.x, tournament_id.y)) %>%
    #selecting desired columns
    select  ( #generic game/team info
      game_id, season.x, game_date.x, team_id, team_short_display_name.x,team_home_away,
      neutral_site, notes_headline, tournament_id,
      #team stats
      team_winner.x, team_score.x,
      #opponent game stats
      opponent_team_id, opponent_team_short_display_name.x, opponent_team_score.x) %>%
    #renaming a couple columns
    rename(season = season.x,
           game_date = game_date.x,
           team_name = team_short_display_name.x,
           team_winner = team_winner.x,
           team_score = team_score.x,
           opp_team_name = opponent_team_short_display_name.x,
           opp_team_score = opponent_team_score.x
    ) 
  
  
  
  ##################################################################################
  ### Wrangle data to get home/away formats
  
  #temp is showing every game of the season for each team. shows each teams stats grouped by season and home/away
  #I filter to only the tournament teams here too. Not interested in the rest of the league
  ###################################################################################
  
  
  stats_2ndhalf <- stats_2ndhalf %>%
    #average up stats group by home and away
    group_by(season, team_id, team_home_away) %>%
    mutate(games = n(),

           wlpct = sum(team_winner)/n(),

           #calculate pythagorean expected win % and luck using coefficient I calculated
           total_points = sum(team_score),
           total_opp_points = sum(opp_team_score)) %>%
    mutate(team_home_away = as.character(team_home_away))
  
  return(stats_2ndhalf)
           

})










mm <- all_stats %>%
  select(season, team_id, team_name, team_home_away, games, wlpct, total_points, total_opp_points) %>%
  distinct()

#Optimal is 11



################################################################
### HOME
#################################################################

mm_home <- mm %>%
  filter(team_home_away == "home")



#Logic to determine pythagorean expectation variable - #grid searching all possible values from 2 to 20 by 0.1
num_list <- seq(2,20,0.1)
#dict <- data.frame(Exponent = as.numeric(), MAE_A = as.numeric(), MAE_B = as.numeric(), Diff = as.numeric())
dict_home <- data.frame(Exponent = as.numeric(), MAE = as.numeric())

for (i in num_list) {
  
  pythag <- (mm_home$total_points^i)/(mm_home$total_points^i + mm_home$total_opp_points^i)
  
  mae <- mean(abs(mm_home$wlpct - pythag))
  
  #pythag_A <- (mm$A_TotalPoints^i)/(mm$A_TotalPoints^i + mm$A_TotalOppPoints^i)
  #pythag_B <- (mm$B_TotalPoints^i)/(mm$B_TotalPoints^i + mm$B_TotalOppPoints^i)
  
  
  #Amae_A <- mean(abs(mm$A_WLPct_PRE - pythag_A))
  #mae_B <- mean(abs(mm$B_WLPct_PRE - pythag_B))
  
  dict_home[nrow(dict_home)+1,] <- c(i, mae)
  
  #dict[nrow(dict)+1,] <- c(i, mae_A, mae_B, mae_A + mae_B)
}


################################################################
### AWAY
#################################################################

mm_away <- mm %>%
  filter(team_home_away == "away")


num_list <- seq(2,20,0.1)
#dict <- data.frame(Exponent = as.numeric(), MAE_A = as.numeric(), MAE_B = as.numeric(), Diff = as.numeric())
dict_away <- data.frame(Exponent = as.numeric(), MAE = as.numeric())

for (i in num_list) {
  
  pythag <- (mm_away$total_points^i)/(mm_away$total_points^i + mm_away$total_opp_points^i)
  
  mae <- mean(abs(mm_away$wlpct - pythag))
  
  #pythag_A <- (mm$A_TotalPoints^i)/(mm$A_TotalPoints^i + mm$A_TotalOppPoints^i)
  #pythag_B <- (mm$B_TotalPoints^i)/(mm$B_TotalPoints^i + mm$B_TotalOppPoints^i)
  
  
  #Amae_A <- mean(abs(mm$A_WLPct_PRE - pythag_A))
  #mae_B <- mean(abs(mm$B_WLPct_PRE - pythag_B))
  
  dict_away[nrow(dict_away)+1,] <- c(i, mae)
  
  #dict[nrow(dict)+1,] <- c(i, mae_A, mae_B, mae_A + mae_B)
}
