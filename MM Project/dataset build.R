library(hoopR)
library(dplyr)
library(lubridate)

#############################################################################
#setting up all teams
#############################################################################

years <- 2008:2025
teams <- data.frame(season=integer(),
                            home_id=integer(), 
                            home_short_display_name=character(), 
                            stringsAsFactors=FALSE)

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
  
  teams <- rbind(teams, temp_tms)
}

teams <- teams %>%
  rename(team_id = home_id,
          team_name = home_short_display_name)


##################################################################################
### Joining in the team stats 
###################################################################################

#all tourney dates the Monday before tourney starts
tourney_dates <- as.Date(c("2008-03-17", "2009-03-16","2010-03-15", "2011-03-14", 
                    "2012-03-12", "2013-03-18", "2014-03-17","2015-03-16", "2016-03-14",
                    "2017-03-13", "2018-03-12", "2019-03-18","2021-03-15", "2022-03-14", 
                    "2023-03-13", "2024-03-18", "2025-03-17"))

# create a lookup table with start and end dates per year
date_ranges <- tibble(
  year = year(tourney_dates),
  start_date = as.Date(paste0(year(tourney_dates), "-01-01")),
  end_date = tourney_dates
)


szn_stats <- load_mbb_team_box()
team_game_info <- load_mbb_schedule()


# filter the dataframe using a join and range check so I get 2nd half of season stats
#2 rows for each game (1 for each team)
stats_2ndhalf <- szn_stats %>%
  inner_join(date_ranges, by = c('season' = 'year')) %>%
  left_join(team_game_info, by = c('game_id' = 'id', 'team_id' = 'home_id')) %>%
  left_join(team_game_info, by =c('game_id' = 'id', 'team_id' = 'away_id')) %>%
  #2nd half of season
  filter(game_date.x >= start_date.x & game_date.x <= end_date) %>%
  #trimming the duplicated columns & setting all neutral site games to away for both teams
  mutate(neutral_site = coalesce(neutral_site.x, neutral_site.y),
         conference_competition = coalesce(conference_competition.x, conference_competition.y),
         notes_headline = coalesce(notes_headline.x, notes_headline.y),
         team_home_away = ifelse(neutral_site == TRUE, "away", team_home_away)) %>%
  #selecting desired columns
  select  ( #generic game/team info
            game_id, season.x, game_date.x, team_id, team_short_display_name,team_home_away,
           neutral_site, conference_competition, notes_headline,
           #team stats
           team_winner, team_score,
           field_goals_made, field_goals_attempted, assists, blocks, steals, 
           three_point_field_goals_made, three_point_field_goals_attempted,
           defensive_rebounds, offensive_rebounds, total_rebounds,
           flagrant_fouls, technical_fouls, fouls, 
           free_throws_made, free_throws_attempted,
           points_in_paint, fast_break_points, 
           team_turnovers, total_turnovers, turnover_points,turnovers,
           #opponent game stats
           opponent_team_id, opponent_team_short_display_name, opponent_team_score) %>%
  #renaming a couple columns
  rename(season = season.x,
          game_date = game_date.x) %>%
  #pivoting the tables to show home and away stats
  pivot_wider(
    names_from = c(game_id, season, game_date, team_id, 
    values_from = c(points, rebounds)
  )

#summarize this df by team (team_id goes up to 130k. looop thru to the max team id)

stats_2ndhalf <- stats_2ndhalf %>%
  filter(team_id == 130) %>%
  select(game_id, season, game_date, team_id, team_short_display_name, team_home_away,
          team_winner, team_score,
          field_goals_made, field_goals_attempted, assists, blocks, steals, 
         three_point_field_goals_made, three_point_field_goals_attempted,
         defensive_rebounds, offensive_rebounds, total_rebounds,
         flagrant_fouls, technical_fouls, fouls, 
         free_throws_made, free_throws_attempted,
         points_in_paint, fast_break_points, 
         team_turnovers, total_turnovers, turnover_points,turnovers,
         
         opponent_team_id, opponent_team_short_display_name, opponent_team_score
         )
