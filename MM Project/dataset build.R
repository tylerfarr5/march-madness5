library(hoopR)
library(dplyr)
library(lubridate)
library(stringr)

#############################################################################
#setting up all teams
#############################################################################

years <- 2008:2025
tourney_teams <- data.frame(season=integer(),
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
           field_goals_made.x, field_goals_attempted.x, assists.x, blocks.x, steals.x, 
           three_point_field_goals_made.x, three_point_field_goals_attempted.x,
           defensive_rebounds.x, offensive_rebounds.x, total_rebounds.x,
           fouls.x, 
           free_throws_made.x, free_throws_attempted.x,
           points_in_paint.x, fast_break_points.x, 
           turnover_points.x,turnovers.x,
           #opponent game stats
           opponent_team_id, opponent_team_short_display_name.x, opponent_team_score.x,
           field_goals_made.y, field_goals_attempted.y,
           three_point_field_goals_made.y, three_point_field_goals_attempted.y,
           turnovers.y, offensive_rebounds.y) %>%
  #renaming a couple columns
  rename(season = season.x,
          game_date = game_date.x,
         team_name = team_short_display_name.x,
         team_winner = team_winner.x,
         team_score = team_score.x,
         field_goals_made = field_goals_made.x,
         field_goals_attempted = field_goals_attempted.x,
         assists = assists.x,
         blocks = blocks.x,
         steals = steals.x,
         three_point_field_goals_made = three_point_field_goals_made.x,
         three_point_field_goals_attempted = three_point_field_goals_attempted.x,
         defensive_rebounds = defensive_rebounds.x,
         offensive_rebounds = offensive_rebounds.x,
         total_rebounds = total_rebounds.x,
         fouls = fouls.x,
         free_throws_made = free_throws_made.x,
         free_throws_attempted = free_throws_attempted.x,
         points_in_paint = points_in_paint.x,
         fast_break_points = fast_break_points.x,
         turnovers = turnovers.x,
         turnover_points = turnover_points.x,
         opp_team_name = opponent_team_short_display_name.x,
         opp_team_score = opponent_team_score.x,
         opp_field_goals_made = field_goals_made.y,
         opp_field_goals_attempted = field_goals_attempted.y,
         opp_three_point_field_goals_made = three_point_field_goals_made.y,
         opp_three_point_field_goals_attempted = three_point_field_goals_attempted.y,
         opp_turnovers = turnovers.y,
         opp_offensive_rebounds = offensive_rebounds.y
         ) %>%
  mutate(points_in_paint = as.integer(points_in_paint),
         turnover_points = as.integer(turnover_points),
         fast_break_points = as.integer(fast_break_points))



##################################################################################
### Wrangle data to get home/away formats
###################################################################################

temp <- stats_2ndhalf %>%
  #average up stats group by home and away
  group_by(season, team_id, team_home_away) %>%
  mutate(games = n(),
          fgpct = sum(field_goals_made)/sum(field_goals_attempted),
         threeptpct = sum(three_point_field_goals_made)/sum(three_point_field_goals_attempted),
         ftpct = sum(free_throws_made)/sum(free_throws_attempted),
         treb = mean(total_rebounds),
         oreb = mean(offensive_rebounds),
         dreb = mean(defensive_rebounds),
         ast = mean(assists),
         stl = mean(steals),
         blk = mean(blocks),
         to = mean(turnovers),
         fouls = mean(fouls),
         ppg = mean(team_score),
         opp_ppg = mean(opp_team_score),
         opp_fgpct = sum(opp_field_goals_made)/sum(opp_field_goals_attempted),
         opp_threeptpct = sum(opp_three_point_field_goals_made)/sum(opp_three_point_field_goals_attempted),
         fast_break_pts = mean(fast_break_points),
         pts_in_paint = mean(points_in_paint),
         wlpct = sum(team_winner)/n(),
         efgpct = mean((field_goals_made + (0.5*three_point_field_goals_made))/field_goals_attempted),
         mov = mean(team_score - opp_team_score),
         
         #formulas found online to estimate pace, efficiency, extra Scoring chances
         pace = mean(field_goals_attempted - offensive_rebounds + turnovers + (0.455 * free_throws_attempted)),
         unadj_off_eff = sum(team_score)/sum(field_goals_attempted - offensive_rebounds + turnovers + (0.455 * free_throws_attempted)),
         unadj_def_eff = sum(opp_team_score)/sum(field_goals_attempted - offensive_rebounds + turnovers + (0.455 * free_throws_attempted)),
         extraScoreChances = mean(offensive_rebounds + opp_turnovers - opp_offensive_rebounds - turnovers),
         
         #calculate pythagorean expected win % and luck using coefficient I calculated
         total_points = sum(team_score),
         total_opp_points = sum(opp_team_score),
         pyth_exp = sum(team_score)^8.8/(sum(team_score)^8.8 + sum(opp_team_score)^8.8),
         luck = mean(pyth_exp - wlpct),
         
         #additional ratios from talk with Russ Spicer
         oppTO_teamTO_ratio = mean(opp_turnovers/turnovers),
         ast_TO_ratio = mean(assists/turnovers),
         wins_close = sum(ifelse(abs(team_score- opp_team_score)<=5, team_winner, 0)),
         count_close = sum(ifelse(abs(team_score-opp_team_score)<=5, 1, 0))
  )


#Additional Variables:

##### Win Rate in Close Games - Adjusted for Sample Size using Bayesian Adjusted Win Rate (Shrinkage Method)
#Get the global win rate in close games (regardless of home or away)
global_win_rate_close_gm <- (temp %>%
                               group_by(team_home_away) %>%
                               summarise(win_rt = sum(wins_close)/sum(count_close)))[1,2][[1]]
#set beta to desired level
beta <- 10
#Calculate adjusted win rate
temp$adj_bayes_win_rate_close <- (temp$wins_close + global_win_rate_close_gm * beta) / (temp$count_close + beta)


##### Conference Tournament Performance
#Get the last game played in the regular season, played by each team (slice max fx)
last_conf_game <- temp %>%
  group_by(team_id) %>%
  slice_max(order_by = game_date, n=1, with_ties = FALSE) %>%
  #uses stringR notation to clean up conference performance to easy to interpret format
  mutate(cleaned = str_remove(notes_headline, " AT .*") ,
         clean_conf = str_extract(cleaned, "[^-]+$"),
         conf1 = str_replace_all(clean_conf, "(?i)\\b[1-4](st|nd|rd|th)? Round\\b", "Early Round"),
         conf1 = str_replace_all(conf1, regex("\\bQtrfinals\\b", ignore_case = TRUE), "Quarterfinals"),
         conf1 = str_replace_all(conf1, regex("\\bQuarterfinal\\b", ignore_case = TRUE), "Quarterfinals"),
         conf1 = str_replace_all(conf1, regex("\\bSemis\\b", ignore_case = TRUE), "Semifinals"),
         conf1 = str_replace_all(conf1, regex("\\bSemi-final\\b", ignore_case = TRUE), "Semifinals"),
         conf1 = str_replace_all(conf1, regex("\\bQtrfinals\\b", ignore_case = TRUE), "Quarterfinals")) %>%
  #concatenate to conference performance
  mutate(conf_perf = paste(conf1, ifelse(team_winner == TRUE, "W", "L"), sep = " - ")) %>%
  select(season, team_id, conf_perf) 

#adding this variable to dataframe
temp <- temp %>%
  left_join(last_conf_game, by = c('team_id' = 'team_id', 'season' = 'season'))


##### Players who Avg 15+ Minutes (aka "starters")
players <- load_mbb_player_box()

#need a neutral site flag for these games
neutral_site_flag <- team_game_info %>%
  select(id, game_date, season, neutral_site)


players_filtered <- players %>%
  inner_join(date_ranges, by = c('season' = 'year')) %>%
  left_join(neutral_site_flag, by = c('game_id' = 'id', 'season' = 'season')) %>%
  #2nd half of season
  filter(game_date.x >= start_date & game_date.x <= end_date) %>%
  #setting all neutral site games to away for both teams
  mutate(team_home_away = ifelse(neutral_site == TRUE, "away", home_away)) %>%
  group_by(season, team_id, athlete_id, team_home_away) %>%
  #grabbing season averages for these players. use na.rm = TRUE for players who didn't play
  dplyr::summarise(games = n(),
            games_played = sum(!is.na(minutes) & minutes >0),
            minutes_ = sum(minutes, na.rm = TRUE),
            three_point_made = sum(three_point_field_goals_made, na.rm = TRUE),
            three_point_att = sum(three_point_field_goals_attempted, na.rm = TRUE),
            mpg = minutes_/games_played,
            three_point_clip = three_point_made/three_point_att)


#want players who have at least 15 minutes per game on the season @ home or away
players_filtered <- players_filtered %>%
  group_by(athlete_id) %>%
  #the "any" argument counts both entries for someone who had 17 mpg at home and 12 mpg at away. 
  filter(all(mpg >= 15)) %>%
  ungroup()

#want players who have played at least 3 Home and 3 Away games this season
players_filtered <- players_filtered %>%
  group_by(athlete_id) %>%
  filter(all(games_played >= 3) & all(minutes_ >= 45)) %>%  # removes ID if played < 3 games home or away
  ungroup()

#aggregate to team level
players_stats <- players_filtered %>%
  mutate(tpp = ifelse(three_point_clip >= 0.35,1,0)) %>%
  group_by(season, team_id, team_home_away) %>%
  summarise(starters = n(),
            threes35 = sum(tpp, na.rm = TRUE))

players_stats %>%
  +   group_by(team_id) %>%
  +   summarise(
    +     unique_starters = n_distinct(starters),
    +     .groups = "drop"
    +   ) %>%
  +   filter(unique_starters > 1)


^^figure this out:
  - need to decide if you use "any" or "all" in mpg >=15 filter (ie: UCF has 4 'starters', not 5)
  - need to decide if you want to split this part out by home/away (ie Florida shows reverse result) -> base = took a 3 pt??

#######
## variables to add
#######
#adjust global win rates for home and away - should be different for each
#num players who avg 15+ min (aka "starters")
#avg age of team for everyone w 15+ min
# num shooters above 35% 3 pt for everyon w 15+ min

#then build loop for all other years & join to tourney_teams df


#######
## possible ways to improve
#######
#break out stats by Home | Away | Total,, for First and Second Half of Year


#######
## next steps in EDA
#######
#show investigation of any teams with proportionally high home/away games
#decide on which bart torvik stats to inclue (if any)
#home/away matrix




         %>%
  #pivot the data to show home and away stats
  pivot_wider(
    id_cols = game_id,
    names_from = team_home_away,
    values_from = c(points, assists, steals),
    names_glue = "{team_home_away} {.value}"
  )

  
#summarize this df by team (team_id goes up to 130k. looop thru to the max team id)

View(stats_2ndhalf %>%
       group_by(team_id) %>%
       summarise(count = n()))
  filter(team_id == 130))
  
