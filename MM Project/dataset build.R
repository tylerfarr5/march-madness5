library(hoopR)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(purrr)
library(glue)
library(progress)

#############################################################################
#setting up all teams
#############################################################################

years <- 2008:2025
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


szn_stats <- load_mbb_team_box(seasons = 2025)
team_game_info <- load_mbb_schedule(seasons = 2025)
players <- load_mbb_player_box(seasons = 2025)

#need a neutral site flag for these games
neutral_site_flag <- team_game_info %>%
  select(id, game_date, season, neutral_site)



################################################################################
################################################################################
### Player Seniority by Seasons Played - Lookup Table
################################################################################
################################################################################


####Player Seniority Logic:
#pull every player from 2003 to present
player_exp <- NULL
for (zzz in 2003:2026) {
  
  hold <- load_mbb_player_box(seasons = zzz) %>%
    inner_join(date_ranges, by = c('season' = 'year')) %>%
    #pre-tourney
    filter(game_date <= end_date) %>%
    select(athlete_id, season) %>%
    distinct() 
  
  player_exp <- rbind(player_exp, hold)
  
}

#use row number function to calculate the number of times the athlete id shows up
#show up twice? == sophomore
#show up four? == senior
player_exp <- player_exp %>%
  arrange(athlete_id, season) %>%
  group_by(athlete_id) %>%
  mutate(years_exp = row_number()) %>%
  ungroup()



#filter player data to desired timeframe and stats
seniority_df <- players %>%
  inner_join(date_ranges, by = c('season' = 'year')) %>%
  left_join(neutral_site_flag, by = c('game_id' = 'id', 'season' = 'season')) %>%
  #2nd half of season - who's starting now
  filter(game_date.x >= start_date & game_date.x <= end_date) %>%
  #setting all neutral site games to away for both teams
  mutate(team_home_away = ifelse(neutral_site == TRUE, "away", home_away)) %>%
  group_by(season, team_id, athlete_id, team_home_away) %>%
  #grabbing season averages for these players. use na.rm = TRUE for players who didn't play
  dplyr::summarise(games = n(),
                   games_played = sum(!is.na(minutes) & minutes >0),
                   minutes_ = sum(minutes, na.rm = TRUE),
                   mpg = minutes_/games_played)

#want players who have at least 15 minutes per game on the season @ home or away
seniority_df <- seniority_df %>%
  group_by(athlete_id) %>%
  #the "any" argument counts both entries for someone who had 17 mpg at home and 12 mpg at away. 
  filter(any(mpg >= 15)) %>%
  ungroup()

#want players who have played at least 3 Home, 3 Away games & at least 45 total min this season
seniority_df <- seniority_df %>%
  group_by(athlete_id) %>%
  filter(all(games_played >= 3) & all(minutes_ >= 45)) %>% 
  ungroup()


#### Adding a stat: seniority of team
player_exp_lookup <- seniority_df %>%
  select(season, team_id, athlete_id) %>%
  distinct %>%
  left_join(player_exp, by = c('season' = 'season', 'athlete_id' = 'athlete_id')) %>%
  group_by(season, team_id) %>%
  summarise(avg_exp = mean(years_exp, na.rm = TRUE))



################################################################################
################################################################################
### Starter - Minute Discrepancy Index
### Ex: 2024 Kentucky. Dillingham and Sheppard were top freshman, but never started games.
#ie: james franklin rule 
################################################################################
################################################################################

### SMDI score:
#0 <- Same 5 players start and play the most
#0.4 <- 2 of top minute players come off bench
#1.0 <- Lineup starters ≠ rotation core


build_smdi_season <- function(season_year) {
  
  load_mbb_player_box(seasons = season_year) %>%
    
    inner_join(date_ranges, by = c("season" = "year")) %>%
    
    # pre-tournament only
    filter(game_date <= end_date) %>%
    
    group_by(season, team_id, athlete_id, athlete_display_name) %>%
    summarise(
      games_started = sum(starter, na.rm = TRUE),
      avg_min = mean(minutes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    group_by(season, team_id) %>%
    mutate(
      starter_rank = rank(-games_started, ties.method = "first"),
      minutes_rank = rank(-avg_min, ties.method = "first")
    ) %>%
    
    summarise(
      smdi =
        1 - length(
          intersect(
            athlete_id[starter_rank <= 5],
            athlete_id[minutes_rank <= 5]
          )
        ) / 5,
      .groups = "drop"
    ) %>%
    
    select(season, team_id, smdi)
}


team_rotation_metrics <- map_dfr(2008:2026, build_smdi_season)





################################################################################
################################################################################
### Coach DF - does the coach have an impact on the outcome of the game
### **Scraped from ProSportsReference**
################################################################################
################################################################################

#Function to scrape every coach during march madness
scrape_coaches_season <- function(season_range) {
  url <- glue::glue("https://www.sports-reference.com/cbb/seasons/men/{season_range}-coaches.html")
  page <- GET(url, user_agent("Mozilla/5.0"))
  
  if (http_error(page)) return(NULL)
  
  html <- read_html(page)
  table_node <- html %>% html_node("table")
  if (is.null(table_node)) return(NULL)
  
  df <- table_node %>%
    html_table(fill = TRUE) %>%
    clean_names() %>%
    # This regex removes the "x2007_08_" prefix from column names
    rename_with(~ str_replace(., "^x\\d{4}_\\d{2}_", "")) %>%
    mutate(year = season_range) %>%
    # Convert all to character temporarily to ensure smooth binding
    mutate(across(everything(), as.character))
  
  df
}


#Set the timeframe for the coach
raw_coaches <- map_dfr(2008:2026, scrape_coaches_season)

coaches_final <- raw_coaches %>%
  # Filter out the sub-header rows found in the middle of the table
  filter(
    !str_detect(x, "Coach"),
    !is.na(x),
    x != ""
  ) %>%
  select(
    coach = x,
    school = x_2,
    year,
    
    current_w = season,
    current_l = season_2,
    
    tourney_perf = season_6,
    
    curr_school_career_w = career_at_current_school_2,
    curr_school_career_l = career_at_current_school_3,
    
    curr_school_career_ncaa_app = career_at_current_school_5,
    curr_school_career_s16_app = career_at_current_school_6,
    curr_school_career_f4_app = career_at_current_school_7,
    curr_school_career_champ = career_at_current_school_8,
    
    career_w = career_overall,
    career_l = career_overall_2,
    
    career_ncaa_app = career_overall_4,
    career_s16_app = career_overall_5,
    career_f4_app = career_overall_6,
    career_champ = career_overall_7,
    
  ) %>%
  # Convert numeric columns back to numbers
  mutate(across(c(year, starts_with("curr"), starts_with("career")), parse_number)) %>%
  mutate(coach = str_remove(coach, " \\*"))


# *** Data cleanup to fix tourney teams that had multiple coaches in season
#ie: Indiana HC Kelvin Sampson stepped down midseason for recruiting violations
coaches_final %>%
  filter(tourney_perf != "") %>%
  group_by (year, school) %>%
  summarise(count = n()) %>%
  filter(count > 1)

#Coaches to remove:
#2008 Indiana Kelvin Sampson
#2012 Western Kentucky Ken McDonald
#2016 Wisconsin Bo Ryan
#2019 LSU Will Wade
#2022 LSU Will Wade
#2023 Kansas Bil Self
#2023 Texas Chris Beard
#2024 McNeese State Vernon Hamilton
#2024 McNeese State Brandon Chambers
coaches_final <- coaches_final %>%
  filter(tourney_perf != "") %>%
  filter(!(coach == "Kelvin Sampson" & school == "Indiana" & year == 2008)) %>%
  filter(!(coach == "Ken McDonald" & school == "Western Kentucky" & year == 2012)) %>%
  filter(!(coach == "Bo Ryan" & school == "Wisconsin" & year == 2016)) %>%
  filter(!(coach == "Will Wade" & school == "LSU" & year == 2019)) %>%
  filter(!(coach == "Will Wade" & school == "LSU" & year == 2022)) %>%
  filter(!(coach == "Bill Self" & school == "Kansas" & year == 2023)) %>%
  filter(!(coach == "Chris Beard" & school == "Texas" & year == 2023)) %>%
  filter(!(coach == "Vernon Hamilton" & school == "McNeese State" & year == 2024)) %>%
  filter(!(coach == "Brandon Chambers" & school == "McNeese State" & year == 2024))


coaches_final <- coaches_final %>%
  #builds out variables for coach
  mutate(adj_curr_school_career_w = curr_school_career_w - current_w, #total wins - current season wins. can't count current szn since it uses tourney perf
         adj_curr_school_career_l = curr_school_career_l - current_l, #total losses - current season losses. can't count current szn since it uses tourney perf
         adj_career_w = career_w - current_w, #career wins - current season wins
         adj_career_l = career_l - current_l, #career losses - current season losses
         
         
         adj_curr_school_career_ncaa_app = curr_school_career_ncaa_app-1,
         
         adj_curr_school_career_s16_app = ifelse(
           !tourney_perf %in% c("Lost First Four", "Lost First Round", "Lost Second Round"),
           curr_school_career_s16_app - 1,
           curr_school_career_s16_app),
         
         adj_curr_school_career_f4_app = ifelse(
           tourney_perf %in% c("Lost National Semifinal", "Lost National Final", "Won National Final"),
           curr_school_career_f4_app - 1,
           curr_school_career_f4_app),
         
         adj_curr_school_career_champ = ifelse(
           tourney_perf == "Won National Final",
           curr_school_career_champ - 1,
           curr_school_career_champ),
         
         adj_career_ncaa_app = career_ncaa_app-1,
         
         adj_career_s16_app = ifelse(
           !tourney_perf %in% c("Lost First Four", "Lost First Round", "Lost Second Round"),
           career_s16_app - 1,
           career_s16_app),
         
         adj_career_f4_app = ifelse(
           tourney_perf %in% c("Lost National Semifinal", "Lost National Final", "Won National Final"),
           career_f4_app - 1,
           career_f4_app),
         
         adj_career_championships = ifelse(
           tourney_perf == "Won National Final", 
           career_champ - 1, 
           career_champ),
         
         total_games_curr_school = adj_curr_school_career_w + adj_curr_school_career_l,
         total_games_career = adj_career_w + adj_career_l
         
  ) %>%
  #turns all numeric columns with NA's to 0
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x))
  ) %>%
  
  #grouping by year to handle mean win pct relative to that year
  group_by(year) %>%
  
  #data transformation - turning data to valuable variables for model
  mutate(
    coach_career_log_games_total = log1p(total_games_career),
    log_games_school = log1p(total_games_curr_school),
    
    # Win rates --> I impute a 50% win rate if they have never coached before
    win_pct_total =
      if_else(total_games_career > 0,
              adj_career_w / total_games_career,
              0.5),
    
    win_pct_school =
      if_else(total_games_curr_school > 0,
              adj_curr_school_career_w / total_games_curr_school,
              0.5),
    
    march_score_total = 
      1.0 * adj_career_ncaa_app +
      2.0 * adj_career_s16_app +
      3.5 * adj_career_f4_app +
      5.0 * adj_career_championships,
    
    march_score_school =
      1.0 * adj_curr_school_career_ncaa_app +
      2.0 * adj_curr_school_career_s16_app +
      3.5 * adj_curr_school_career_f4_app +
      5.0 * adj_curr_school_career_champ,
    
    march_eff_total =
      march_score_total / pmax(adj_career_ncaa_app, 1),
    
    coach_curr_school_march_eff = 
      march_score_school / pmax(adj_curr_school_career_ncaa_app, 1),
    
    exp_weight = pmin(coach_career_log_games_total / log(400), 1),
    
    coach_career_adj_win_pct =
      exp_weight * win_pct_total +
      (1 - exp_weight) * mean(win_pct_total, na.rm = TRUE),
    
    coach_career_adj_march_eff =
      exp_weight * march_eff_total
    
  ) %>%
  
  ungroup() %>%
  
  select(coach, school, year,
         coach_career_adj_win_pct, 
         coach_career_log_games_total,
         coach_career_adj_march_eff,
         coach_curr_school_march_eff)
# adj_win_pct, #baseline, win percent over career
# log_games_total, #total career games coached... how much basketball have they seen?
# adj_march_eff, #weight of lifetime success in  MM tourney * log_games_total
# march_eff_school #current school fit.. how well have you done in MM tourney with this school



team_lookup <- bind_rows(
team_game_info %>%  select(team_id = home_id, team_name = home_location), 
team_game_info %>%  select(team_id = away_id, team_name = away_location)
) %>%
  distinct()


coach_lookup <- coaches_final %>%
  mutate(school = recode(
    school,
    "Albany (NY)" = "UAlbany",
    "American" = "American University",
    "Appalachian State" = "App State",
    "College of Charleston" = "Charleston",
    "ETSU" = "East Tennessee State",
    "FDU" = "Fairleigh Dickinson",
    "Hawaii" = "Hawai'i",
    "LIU" = "Long Island University",
    "Long Beach State" = "Long Beach State",
    "Loyola (IL)" = "Loyola Chicago",
    "Loyola (MD)" = "Loyola Maryland",
    "New Orleans" = "Loyola New Orleans",
    "McNeese State" = "McNeese",
    "Miami (FL)" = "Miami",
    "Penn" = "Pennsylvania",
    "Pitt" = "Pittsburgh",
    "Prairie View" = "Prairie View A&M",
    "Saint Francis (PA)" = "Saint Francis",
    "SIU-Edwardsville" = "SIU Edwardsville",
    "St. John's (NY)" = "St. John's",
    "St. Joseph's" = "Saint Joseph's",
    "St. Peter's" = "Saint Peter's",
    "UC-Davis" = "UC Davis",
    "UC-Irvine" = "UC Irvine",
    "UC-San Diego" = "UC San Diego",
    "UCSB" = "UC Santa Barbara",
    "UMass" = "Massachusetts",
    "UNC" = "North Carolina",
    .default = school
  )) %>%
  left_join(team_lookup, by = c('school' = 'team_name'))




################################################################################
################################################################################
### Splitting data by 2nd Half of year stats, by home/away as well
################################################################################
################################################################################


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
           field_goals_made.x, field_goals_attempted.x, assists.x, blocks.x, steals.x, 
           three_point_field_goals_made.x, three_point_field_goals_attempted.x,
           defensive_rebounds.x, offensive_rebounds.x, total_rebounds.x,
           fouls.x, 
           free_throws_made.x, free_throws_attempted.x,
           turnovers.x,
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
         turnovers = turnovers.x,
         opp_team_name = opponent_team_short_display_name.x,
         opp_team_score = opponent_team_score.x,
         opp_field_goals_made = field_goals_made.y,
         opp_field_goals_attempted = field_goals_attempted.y,
         opp_three_point_field_goals_made = three_point_field_goals_made.y,
         opp_three_point_field_goals_attempted = three_point_field_goals_attempted.y,
         opp_turnovers = turnovers.y,
         opp_offensive_rebounds = offensive_rebounds.y
         ) 



##################################################################################
### Wrangle data to get home/away formats

#temp is showing every game of the season for each team. shows each teams stats grouped by season and home/away
#I filter to only the tournament teams here too. Not interested in the rest of the league
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
         pers_fouls = mean(fouls),
         ppg = mean(team_score),
         opp_ppg = mean(opp_team_score),
         opp_fgpct = sum(opp_field_goals_made)/sum(opp_field_goals_attempted),
         opp_threeptpct = sum(opp_three_point_field_goals_made)/sum(opp_three_point_field_goals_attempted),
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
         pyth_exp = sum(team_score)^11/(sum(team_score)^11 + sum(opp_team_score)^11),
         luck = mean(pyth_exp - wlpct),
         
         #additional ratios from talk with Russ Spicer
         oppTO_teamTO_ratio = sum(opp_turnovers)/sum(turnovers),
         ast_TO_ratio = sum(assists)/sum(turnovers),
         wins_close = sum(ifelse(abs(team_score- opp_team_score)<=5, team_winner, 0)),
         count_close = sum(ifelse(abs(team_score-opp_team_score)<=5, 1, 0)),
         count_blowout = sum(ifelse(abs(team_score-opp_team_score)>=20, 1, 0))
  ) %>% #only want tourney teams; creates flag to find them
  left_join(tourney_teams, by = c('season' = 'season', 'team_id' = 'team_id')) %>%
    mutate(tourney_team_flag = ifelse(!is.na(team_name.y),1,0)) %>%
    select(-team_name.y) %>%
    rename(team_name = team_name.x)



#Additional Variables:

##### Win Rate in Close Games - Adjusted for Sample Size using Bayesian Adjusted Win Rate (Shrinkage Method)
#Get the global win rate in close games
#select distinct teams to get a global win % across all teams in conf
# i am assuming if they play in 0 cloes games, then they have the ncaa/tourney teams average in winning close games

global_win_rate_close_gm_hm <- temp %>%
  filter(team_home_away == "home" & tourney_team_flag ==1) %>%
  select(season, team_id, team_name, wins_close, count_close) %>%
  distinct() %>%
  group_by(season) %>%
  mutate(close_win_pct = wins_close/count_close)

global_win_rate_close_gm_aw <- temp %>%
  filter(team_home_away == "away"  & tourney_team_flag ==1) %>%
  select(season, team_id, team_name, wins_close, count_close) %>%
  distinct() %>%
  group_by(season) %>%
  mutate(close_win_pct = wins_close/count_close)

#average win % in home games + variance
hm_mu_hat <- mean(global_win_rate_close_gm_hm$close_win_pct, na.rm = TRUE)
hm_var_hat <- var(global_win_rate_close_gm_hm$close_win_pct, na.rm = TRUE)
#average win % in away games + variance
aw_mu_hat <- mean(global_win_rate_close_gm_aw$close_win_pct, na.rm = TRUE)
aw_var_hat <- var(global_win_rate_close_gm_aw$close_win_pct, na.rm = TRUE)

#alpha, beta for home & away
hm_alpha <- hm_mu_hat * ((hm_mu_hat * (1 - hm_mu_hat) / hm_var_hat) - 1)
hm_beta <- (1 - hm_mu_hat) * ((hm_mu_hat * (1 - hm_mu_hat) / hm_var_hat) - 1)

aw_alpha <- aw_mu_hat * ((aw_mu_hat * (1 - aw_mu_hat) / aw_var_hat) - 1)
aw_beta <- (1 - aw_mu_hat) * ((aw_mu_hat * (1 - aw_mu_hat) / aw_var_hat) - 1)

#Calculate adjusted win rate

temp <- temp %>%
  rowwise() %>%
  mutate(adj_bayes_win_rate_close = ifelse(
    team_home_away == "home", 
      (wins_close + hm_alpha) / (count_close + hm_alpha + hm_beta), #home
      (wins_close + aw_alpha) / (count_close + aw_alpha + aw_beta) #away
  )) 


#Calculating % of games that are close & blowouts
temp <- temp %>%
  mutate(close_pct = count_close / games,
         blowout_pct = count_blowout / games)


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
  #clean up for teams who didn't play in conference tournament
  mutate(conf1 = if_else(is.na(conf1), 
                  "Did Not Play in Conference Tournament",
                  conf1)) %>%
  #concatenate to conference performance
  mutate(
    conf_perf = if_else(
        conf1 == "Did Not Play in Conference Tournament",
        conf1, #leave as is, no W/L
        paste(conf1, if_else(team_winner, "W", "L"), sep = " - ")
    )
  ) %>%
  select(season, team_id, conf_perf) 


#adding this variable to dataframe
temp <- temp %>%
  left_join(last_conf_game, by = c('team_id' = 'team_id', 'season' = 'season'))


##### Players who Avg 15+ Minutes (aka "starters")

#filter player data to desired timeframe and stats
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


#Logic to determine mpg for starters. I choose 15 because the median is 8 players for 15, but avg is less so I would prefer to include more than less
# MPG thresholds you want to evaluate
# thresholds <- c(10, 12, 15, 18, 20, 25)
# 
# median_players_by_threshold <- map_df(thresholds, function(t) {
#   
#   players_filtered %>%
#     group_by(team_id, athlete_id) %>%
#     filter(any(mpg >= t)) %>%
#     distinct(team_id, athlete_id) %>%
#     group_by(team_id) %>%
#     summarise(n_players = n(), .groups = "drop") %>%
#     
#     # get median across all teams
#     summarise(
#       mpg_threshold = t,
#       median_players = median(n_players),
#       avg_players = mean(n_players)
#     )
# })
# 
# median_players_by_threshold

#want players who have at least 15 minutes per game on the season @ home or away
players_filtered <- players_filtered %>%
  group_by(athlete_id) %>%
  #the "any" argument counts both entries for someone who had 17 mpg at home and 12 mpg at away. 
  filter(any(mpg >= 15)) %>%
  ungroup()

#want players who have played at least 3 Home, 3 Away games & at least 45 total min this season
players_filtered <- players_filtered %>%
  group_by(athlete_id) %>%
  filter(all(games_played >= 3) & all(minutes_ >= 45)) %>% 
  ungroup()

#determine the cutoff for # of players attempting a 3 & exploring three point clip rate
hist(players_filtered$three_point_clip)
quantile(players_filtered$three_point_clip, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)

hist(players_filtered$three_point_att) #right skew
quantile(players_filtered$three_point_att, c(0.1, 0.25, 0.5, 0.75, 0.9))

#this says that for 90% of players to shoot 1.5 threes per away game, they would need to shoot 14 in total
quantile(players_filtered[players_filtered$team_home_away == "away",]$games*1.5, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))
#this says that for 90% of players to shoot 1.5 threes per home game, they would need to shoot 12 in total
quantile(players_filtered[players_filtered$team_home_away == "home",]$games*1.5, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))

#add validated attempts and three point clips
#i use a formula so 90% of players have at least X three point attempts per [home or away] game]
#the dist is just across home and away, not by team
players_filtered <- players_filtered %>%
  mutate(valid_att = ifelse(team_home_away == "away" & three_point_att >= floor(quantile(players_filtered[players_filtered$team_home_away == "away",]$games*1.5, 0.1)), 
                            1, 
                            ifelse(team_home_away == "home" & three_point_att >= floor(quantile(players_filtered[players_filtered$team_home_away == "home",]$games*1.5, 0.1)), 1, 0)),
         
         valid_clip = ifelse(three_point_clip >= 0.35 & valid_att == 1, 1, 0))

#aggregate to team level. want shooters who shot 35% & count # of shooters who attempted at least a valiD # OF 3'S
players_stats <- players_filtered %>%
  group_by(season, team_id, team_home_away) %>%
  summarise(starters = n(),
            three_point_shooters = sum(valid_att, na.rm = TRUE),
            threes35 = sum(valid_clip, na.rm = TRUE)) %>%
  mutate(three_pt_shooters_at_35pct_pct = threes35 / three_point_shooters,
         starters_shooting_threes_pct = three_point_shooters / starters)

#data check - confirms no mismatch b/w home & away starters by team
# players_stats %>%
# group_by(team_id) %>%
#   summarise(
#       unique_starters = n_distinct(starters),
#       .groups = "drop"
#     ) %>%
# filter(unique_starters > 1)

#joining player stats to main df
temp <- temp %>%
  left_join(players_stats, by = c('team_id' = 'team_id', 'season' = 'season', 'team_home_away' = 'team_home_away')) %>%
  filter(!is.na(starters)) #removes some edge cases of D3 schools


#### Adding a stat: count of players who fouled out
team_fouls <- players %>%
  inner_join(date_ranges, by = c('season' = 'year')) %>%
  left_join(neutral_site_flag, by = c('game_id' = 'id', 'season' = 'season')) %>%
  #2nd half of season
  filter(game_date.x >= start_date & game_date.x <= end_date) %>%
  #setting all neutral site games to away for both teams
  mutate(team_home_away = ifelse(neutral_site == TRUE, "away", home_away)) %>%
  group_by(season, game_id, team_id, athlete_id, team_home_away) %>%
  #number of games fouled out
  summarise(fouled_out = fouls >= 5,
            .groups = "drop") %>%
  group_by(season, team_id, team_home_away) %>%
  summarise(foul_out_total = sum(fouled_out, na.rm = TRUE))


temp <- temp %>%
  left_join(team_fouls, by = c('team_id' = 'team_id', 'season' = 'season', 'team_home_away' = 'team_home_away'))




#### Adding a stat: starters % who are guards

positions <- players %>%
    #distinct(athlete_id, athlete_position_name) %>%
    group_by(athlete_id) %>%
    summarise(
      athlete_position_name = paste(unique(athlete_position_name), collapse = "/"),
      .groups = "drop"
    ) %>%
    inner_join(players_filtered, by = 'athlete_id') %>%
    select(season, team_id, athlete_id, athlete_position_name) %>%
    distinct(season, team_id, athlete_id, athlete_position_name)

guard_pct <- positions %>%
  # Flag if this athlete is a guard
  mutate(is_guard = str_detect(str_to_lower(athlete_position_name), "guard")) %>%
  group_by(season, team_id) %>%
  summarise(
    total_athletes = n(),
    guard_athletes = sum(is_guard),
    pct_guards = guard_athletes / total_athletes,
    .groups = "drop"
  ) %>%
  select(season, team_id, pct_guards)


temp <- temp %>%
  left_join(guard_pct, by = c('team_id' = 'team_id', 'season' = 'season'))


#### Adding a stat: injuries -->Of the starters, how many missed games? 
# Use a ratio of sum(games played/games)*minutes/sum(minutes)
# What fraction of this team’s played minutes came from players who were actually playing
# so higher % = more healthy?, lower % = less healthy?
# This is my way to get around the significant lack of injury data present online


healthy_rate <- players_filtered %>%
  group_by(season, team_id, athlete_id) %>%
  summarise(total_games = sum(games),
            total_played_games = sum(games_played),
            total_minutes = sum(minutes_),
            total_weighted_minutes = sum((total_played_games/total_games)*minutes_))%>%
  group_by(season, team_id) %>%
  summarise(healthy_rate = sum(total_weighted_minutes)/sum(total_minutes))


temp <- temp %>%
  left_join(healthy_rate, by = c('team_id' = 'team_id', 'season' = 'season'))

##### Teams who go on 10+ point scoring runs (aka Kill Shot) in 2nd half

pbp <- load_mbb_pbp(seasons = 2025)

pbp <- pbp %>%
  select(season, game_date, game_id, home_team_id, home_score, away_team_id, away_score, period_number) %>%
  distinct(season, game_date, game_id, home_team_id, home_score, away_team_id, away_score, period_number) %>%
  #only want 2nd half of season
  inner_join(date_ranges, by = c('season' = 'year')) %>%
  left_join(neutral_site_flag, by = c('game_id' = 'id', 'season' = 'season')) %>%
  #2nd half of season
  filter(game_date.x >= start_date & game_date.x <= end_date) %>%
  #setting all neutral site games to away for both teams
  mutate(
    teamA = ifelse(neutral_site == TRUE, "away", "home"),
    teamB = ifelse(neutral_site == TRUE, "away", "away")) %>%
  select(-game_date.y, -neutral_site, -start_date, -end_date) %>%
  rename(game_date = game_date.x)


# Step 1: Compute scoring team per row
pbp <- pbp %>%
  arrange(game_id, period_number, row_number()) %>%
  group_by(game_id) %>%
  mutate(
    lag_home_score = lag(home_score, default = 0),
    lag_away_score = lag(away_score, default = 0),
    a_diff = home_score - lag_home_score,
    b_diff = away_score - lag_away_score,
    scoring_team_id = case_when(
      a_diff > 0 ~ home_team_id,
      b_diff > 0 ~ away_team_id,
      TRUE ~ NA
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(scoring_team_id)) #filters out start of game & halftimes. want 10 points regardless of half 1 or 2

# Step 2: Detect 10–0 runs by team ID
detect_10_0_runs_by_team <- function(play_by_play) {
  unique_teams <- unique(c(play_by_play$home_team_id, play_by_play$away_team_id))
  game_id_val <- unique(play_by_play$game_id)
  
  results <- map_dfr(unique_teams, function(team_id) {
    current_run <- 0 #sets counter at 0
    run_count <- 0
    
    team_status <- 
      if (team_id == play_by_play$home_team_id[1]) {
      play_by_play$teamA[1]  # already "home"
    } else if (team_id == play_by_play$away_team_id[1]) {
      play_by_play$teamB[1]  # already "away"
    } else {
      NA_character_
    }
    
    for (i in seq_len(nrow(play_by_play))) { #for each row of the pbp data
      row <- play_by_play[i, ]
      
      home_score <- row$home_score #determines who scored & how mnay points
      away_score <- row$away_score #keep summing and if it is >= 10, then assign 1
      lag_home_score <- row$lag_home_score #reset counter and keep going
      lag_away_score <- row$lag_away_score
      
      a_diff <- home_score - lag_home_score
      b_diff <- away_score - lag_away_score
      
      scored <- row$scoring_team_id == team_id
      
      opponent_scored <- !is.na(row$scoring_team_id) && row$scoring_team_id != team_id
      
      # Increase run or reset
      if (scored == TRUE) {
        current_run <- current_run + ifelse(team_id == row$home_team_id, a_diff, b_diff)
        if (current_run >= 10) {
          run_count <- run_count + 1
          current_run <- 0
        }
      } else if (opponent_scored) {
        current_run <- 0
      }
    }
    
    tibble(
      game_id = game_id_val,
      team_id = team_id,
      home_away = team_status,
      ten_zero_runs = run_count
    )
  })
  
  return(results)
}

# Step 3: Apply per game
run_results <- pbp %>%
  group_by(game_id) %>%
  group_split() %>%
  map_dfr(detect_10_0_runs_by_team)

#summarize season by team
run_results <- run_results %>%
  group_by(team_id, home_away) %>%
  summarise(kill_shot_count = sum(ten_zero_runs, na.rm = TRUE))


#joining kill shot count to main df
temp <- temp %>%
  left_join(run_results, by = c('team_id' = 'team_id', 'team_home_away'='home_away'))



#Finalize the temp dataset by reducing down to season summary
final_df <- temp %>%
  select(season, team_id, team_name, team_home_away, tourney_team_flag, conf_perf, 
         games, fgpct, threeptpct, ftpct, treb, oreb, dreb, ast, stl, blk, to, pers_fouls, 
         ppg, opp_ppg, opp_fgpct, opp_threeptpct, wlpct, efgpct, mov, pace, unadj_off_eff, unadj_def_eff,
         extraScoreChances, pyth_exp, luck, oppTO_teamTO_ratio, ast_TO_ratio, foul_out_total, wins_close, count_close, adj_bayes_win_rate_close, close_pct, blowout_pct,
         starters, three_point_shooters, threes35, three_pt_shooters_at_35pct_pct, starters_shooting_threes_pct, kill_shot_count, pct_guards, healthy_rate) %>%
    distinct()


final_df_home <- final_df %>%
  filter(team_home_away == "home")

final_df_away <- final_df %>%
  filter(team_home_away == "away")

































################################################################################
################################################################################
### Splitting data by 1st Half stats (not home/away though)
################################################################################
################################################################################

rm(global_win_rate_close_gm_aw)
rm(global_win_rate_close_gm_hm)
rm(guard_pct)
rm(healthy_rate)
rm(last_conf_game)
rm(pbp)
rm(players)
rm(players_filtered)
rm(players_stats)
rm(positions)
rm(run_results)
rm(stats_2ndhalf)
rm(szn_stats)
rm(team_game_info)
rm(temp)
rm(aw_alpha)
rm(aw_beta)
rm(aw_mu_hat)
rm(aw_var_hat)
rm(hm_alpha)
rm(hm_beta)
rm(hm_mu_hat)
rm(hm_var_hat)
rm(detect_10_0_runs_by_team)

#Need to confirm when the season typically starts - usually first week of Nov
# for(i in 2002:2026) {
#   print(i)
#   print(min(load_mbb_schedule(i)$date))
# }


# create a lookup table with start and end dates per year
#want start of season (first week of Nov).. did Oct 1 to be safe to 12/31
#This is the 1st half of the season
date_ranges_2 <- tibble(
  year = year(tourney_dates),
  start_date = as.Date(paste0(year(tourney_dates)-1, "-10-01")),
  end_date = as.Date(paste0(year(tourney_dates)-1, "-12-31"))
)


szn_stats <- load_mbb_team_box(seasons = 2025)
team_game_info <- load_mbb_schedule(seasons = 2025)


# filter the dataframe using a join and range check so I get 2nd half of season stats
#2 rows for each game (1 for each team)
#gets season stats, filters to 2nd half of season, inner join opponent stats, and left join home/away team stats
stats_1sthalf <- szn_stats %>%
  inner_join(date_ranges_2, by = c('season' = 'year')) %>%
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
    turnovers.x,
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
         turnovers = turnovers.x,
         opp_team_name = opponent_team_short_display_name.x,
         opp_team_score = opponent_team_score.x,
         opp_field_goals_made = field_goals_made.y,
         opp_field_goals_attempted = field_goals_attempted.y,
         opp_three_point_field_goals_made = three_point_field_goals_made.y,
         opp_three_point_field_goals_attempted = three_point_field_goals_attempted.y,
         opp_turnovers = turnovers.y,
         opp_offensive_rebounds = offensive_rebounds.y
  ) 



##################################################################################
### Wrangle data to get 1st half stats. Do *NOT* want to group by home/away

#temp is showing every game of the season for each team. 
#I filter to only the tournament teams here too. Not interested in the rest of the league
###################################################################################


temp <- stats_1sthalf %>%
  #average up stats group by home and away
  group_by(season, team_id) %>%
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
         pers_fouls = mean(fouls),
         ppg = mean(team_score),
         opp_ppg = mean(opp_team_score),
         opp_fgpct = sum(opp_field_goals_made)/sum(opp_field_goals_attempted),
         opp_threeptpct = sum(opp_three_point_field_goals_made)/sum(opp_three_point_field_goals_attempted),
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
         pyth_exp = sum(team_score)^11/(sum(team_score)^11 + sum(opp_team_score)^11),
         luck = mean(pyth_exp - wlpct),
         
         #additional ratios from talk with Russ Spicer
         oppTO_teamTO_ratio = sum(opp_turnovers)/sum(turnovers),
         ast_TO_ratio = sum(assists)/sum(turnovers),
         wins_close = sum(ifelse(abs(team_score- opp_team_score)<=5, team_winner, 0)),
         count_close = sum(ifelse(abs(team_score-opp_team_score)<=5, 1, 0)),
         count_blowout = sum(ifelse(abs(team_score-opp_team_score)>=20, 1, 0))
  ) %>% #only want tourney teams; creates flag to find them
  left_join(tourney_teams, by = c('season' = 'season', 'team_id' = 'team_id')) %>%
  mutate(tourney_team_flag = ifelse(!is.na(team_name.y),1,0)) %>%
  select(-team_name.y) %>%
  rename(team_name = team_name.x)



#Additional Variables:

##### Win Rate in Close Games - Adjusted for Sample Size using Bayesian Adjusted Win Rate (Shrinkage Method)
#Get the global win rate in close games
#select distinct teams to get a global win % across all teams in conf
# i am assuming if they play in 0 cloes games, then they have the ncaa/tourney teams average in winning close games

global_win_rate_close_gm <- temp %>%
  filter(tourney_team_flag==1) %>%
  select(season, team_id, team_name, wins_close, count_close) %>%
  distinct() %>%
  group_by(season) %>%
  mutate(close_win_pct = wins_close/count_close)

#average win % + variance
mu_hat <- mean(global_win_rate_close_gm$close_win_pct, na.rm = TRUE)
var_hat <- var(global_win_rate_close_gm$close_win_pct, na.rm = TRUE)

#alpha, beta for home & away
alpha <- mu_hat * ((mu_hat * (1 - mu_hat) / var_hat) - 1)
beta <- (1 - mu_hat) * ((mu_hat * (1 - mu_hat) / var_hat) - 1)

#Calculate adjusted win rate

temp <- temp %>%
  rowwise() %>%
  mutate(adj_bayes_win_rate_close = (wins_close + alpha) / (count_close + alpha + beta))


#Calculating % of games that are close & blowouts
temp <- temp %>%
  mutate(close_pct = count_close / games,
         blowout_pct = count_blowout / games)


##### Players who Avg 15+ Minutes (aka "starters")

#filter player data to desired timeframe and stats
players_filtered <- players %>%
  inner_join(date_ranges_2, by = c('season' = 'year')) %>%
  #1st half of season
  filter(game_date >= start_date & game_date <= end_date) %>%
  group_by(season, team_id, athlete_id) %>%
  #grabbing season averages for these players. use na.rm = TRUE for players who didn't play
  dplyr::summarise(games = n(),
                   games_played = sum(!is.na(minutes) & minutes >0),
                   minutes_ = sum(minutes, na.rm = TRUE),
                   three_point_made = sum(three_point_field_goals_made, na.rm = TRUE),
                   three_point_att = sum(three_point_field_goals_attempted, na.rm = TRUE),
                   mpg = minutes_/games_played,
                   three_point_clip = three_point_made/three_point_att)


#Logic to determine mpg for starters. I choose 15 because the median is 8 players for 15, but avg is less so I would prefer to include more than less
# MPG thresholds you want to evaluate
# thresholds <- c(10, 12, 15, 18, 20, 25)
# 
# median_players_by_threshold <- map_df(thresholds, function(t) {
#   
#   players_filtered %>%
#     group_by(team_id, athlete_id) %>%
#     filter(any(mpg >= t)) %>%
#     distinct(team_id, athlete_id) %>%
#     group_by(team_id) %>%
#     summarise(n_players = n(), .groups = "drop") %>%
#     
#     # get median across all teams
#     summarise(
#       mpg_threshold = t,
#       median_players = median(n_players),
#       avg_players = mean(n_players)
#     )
# })
# 
# median_players_by_threshold

#want players who have at least 15 minutes per game on the season @ home or away
players_filtered <- players_filtered %>%
  group_by(athlete_id) %>%
  #the "any" argument counts both entries for someone who had 17 mpg at home and 12 mpg at away. 
  filter(any(mpg >= 15)) %>%
  ungroup()

#want players who have played at least 3 Home, 3 Away games & at least 45 total min this season
players_filtered <- players_filtered %>%
  group_by(athlete_id) %>%
  filter(all(games_played >= 3) & all(minutes_ >= 45)) %>% 
  ungroup()

#determine the cutoff for # of players attempting a 3 & exploring three point clip rate
hist(players_filtered$three_point_clip)
quantile(players_filtered$three_point_clip, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)

hist(players_filtered$three_point_att) #right skew
quantile(players_filtered$three_point_att, c(0.1, 0.25, 0.5, 0.75, 0.9))

#this says that for 90% of players to shoot 1.5 threes per game, they would need to shoot 15 in total
quantile(players_filtered$games*1.5, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))


#add validated attempts and three point clips
#i use a formula so 90% of players have at least X three point attempts per [home or away] game]
#the dist is just across home and away, not by team
players_filtered <- players_filtered %>%
  mutate(valid_att = ifelse(three_point_att >= floor(quantile(players_filtered$games*1.5, 0.1)),
                                                     1, 0),
         valid_clip = ifelse(three_point_clip >= 0.35 & valid_att == 1, 1, 0)
  )

#aggregate to team level. want shooters who shot 35% & count # of shooters who attempted at least a valiD # OF 3'S
players_stats <- players_filtered %>%
  group_by(season, team_id) %>%
  summarise(starters = n(),
            three_point_shooters = sum(valid_att, na.rm = TRUE),
            threes35 = sum(valid_clip, na.rm = TRUE)) %>%
  mutate(three_pt_shooters_at_35pct_pct = threes35 / three_point_shooters,
         starters_shooting_threes_pct = three_point_shooters / starters)

#data check - confirms no mismatch b/w home & away starters by team
# players_stats %>%
# group_by(team_id) %>%
#   summarise(
#       unique_starters = n_distinct(starters),
#       .groups = "drop"
#     ) %>%
# filter(unique_starters > 1)

#joining player stats to main df
temp <- temp %>%
  left_join(players_stats, by = c('team_id' = 'team_id', 'season' = 'season')) %>%
  filter(!is.na(starters)) #removes some edge cases of D3 schools



#### Adding a stat: count of players who fouled out
team_fouls <- players %>%
  inner_join(date_ranges_2, by = c('season' = 'year')) %>%
  #1st half of season
  filter(game_date >= start_date & game_date <= end_date) %>%
  group_by(season, game_id, team_id, athlete_id) %>%
  #number of games fouled out
  summarise(fouled_out = fouls >= 5,
            .groups = "drop") %>%
  group_by(season, team_id) %>%
  summarise(foul_out_total = sum(fouled_out, na.rm = TRUE))


temp <- temp %>%
  left_join(team_fouls, by = c('team_id' = 'team_id', 'season' = 'season'))




#### Adding a stat: starters % who are guards

positions <- players %>%
  #distinct(athlete_id, athlete_position_name) %>%
  group_by(athlete_id) %>%
  summarise(
    athlete_position_name = paste(unique(athlete_position_name), collapse = "/"),
    .groups = "drop"
  ) %>%
  inner_join(players_filtered, by = 'athlete_id') %>%
  select(season, team_id, athlete_id, athlete_position_name) %>%
  distinct(season, team_id, athlete_id, athlete_position_name)

guard_pct <- positions %>%
  # Flag if this athlete is a guard
  mutate(is_guard = str_detect(str_to_lower(athlete_position_name), "guard")) %>%
  group_by(season, team_id) %>%
  summarise(
    total_athletes = n(),
    guard_athletes = sum(is_guard),
    pct_guards = guard_athletes / total_athletes,
    .groups = "drop"
  ) %>%
  select(season, team_id, pct_guards)


temp <- temp %>%
  left_join(guard_pct, by = c('team_id' = 'team_id', 'season' = 'season'))


#### Adding a stat: injuries --Of the starters, how many missed games? 
# Use a ratio of sum(games played/games)*minutes/sum(minutes)
# What fraction of this team’s played minutes came from players who were actually playing
# so higher % = more healthy?, lower % = less healthy?
# This is my way to get around the significant lack of injury data present online


healthy_rate <- players_filtered %>%
  group_by(season, team_id, athlete_id) %>%
  summarise(total_games = sum(games),
            total_played_games = sum(games_played),
            total_minutes = sum(minutes_),
            total_weighted_minutes = sum((total_played_games/total_games)*minutes_))%>%
  group_by(season, team_id) %>%
  summarise(healthy_rate = sum(total_weighted_minutes)/sum(total_minutes))


temp <- temp %>%
  left_join(healthy_rate, by = c('team_id' = 'team_id', 'season' = 'season'))


##### Teams who go on 10+ point scoring runs (aka Kill Shot) in 2nd half

pbp <- load_mbb_pbp(seasons = 2025)

pbp <- pbp %>%
  select(season, game_date, game_id, home_team_id, home_score, away_team_id, away_score, period_number) %>%
  distinct(season, game_date, game_id, home_team_id, home_score, away_team_id, away_score, period_number) %>%
  #only want 1st half of season
  inner_join(date_ranges_2, by = c('season' = 'year')) %>%
  #1st half of season
  filter(game_date >= start_date & game_date <= end_date) %>%
  select(-start_date, -end_date)


# Step 1: Compute scoring team per row
pbp <- pbp %>%
  arrange(game_id, period_number, row_number()) %>%
  group_by(game_id) %>%
  mutate(
    lag_home_score = lag(home_score, default = 0),
    lag_away_score = lag(away_score, default = 0),
    a_diff = home_score - lag_home_score,
    b_diff = away_score - lag_away_score,
    scoring_team_id = case_when(
      a_diff > 0 ~ home_team_id,
      b_diff > 0 ~ away_team_id,
      TRUE ~ NA
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(scoring_team_id)) #filters out start of game & halftimes. want 10 points regardless of half 1 or 2

# Step 2: Detect 10–0 runs by team ID
detect_10_0_runs_by_team <- function(play_by_play) {
  unique_teams <- unique(c(play_by_play$home_team_id, play_by_play$away_team_id))
  game_id_val <- unique(play_by_play$game_id)
  
  results <- map_dfr(unique_teams, function(team_id) {
    current_run <- 0 #sets counter at 0
    run_count <- 0
    
    for (i in seq_len(nrow(play_by_play))) { #for each row of the pbp data
      row <- play_by_play[i, ]
      
      home_score <- row$home_score #determines who scored & how mnay points
      away_score <- row$away_score #keep summing and if it is >= 10, then assign 1
      lag_home_score <- row$lag_home_score #reset counter and keep going
      lag_away_score <- row$lag_away_score
      
      a_diff <- home_score - lag_home_score
      b_diff <- away_score - lag_away_score
      
      scored <- row$scoring_team_id == team_id
      
      opponent_scored <- !is.na(row$scoring_team_id) && row$scoring_team_id != team_id
      
      # Increase run or reset
      if (scored == TRUE) {
        current_run <- current_run + ifelse(team_id == row$home_team_id, a_diff, b_diff)
        if (current_run >= 10) {
          run_count <- run_count + 1
          current_run <- 0
        }
      } else if (opponent_scored) {
        current_run <- 0
      }
    }
    
    tibble(
      game_id = game_id_val,
      team_id = team_id,
      ten_zero_runs = run_count
    )
  })
  
  return(results)
}

# Step 3: Apply per game
run_results <- pbp %>%
  group_by(game_id) %>%
  group_split() %>%
  map_dfr(detect_10_0_runs_by_team)

#summarize season by team
run_results <- run_results %>%
  group_by(team_id) %>%
  summarise(kill_shot_count = sum(ten_zero_runs, na.rm = TRUE))


#joining kill shot count to main df
temp <- temp %>%
  left_join(run_results, by = c('team_id' = 'team_id'))



#Finalize the temp dataset by reducing down to season summary
final_df_1sthalf <- temp %>%
  select(season, team_id, team_name, tourney_team_flag, 
         games, fgpct, threeptpct, ftpct, treb, oreb, dreb, ast, stl, blk, to, pers_fouls, 
         ppg, opp_ppg, opp_fgpct, opp_threeptpct, wlpct, efgpct, mov, pace, unadj_off_eff, unadj_def_eff,
         extraScoreChances, pyth_exp, luck, oppTO_teamTO_ratio, ast_TO_ratio, foul_out_total, wins_close, count_close, adj_bayes_win_rate_close, close_pct, blowout_pct, 
         starters, three_point_shooters, threes35, three_pt_shooters_at_35pct_pct, starters_shooting_threes_pct, kill_shot_count, pct_guards, healthy_rate) %>%
  distinct()



#########################################################
####### Finalize dataframes
#########################################################


final_df_1sthalf #1st half total stats

final_df_home #2nd half home stats
final_df_away #2nd half away stats

#weights to decide on: 1h/2ha/2hh
#1. [ ]0.1/0.5/0.4
#2. [ ]0.1/0.6/0.3
#3. [ ]0.1/0.7/0.2
#4. [ ]0.1/0.8/0.1
#5. [ ]0.2/0.4/0.4
#6. [ ]0.2/0.5/0.3
#7. [ ]0.2/0.6/0.2
#8. [ ]0.3/0.4/0.3

#Choose weight scheme
weight_1h <- 0.1
weight_2ha <- 0.5
weight_2hh <- 0.4

#weight 2nd half home
aa <- final_df_home %>%
   mutate(
     across(
       where(is.numeric) & !c(tourney_team_flag, games), 
       ~ .x * weight_2hh
     )
   )

#weight 2nd half away
bb <- final_df_away %>%
  mutate(
    across(
      where(is.numeric) & !c(tourney_team_flag, games), 
      ~ .x * weight_2ha
    )
  )
#weight 1st half
cc <- final_df_1sthalf %>%
  mutate(
    across(
      where(is.numeric) & !c(tourney_team_flag, games), 
      ~ .x * weight_1h
    )
  )

#Map conference perf to 1st half DF b/c I did not include it originally, need it to bind rows
conf_perf_map <- bind_rows(final_df_home, final_df_away) %>%
  ungroup() %>% 
  select(season, team_id, conf_perf) %>%
  distinct()

#Joining in the conf performance map
df_first_half_fixed <- cc %>%
  left_join(conf_perf_map, by = c("season", "team_id"))

#Joins 1st half, 2nd half home/away & sums all values; creates weighted dataframe
weighted_df <- bind_rows(aa, bb, df_first_half_fixed) %>%
  group_by(season, team_id, team_name, tourney_team_flag, conf_perf) %>%
  summarise(
    across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), 
    .groups = "drop"
  ) %>%
  filter(tourney_team_flag ==1)


####################################################################################
#add variable: 
- [x] head coach stats
- [x] count of player ID for "seniority"
- [x] SMDI mislabeled_starters = investigate 2024 kentucky.... apparently dillingham & reed sheppard were the best players but they were coming off the bench. a bad decision/"james franklin" favs situation with calipari
- [x] % of players who foul out? if personal_fouls = 5, then they fouled out

coach stats do not consider current W/L of current season

add note to always check NA's in coach table
>> loop through 2008 to 2026 in team_lookup
>> e.g. Hartford is team id 42 but thye don't showup

join in coach lookup & seniority lookup to main df

remove weights from variables, ie starters should be a whole number

write up documentation for variables





compare your model against basic kaggle dataset


have chatgpt review your code, clean it up if necessary, & then loop 2008:2025 to create full df

# for final model, create and drop the following:
#DROP:
- games
- wins_close, count_close, count_blowout
- starters, threes35, three_point_shooters


####Coach MM Performance Logic:
<> <> <> #separate tab
<> <> <> #separate tab
