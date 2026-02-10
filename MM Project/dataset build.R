#Note for code updates each year:
# 1. check coach_lookup table for NA's in team_id col. Do you need to recode team names
# 2. check coaches_final for exclusions to add (ie remove a coach for retiring or scandal)
# 3. check conf_perf for teams that win any round other than Final
# 4. Update years + tourney dates under Configuration & Helper Data

library(hoopR)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(purrr)
library(glue)
library(progress)
library(rvest)
library(httr)
library(httr2)
library(janitor)
library(data.table)

#############################################################################
# --- 1. Configuration & Helper Data ---
#############################################################################

#Pulling all tournament teams since 2008 (EXCLUDE 2020)
years <- setdiff(2008:2025, 2020)
player_exp_years_lookup <- setdiff(2003:2025, 2020)

#all tourney dates the *Monday* before tourney starts
#Data goes back to 2008, but I go back to 2003 for a player experience variable
tourney_dates <- as.Date(c("2003-03-17", "2004-03-15", "2005-03-14","2006-03-13", "2007-03-12", 
                            "2008-03-17", "2009-03-16","2010-03-15", "2011-03-14", 
                           "2012-03-12", "2013-03-18", "2014-03-17","2015-03-16", "2016-03-14",
                           "2017-03-13", "2018-03-12", "2019-03-18","2021-03-15", "2022-03-14", 
                           "2023-03-13", "2024-03-18", "2025-03-17"))

# create a lookup table with start and end dates per year
#want Jan 1 - start of MM tourney. this is the 2nd half of the season
date_ranges <- tibble(
  season = year(tourney_dates),
  start_date = as.Date(paste0(year(tourney_dates)-1, "-10-01")),
  mid_date = as.Date(paste0(year(tourney_dates), "-01-01")),
  end_date = tourney_dates
)


#############################################################################
# # --- 2. Core Dataframes ---
#############################################################################
team_game_info <- map_dfr(years, ~load_mbb_schedule(seasons = .x))
players <- map_dfr(years, ~load_mbb_player_box(seasons = .x))
players_full_experience <- map_dfr(player_exp_years_lookup, ~load_mbb_player_box(seasons = .x))
szn_stats   <- map_dfr(years, ~load_mbb_team_box(seasons = .x))
#have to create the pbp function this way since the table cols changes over the years
pbp <- map(years, function(x) {
  # Load the season
  data <- load_mbb_pbp(seasons = x)
  
  data %>%
    distinct(
      season, game_date, game_id, 
      home_team_id, home_score, 
      away_team_id, away_score, 
      period_number
    )
}) %>%
  list_rbind() # binds the cleaned list into one df


#need a neutral site flag for these games
neutral_site_flag <- team_game_info %>%
  select(id, game_date, season, neutral_site)



#############################################################################
### --- 3. Tourney Teams & All Teams ---
#############################################################################

#tourney teams
tourney_teams <- team_game_info %>%
  filter(tournament_id == 22) %>%
  bind_rows(
    select(., season, team_id = home_id, team_name = home_short_display_name),
    select(., season, team_id = away_id, team_name = away_short_display_name)
  ) %>%
  drop_na(team_id) %>%
  distinct(season, team_id, team_name) 

#all teams
full_team_list_lookup <- team_game_info %>%
  bind_rows(
    select(., team_id = home_id, team_name = home_location, season),
    select(., team_id = away_id, team_name = away_location, season)
  ) %>%
  drop_na(team_id) %>%
  distinct(season, team_id, team_name)


################################################################################
### --- 4. Seniority & SMDI Metrics ---
################################################################################


#  Player Seniority Logic:
#Use 2003 to present for players with experience prior to 2008
#Use row number function to calculate the number of times the athlete id shows up
#ie: twice? = sophomore, four times? = senior
player_exp <- players_full_experience %>%
  inner_join(date_ranges, by = "season") %>%
  filter(game_date <= end_date) %>%
  distinct(athlete_id, season) %>%
  arrange(athlete_id, season) %>%
  group_by(athlete_id) %>%
  mutate(years_exp = row_number()) %>%
  ungroup()


# Team-level Experience (Avg Exp of rotation players)
#Rotation player aka 15 min per game for 10 games
player_exp_lookup <- players %>%
  inner_join(date_ranges, by = "season") %>%
  filter(game_date <= end_date) %>%
  group_by(season, team_id, athlete_id) %>%
  summarise(mpg = mean(minutes, na.rm = TRUE), 
            total_min = sum(minutes, na.rm = TRUE), 
            .groups = "drop") %>%
  filter(total_min >= 150, mpg >= 7.5) %>%
  left_join(player_exp, by = c("season", "athlete_id")) %>%
  group_by(season, team_id) %>%
  summarise(avg_team_exp = mean(years_exp, na.rm = TRUE), 
            .groups = "drop")

rm(players_full_experience)
rm(player_exp_years_lookup)

#  Starter/Minute Discrepancy Index (SMDI)
#Essentially 2024 Kentucky. Dillingham and Sheppard were top freshman scorers, but were never starters
#aka James Franklin

### SMDI score: how many of the 5 official starters also led the team in minutes?
#0 <- Same 5 players start and play the most
#0.4 <- 2 of top minute players come off bench
#1.0 <- Lineup starters ≠ rotation core

team_smdi <- players %>%
  inner_join(date_ranges, by = "season") %>%
  filter(game_date <= end_date) %>%
  group_by(season, team_id, athlete_id) %>%
  summarise(gs = sum(starter, na.rm = TRUE), 
            mpg = mean(minutes, na.rm = TRUE), 
            .groups = "drop") %>%
  group_by(season, team_id) %>%
  mutate(starter_rank = rank(-gs, ties.method = "first"), 
         minutes_rank = rank(-mpg, ties.method = "first")) %>%
  summarise(smdi = 1 - (length(intersect(athlete_id[starter_rank <= 5], 
                                         athlete_id[minutes_rank <= 5])) / 5), 
            .groups = "drop")


################################################################################
### --- 5. Coach Dataframe (scraped from ProSportsReference) ---
################################################################################

#Does the coach help sway the signal for game outcome?
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
raw_coaches <- map_dfr(years, scrape_coaches_season)



# *** Data cleanup to fix tourney teams that had multiple coaches in season
#ie: Indiana HC Kelvin Sampson stepped down midseason for recruiting violations

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

#Here is a check to see if there are more coaches to remove
# coaches_final %>%
#   filter(tourney_perf != "") %>%
#   group_by (year, school) %>%
#   summarise(count = n()) %>%
#   filter(count > 1)


exclusions <- tibble(
  coach = c("Kelvin Sampson", "Ken McDonald", "Bo Ryan", "Will Wade", "Will Wade", "Bill Self", "Chris Beard", "Vernon Hamilton", "Brandon Chambers"),
  school = c("Indiana", "Western Kentucky", "Wisconsin", "LSU", "LSU", "Kansas", "Texas", "McNeese State", "McNeese State"),
  year = c(2008, 2012, 2016, 2019, 2022, 2023, 2023, 2024, 2024)
)


coaches_final <- raw_coaches %>%
  # Filter out the sub-header rows found in the middle of the table
  filter(!str_detect(x, "Coach") & x != "") %>%
  select(
    coach = x, school = x_2, year,
    current_w = season, current_l = season_2, tourney_perf = season_6,
    #win/loss does not consider W/L of current season. it is the coach performance for all seasons prior
    curr_school_career_w = career_at_current_school_2, 
    curr_school_career_l = career_at_current_school_3,
    
    curr_school_career_ncaa_app = career_at_current_school_5,
    curr_school_career_s16_app = career_at_current_school_6,
    curr_school_career_f4_app = career_at_current_school_7,
    curr_school_career_champ = career_at_current_school_8,
    
    #win/loss does not consider W/L of current season. it is the coach performance for all seasons prior
    career_w = career_overall, career_l = career_overall_2,
    
    career_ncaa_app = career_overall_4,
    career_s16_app = career_overall_5,
    career_f4_app = career_overall_6,
    career_champ = career_overall_7,
    
  ) %>%
  #remove * from coach name
  mutate(coach = str_remove(coach, " \\*")) %>%
  # Convert numeric columns back to numbers
  mutate(across(c(year, starts_with("curr"), starts_with("career")), parse_number)) %>%
  filter(tourney_perf != "") %>%
  anti_join(exclusions, by = c("coach", "school", "year"))



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



#Final coach table Lookup
coach_lookup <- coaches_final %>%
  mutate(school = recode(
    school,
    "Albany (NY)" = "Albany",
    "American" = "American University",
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
    "UNC" = "North Carolina",
    .default = school
  )) %>%
  left_join(full_team_list_lookup, by = c('school' = 'team_name', 'year' = 'season'))



################################################################################
### --- 6. Split data in to 2nd Half (Home), 2nd Half (Away) ---
################################################################################

#Splits season stats to 2nd half home/away
#Gets 2 rows for each game (1 for each team)
stats_2ndhalf <- szn_stats %>%
  inner_join(date_ranges, by = 'season') %>%
  #2nd half of season
  filter(game_date >= mid_date & game_date <= end_date) %>%
  inner_join(szn_stats, by = c('game_id', 'opponent_team_id' = 'team_id')) %>%
  left_join(team_game_info, by = c('game_id' = 'id', 'team_id' = 'home_id')) %>%
  left_join(team_game_info, by =c('game_id' = 'id', 'team_id' = 'away_id')) %>%
  #trimming the duplicated columns & setting all neutral site games to away for both teams
  mutate(neutral_site = coalesce(neutral_site.x, neutral_site.y),
         notes_headline = coalesce(notes_headline.x, notes_headline.y),
         team_home_away = ifelse(neutral_site == TRUE, "away", team_home_away.x),
         tournament_id = coalesce(tournament_id.x, tournament_id.y)) %>%
  #selecting desired columns
  select(
    # game / team info
    game_id, season = season.x, game_date = game_date.x,
    team_id,
    team_name = team_short_display_name.x,
    team_home_away,
    neutral_site, notes_headline, tournament_id,
    
    # team stats
    team_winner = team_winner.x,
    team_score  = team_score.x,
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
    
    # opponent stats
    opponent_team_id,
    opp_team_score = opponent_team_score.x,
    opp_field_goals_made = field_goals_made.y,
    opp_field_goals_attempted = field_goals_attempted.y,
    opp_three_point_field_goals_made = three_point_field_goals_made.y,
    opp_three_point_field_goals_attempted = three_point_field_goals_attempted.y,
    opp_turnovers = turnovers.y,
    opp_offensive_rebounds = offensive_rebounds.y
  )


# team stats grouped by season and home/away. only tournament teams
team_season_stats_2H <- stats_2ndhalf %>%
  #average up stats group by home and away
  group_by(season, team_id, team_home_away) %>%
  mutate(games           = n(),
         fgpct           = sum(field_goals_made)/sum(field_goals_attempted),
         threeptpct      = sum(three_point_field_goals_made)/sum(three_point_field_goals_attempted),
         ftpct           = sum(free_throws_made)/sum(free_throws_attempted),
         treb            = mean(total_rebounds),
         oreb            = mean(offensive_rebounds),
         dreb            = mean(defensive_rebounds),
         ast             = mean(assists),
         stl             = mean(steals),
         blk             = mean(blocks),
         to              = mean(turnovers),
         pers_fouls      = mean(fouls),
         ppg             = mean(team_score),
         opp_ppg         = mean(opp_team_score),
         opp_fgpct       = sum(opp_field_goals_made)/sum(opp_field_goals_attempted),
         opp_threeptpct  = sum(opp_three_point_field_goals_made)/sum(opp_three_point_field_goals_attempted),
         wins            = sum(team_winner),
         efgpct          = mean((field_goals_made + (0.5*three_point_field_goals_made))/field_goals_attempted),
         mov             = mean(team_score - opp_team_score),
         
         #formulas found online to estimate pace, efficiency, extra Scoring chances
         pace            = mean(field_goals_attempted - offensive_rebounds + turnovers + (0.455 * free_throws_attempted)),
         unadj_off_eff   = sum(team_score)/sum(field_goals_attempted - offensive_rebounds + turnovers + (0.455 * free_throws_attempted)),
         unadj_def_eff   = sum(opp_team_score)/sum(field_goals_attempted - offensive_rebounds + turnovers + (0.455 * free_throws_attempted)),
         extraScoreChances = mean(offensive_rebounds + opp_turnovers - opp_offensive_rebounds - turnovers),
         
         #use for pythagorean expectation calculation latre on
         total_points     = sum(team_score),
         total_opp_points = sum(opp_team_score),
         
         #additional ratios from talk with Russ Spicer
         oppTO_teamTO_ratio   = sum(opp_turnovers)/sum(turnovers),
         ast_TO_ratio         = sum(assists)/sum(turnovers),
         wins_close           = sum(ifelse(abs(team_score- opp_team_score)<=5, team_winner, 0)),
         count_close          = sum(ifelse(abs(team_score-opp_team_score)<=5, 1, 0)),
         count_blowout        = sum(ifelse(abs(team_score-opp_team_score)>=20, 1, 0)),
         close_pct            = count_close / games,
         blowout_pct          = count_blowout / games
  ) %>% #only want tourney teams; creates flag to find them
  left_join(tourney_teams, by = c('season', 'team_id')) %>%
  mutate(tourney_team_flag = ifelse(!is.na(team_name.y),1,0)) %>%
  select(-team_name.y) %>%
  rename(team_name = team_name.x)


################################################################################
### --- 7. Additional Vars for 2nd Half (Home), 2nd Half (Away) ---
################################################################################


####
# Adjusted Win Rate for Close Games
####

#adjust sample size using Bayesian Adj Win Rate ("shrinkage method")
#basically use the global win rate in close games & if the team has 0 close games, impute the global win rate


# calculate Bayesian Priors (mu + var) for home/away, season
priors <- team_season_stats_2H %>%
  filter(tourney_team_flag == 1) %>%
  distinct(season, team_id, wins_close, count_close) %>%
  group_by(season, team_home_away) %>%
  summarise(
    mu  = sum(wins_close)/ sum(count_close),
    var = var(wins_close / count_close, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  #alpha/beta for weights to mu/var
  mutate(
    alpha = mu * (((mu * (1 - mu)) / var) - 1),
    beta  = (1 - mu) * (((mu * (1 - mu)) / var) - 1)
  )

#calculate adj bayes win rate
team_season_stats_2H <- team_season_stats_2H %>%
  left_join(priors, by = c("season", "team_home_away")) %>%
  mutate(
    adj_bayes_win_rate_close = (wins_close + alpha) / (count_close + alpha + beta)
  ) %>%
  # Clean up temporary prior columns
  select(-mu, -var, -alpha, -beta)




####
# Conference Tournament Performance
####

# Get the last game played in the regular season (use slice max)
last_conf_game <- team_season_stats_2H %>%
  group_by(season, team_id) %>%
  slice_max(order_by = game_date, n=1, with_ties = FALSE) %>%
  #uses stringR notation to clean up conference performance to easy to interpret format
  mutate(cleaned = str_to_title(str_remove(notes_headline, " AT .*")),
         # Remove trailing city/state (e.g., " TULSA OK") after "Finals"
         cleaned = str_remove(cleaned, "(?<=Finals)\\s+.*$"),
         
         # 2. Extract the round info
         clean_conf = str_extract(cleaned, "[^-]+$"),
         clean_conf = str_trim(clean_conf), # Ensure no trailing whitespace
         
         # 3. Standardize "Finals" to "Final"
         conf1 = str_replace_all(clean_conf, "(?i)\\bFinals\\b", "Final"),
         
         # 4. Existing standardization logic
         conf1 = str_replace_all(conf1, "(?i)\\b[1-4](st|nd|rd|th)? Round\\b", "Early Round"),
         conf1 = str_replace_all(conf1, regex("\\bQtrfinals\\b", ignore_case = TRUE), "Quarterfinals"),
         conf1 = str_replace_all(conf1, regex("\\bQuarterfinal\\b", ignore_case = TRUE), "Quarterfinals"),
         conf1 = str_replace_all(conf1, regex("\\bSemis\\b", ignore_case = TRUE), "Semifinals"),
         conf1 = str_replace_all(conf1, regex("\\bSemi-final\\b", ignore_case = TRUE), "Semifinals"),
         conf1 = str_replace_all(conf1, regex("\\bSemifinal\\b", ignore_case = TRUE), "Semifinals")) %>%
  # Clean up for teams who didn't play in conference tournament
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
team_season_stats_2H <- team_season_stats_2H %>%
  left_join(last_conf_game, by = c('team_id', 'season'))


# check to ensure no more exceptions needed
# team_season_stats_2H %>%
#   ungroup() %>%
#   filter(tourney_team_flag ==1) %>%
#   group_by(conf_perf) %>%
#   summarise(count = n()) %>%
#   arrange(desc(count))


#Have to handle exclusions because there are issues with the data
team_season_stats_2H <- team_season_stats_2H %>%
  mutate(conf_perf = case_when(
    # 2013 Exceptions & Typos
    season == 2013 & team_id == 57  ~ "Final - L",        # Florida
    season == 2013 & team_id == 139 ~ "Final - W",        # Saint Louis
    season == 2013 & team_id == 145 ~ "Final - W",        # Ole Miss
    season == 2013 & team_id == 153 ~ "Final - L",        # North Carolina
    season == 2013 & team_id == 194 ~ "Final - W",        # Ohio State
    season == 2013 & team_id == 275 ~ "Final - L",        # Wisconsin
    season == 2013 & team_id == 2390 ~ "Final - W",       # Miami
    season == 2013 & team_id == 2670 ~ "Final - L",       # VCU
    
    # 2012 Exceptions
    season == 2012 & team_id == 52  ~ "Final - W",        # Florida State
    season == 2012 & team_id == 96  ~ "Final - L",        # Kentucky
    season == 2012 & team_id == 127 ~ "Final - W",        # Michigan State
    season == 2012 & team_id == 153 ~ "Final - L",        # North Carolina
    season == 2012 & team_id == 194 ~ "Final - L",        # Ohio State
    season == 2012 & team_id == 179 ~ "Final - W",        # St. Bonaventure
    season == 2012 & team_id == 238 ~ "Final - W",        # Vanderbilt
    season == 2012 & team_id == 2752 ~ "Final - L",       # Xavier
    
    # 2008, 2010, 2011/2015, 2021 Exceptions
    season == 2021 & team_id == 2305 ~ "Quarterfinals - L", # Kansas
    season == 2021 & team_id == 258 ~ "Quarterfinals - L", # Virginia
    season == 2015 & team_id == 108 ~ "Final - W", # Harvard
    season == 2011 & team_id == 163 ~ "Final - W", # Princeton
    season == 2010 & team_id == 96  ~ "Final - W",        # Kentucky
    season == 2010 & team_id == 135 ~ "Final - L",        # Minnesota
    season == 2010 & team_id == 194 ~ "Final - W",        # Ohio State
    season == 2010 & team_id == 150 ~ "Final - W",        # Duke
    season == 2010 & team_id == 59  ~ "Final - L",        # Georgia Tech
    season == 2010 & team_id == 218 ~ "Final - W",        # Temple
    season == 2010 & team_id == 257 ~ "Final - L",        # Richmond
    season == 2008 & team_id == 252 ~ "Final - L",        # BYU
    season == 2008 & team_id == 2439 ~ "Final - W",        # UNLV
    
    # Keep everything else the same
    TRUE ~ conf_perf
  ))



####
# Player Three Point Ability, Foul Outs, Injury Rate, & Guard % of Starters
####

#For injuries: took % of games played times total minutes to get a representation of minutes lost due to missed time
#Higher % = played more games, Lower % = less healthy
# This is my way to get around the significant lack of injury data present online


#get player stats and 2nd half
players_filtered <- players %>%
  inner_join(date_ranges, by = 'season') %>%
  left_join(neutral_site_flag, by = c('game_id' = 'id', 'season')) %>%
  #2nd half of season
  filter(game_date.x >= mid_date & game_date.x <= end_date) %>%
  #setting all neutral site games to away for both teams
  mutate(team_home_away = ifelse(neutral_site == TRUE, "away", home_away)) %>%
  group_by(season, team_id, athlete_id, team_home_away) %>%
  #grabbing season averages for these players. use na.rm = TRUE for players who didn't play
  dplyr::summarise(
            athlete_position_name = paste(unique(athlete_position_name), collapse = "/"),
            games = n(),
            games_played = sum(minutes >0, na.rm = TRUE),
            minutes_ = sum(minutes, na.rm = TRUE),
            three_point_made = sum(three_point_field_goals_made, na.rm = TRUE),
            three_point_att = sum(three_point_field_goals_attempted, na.rm = TRUE),
            
            weighted_minutes = (games_played/games)*minutes_,
            mpg = minutes_/games_played,
            three_point_clip = three_point_made/three_point_att,
            three_pt_att_pg = three_point_att / games,
            three_pt_att_pgp = three_point_att / games_played,
            fouled_out_count = sum(fouls >= 5, na.rm = TRUE),
            .groups = 'drop')


#Logic to determine mpg for starters. I choose 14 because the median is 8 players, feels like a good number
# MPG thresholds you want to evaluate
# thresholds <- c(5, 8, 10, 12, 14,  15, 18)
# 
# median_players_by_threshold <- map_df(thresholds, function(t) {
# 
#   players_filtered %>%
#     group_by(season, team_id, athlete_id) %>%
#     filter(any(mpg >= t)) %>%
#     distinct(season, team_id, athlete_id) %>%
#     group_by(season, team_id) %>%
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



#filter for starters: want 14 mpg in home or away, & play >= 3 games & 45 min home AND away
starters <- players_filtered %>%
  group_by(season, team_id, athlete_id) %>%
  filter(any(mpg >= 14), all(games_played >=3), all(minutes_ >=45)) %>%
  ungroup() 


#roughly 2 3pa per game is the median
quantile(starters[starters$team_home_away=="home",]$three_pt_att_pgp, c(0, 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9))
quantile(starters[starters$team_home_away=="away",]$three_pt_att_pgp, c(0, 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9))
hist(starters$three_pt_att_pgp)

#add a variable for valid 3pt shooters. are they attempting ~2 3pa per game
starters <- starters %>%
  mutate(valid_three_point_shooter = ifelse(team_home_away == "away" & three_pt_att_pgp >= quantile(starters[starters$team_home_away=="away",]$three_pt_att_pgp, 0.5),
                                            1, 
                                            ifelse(team_home_away == "home" & three_pt_att_pgp >= quantile(starters[starters$team_home_away=="home",]$three_pt_att_pgp, 0.5),
                                                   1, 0)),
         effective_three_point_shooter = ifelse(three_point_clip >= 0.35 & valid_three_point_shooter == 1, 1, 0)
         )

#aggregate to team level. want shooters who shot 35% & count # of shooters who attempted at least a valiD # OF 3'S
#also include foul outs count
#also include % of total minutes played by starters were lost due to injury
starters_agg <- starters %>%
  group_by(season, team_id, team_home_away) %>%
  summarise(starters = n(),
            three_point_shooters = sum(valid_three_point_shooter, na.rm = TRUE),
            threes35 = sum(effective_three_point_shooter, na.rm = TRUE),
            foul_outs = sum(fouled_out_count, na.rm = TRUE),
            healthy_rate = sum(weighted_minutes, na.rm = TRUE)/sum(minutes_, na.rm = TRUE)) %>%
  mutate(three_pt_shooters_at_35pct_pct = threes35 / three_point_shooters,
         starters_shooting_threes_pct = three_point_shooters / starters) %>%
  select(-three_point_shooters, -threes35)


#data check - confirms no mismatch b/w home & away for starters by team
# what %>%
# group_by(team_id,season) %>%
#   summarise(
#       unique_starters = n_distinct(starters),
#       .groups = "drop"
#     ) %>%
# filter(unique_starters > 1)

#joining player 3p + foul stats to main df
team_season_stats_2H <- team_season_stats_2H %>%
  left_join(starters_agg, by = c('team_id', 'season', 'team_home_away'))


guard_pct <- starters %>%
  mutate(is_guard = str_detect(str_to_lower(athlete_position_name), "guard")) %>%
  group_by(season, team_id, team_home_away) %>%
  summarise(
    starters = n(),
    guards = sum(is_guard),
    pct_guards = guards / starters,
    .groups = "drop"
  ) %>%
  select(season, team_id, team_home_away, pct_guards)

#joining player position (% guards) to main df
team_season_stats_2H <- team_season_stats_2H %>%
  left_join(guard_pct, by = c('team_id', 'season', 'team_home_away'))





####
# Teams who go on 10+ point scoring runs (aka Kill Shot)
####

pbp_clean <- pbp %>%
  inner_join(date_ranges, by = "season") %>%
  left_join(neutral_site_flag, by = c("game_id" = "id", "season")) %>%
  filter(game_date.x >= mid_date, game_date.x <= end_date) %>%
  arrange(season, game_id, period_number, row_number()) %>%
  group_by(game_id) %>%
  # Vectorized calculation of points per play
  mutate(
    #points scored on play
    pts = pmax(home_score - lag(home_score, default = 0), 
               away_score - lag(away_score, default = 0)),
    #who scored
    scoring_team_id = case_when(
      home_score > lag(home_score, default = 0) ~ home_team_id,
      away_score > lag(away_score, default = 0) ~ away_team_id,
      TRUE ~ NA_real_
    ),
    # Pre-determine home/away status based on neutral site flag
    team_status = case_when(
      neutral_site ~ "away",
      scoring_team_id == home_team_id ~ "home",
      TRUE ~ "away"
    )
  ) %>%
  filter(!is.na(scoring_team_id)) %>%
  # logic for 10--0 run
  # rleid creates a unique ID every time the scoring team changes
  mutate(run_id = data.table::rleid(scoring_team_id)) %>%
  group_by(season, game_id, run_id, scoring_team_id, team_status) %>%
  summarise(run_pts = sum(pts), 
            .groups = "drop") %>%
  # Using floor division (%/%) handles cases like a 22-0 run (counts as 2 kill shots)
  # fixes glitch of some teams have 600 10-0 runs in one game
  mutate(ten_zero_runs = run_pts %/% 10,
         ten_zero_runs = ifelse(ten_zero_runs >2, 1, ten_zero_runs)) %>%
  filter(ten_zero_runs > 0)

# 4. Final Season Summarization
run_results <- pbp_clean %>%
  group_by(season, team_id = scoring_team_id, team_home_away = team_status) %>%
  summarise(kill_shot_count = sum(ten_zero_runs), 
            .groups = "drop")


team_season_stats_2H <- team_season_stats_2H %>%
  left_join(run_results, by = c('season', 'team_id', 'team_home_away')) %>%
  mutate(kill_shot_count = coalesce(kill_shot_count, 0))


#Final DF for 2nd half of season stats
final_df_2H <- team_season_stats_2H %>%
  select(season, team_id, team_name, team_home_away, tourney_team_flag, 
         conf_perf, 
         games, wins,  total_points, total_opp_points,
         fgpct, threeptpct, ftpct, treb, oreb, dreb, ast, stl, blk, to, pers_fouls, 
         ppg, opp_ppg, opp_fgpct, opp_threeptpct, efgpct, mov, pace, unadj_off_eff, unadj_def_eff,
         starters_shooting_threes_pct, three_pt_shooters_at_35pct_pct,
         extraScoreChances, oppTO_teamTO_ratio, ast_TO_ratio, foul_outs, 
         adj_bayes_win_rate_close, close_pct, blowout_pct,
         healthy_rate, pct_guards, kill_shot_count) %>%
  filter(tourney_team_flag == 1) %>%
    distinct()


final_df_2H_home <- final_df_2H %>%
  filter(team_home_away == "home")

final_df_2H_away <- final_df_2H %>%
  filter(team_home_away == "away")
































################################################################################
### --- 6. Split data in to 1st half only ---
################################################################################


#Need to confirm when the season typically starts - usually first week of Nov
# for(i in 2002:2026) {
#   print(i)
#   print(min(load_mbb_schedule(i)$date))
# }


#Splits season stats to 1st half. Do *NOT* want to group by home/away
#Gets 2 rows for each game (1 for each team)
stats_1sthalf <- szn_stats %>%
  inner_join(date_ranges, by = 'season') %>%
  #1st half of season
  filter(game_date >= start_date & game_date < mid_date) %>%
  inner_join(szn_stats, by = c('game_id', 'opponent_team_id' = 'team_id')) %>%
  left_join(team_game_info, by = c('game_id' = 'id', 'team_id' = 'home_id')) %>%
  left_join(team_game_info, by =c('game_id' = 'id', 'team_id' = 'away_id')) %>%
  #trimming the duplicated columns & setting all neutral site games to away for both teams
  mutate(neutral_site = coalesce(neutral_site.x, neutral_site.y),
         notes_headline = coalesce(notes_headline.x, notes_headline.y),
         team_home_away = ifelse(neutral_site == TRUE, "away", team_home_away.x),
         tournament_id = coalesce(tournament_id.x, tournament_id.y)) %>%
  #selecting desired columns
  select(
    # game / team info
    game_id, season = season.x, game_date = game_date.x,
    team_id,
    team_name = team_short_display_name.x,
    team_home_away,
    neutral_site, notes_headline, tournament_id,
    
    # team stats
    team_winner = team_winner.x,
    team_score  = team_score.x,
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
    
    # opponent stats
    opponent_team_id,
    opp_team_score = opponent_team_score.x,
    opp_field_goals_made = field_goals_made.y,
    opp_field_goals_attempted = field_goals_attempted.y,
    opp_three_point_field_goals_made = three_point_field_goals_made.y,
    opp_three_point_field_goals_attempted = three_point_field_goals_attempted.y,
    opp_turnovers = turnovers.y,
    opp_offensive_rebounds = offensive_rebounds.y
  )


# team stats grouped by season and home/away. only tournament teams
team_season_stats_1H <- stats_1sthalf %>%
  #average up stats group by home and away
  group_by(season, team_id) %>%
  mutate(games           = n(),
         fgpct           = sum(field_goals_made)/sum(field_goals_attempted),
         threeptpct      = sum(three_point_field_goals_made)/sum(three_point_field_goals_attempted),
         ftpct           = sum(free_throws_made)/sum(free_throws_attempted),
         treb            = mean(total_rebounds),
         oreb            = mean(offensive_rebounds),
         dreb            = mean(defensive_rebounds),
         ast             = mean(assists),
         stl             = mean(steals),
         blk             = mean(blocks),
         to              = mean(turnovers),
         pers_fouls      = mean(fouls),
         ppg             = mean(team_score),
         opp_ppg         = mean(opp_team_score),
         opp_fgpct       = sum(opp_field_goals_made)/sum(opp_field_goals_attempted),
         opp_threeptpct  = sum(opp_three_point_field_goals_made)/sum(opp_three_point_field_goals_attempted),
         wins            = sum(team_winner),
         efgpct          = mean((field_goals_made + (0.5*three_point_field_goals_made))/field_goals_attempted),
         mov             = mean(team_score - opp_team_score),
         
         #formulas found online to estimate pace, efficiency, extra Scoring chances
         pace            = mean(field_goals_attempted - offensive_rebounds + turnovers + (0.455 * free_throws_attempted)),
         unadj_off_eff   = sum(team_score)/sum(field_goals_attempted - offensive_rebounds + turnovers + (0.455 * free_throws_attempted)),
         unadj_def_eff   = sum(opp_team_score)/sum(field_goals_attempted - offensive_rebounds + turnovers + (0.455 * free_throws_attempted)),
         extraScoreChances = mean(offensive_rebounds + opp_turnovers - opp_offensive_rebounds - turnovers),
         
         #use for pythagorean expectation calculation latre on
         total_points     = sum(team_score),
         total_opp_points = sum(opp_team_score),
         
         #additional ratios from talk with Russ Spicer
         oppTO_teamTO_ratio   = sum(opp_turnovers)/sum(turnovers),
         ast_TO_ratio         = sum(assists)/sum(turnovers),
         wins_close           = sum(ifelse(abs(team_score- opp_team_score)<=5, team_winner, 0)),
         count_close          = sum(ifelse(abs(team_score-opp_team_score)<=5, 1, 0)),
         count_blowout        = sum(ifelse(abs(team_score-opp_team_score)>=20, 1, 0)),
         close_pct            = count_close / games,
         blowout_pct          = count_blowout / games
  ) %>% #only want tourney teams; creates flag to find them
  left_join(tourney_teams, by = c('season', 'team_id')) %>%
  mutate(tourney_team_flag = ifelse(!is.na(team_name.y),1,0)) %>%
  select(-team_name.y) %>%
  rename(team_name = team_name.x)


################################################################################
### --- 7. Additional Vars for 1st Half ---
################################################################################



####
# Adjusted Win Rate for Close Games
####

#adjust sample size using Bayesian Adj Win Rate ("shrinkage method")
#basically use the global win rate in close games & if the team has 0 close games, impute the global win rate

rm(priors)

# calculate Bayesian Priors (mu + var) for home/away, season
priors_1H <- team_season_stats_1H %>%
  filter(tourney_team_flag == 1) %>%
  distinct(season, team_id, wins_close, count_close) %>%
  group_by(season) %>%
  summarise(
    mu  = sum(wins_close)/ sum(count_close),
    var = var(wins_close / count_close, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  #alpha/beta for weights to mu/var
  mutate(
    alpha = mu * (((mu * (1 - mu)) / var) - 1),
    beta  = (1 - mu) * (((mu * (1 - mu)) / var) - 1)
  )

#calculate adj bayes win rate
team_season_stats_1H <- team_season_stats_1H %>%
  left_join(priors_1H, by = "season") %>%
  mutate(
    adj_bayes_win_rate_close = (wins_close + alpha) / (count_close + alpha + beta)
  ) %>%
  # Clean up temporary prior columns
  select(-mu, -var, -alpha, -beta)



####
# Player Three Point Ability, Foul Outs, Injury Rate, & Guard % of Starters
####

#For injuries: took % of games played times total minutes to get a representation of minutes lost due to missed time
#Higher % = played more games, Lower % = less healthy
# This is my way to get around the significant lack of injury data present online

rm(players_filtered)

#get player stats and 1st half
players_filtered_1H <- players %>%
  inner_join(date_ranges, by = 'season') %>%
  left_join(neutral_site_flag, by = c('game_id' = 'id', 'season')) %>%
  #2nd half of season
  filter(game_date.x >= start_date & game_date.x < mid_date) %>%
  #setting all neutral site games to away for both teams
  mutate(team_home_away = ifelse(neutral_site == TRUE, "away", home_away)) %>%
  group_by(season, team_id, athlete_id) %>%
  #grabbing season averages for these players. use na.rm = TRUE for players who didn't play
  dplyr::summarise(
    athlete_position_name = paste(unique(athlete_position_name), collapse = "/"),
    games = n(),
    games_played = sum(minutes >0, na.rm = TRUE),
    minutes_ = sum(minutes, na.rm = TRUE),
    three_point_made = sum(three_point_field_goals_made, na.rm = TRUE),
    three_point_att = sum(three_point_field_goals_attempted, na.rm = TRUE),
    
    weighted_minutes = (games_played/games)*minutes_,
    mpg = minutes_/games_played,
    three_point_clip = three_point_made/three_point_att,
    three_pt_att_pg = three_point_att / games,
    three_pt_att_pgp = three_point_att / games_played,
    fouled_out_count = sum(fouls >= 5, na.rm = TRUE),
    .groups = 'drop')


#Logic to determine mpg for starters. I choose 14 because the median is 8 players, feels like a good number
# MPG thresholds you want to evaluate
# thresholds <- c(5, 8, 10, 12, 14,  15, 18)
# 
# median_players_by_threshold <- map_df(thresholds, function(t) {
# 
#   players_filtered_1H %>%
#     group_by(season, team_id, athlete_id) %>%
#     filter(any(mpg >= t)) %>%
#     distinct(season, team_id, athlete_id) %>%
#     group_by(season, team_id) %>%
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

rm(starters)

#filter for starters: want 12 mpg, play in at least 4 games, & 60 min in total
starters_1H <- players_filtered_1H %>%
  group_by(season, team_id, athlete_id) %>%
  filter(mpg>=12, games_played >=4, minutes_ >= 60) %>%
  ungroup() 


#roughly 2 3pa per game is the median
quantile(starters_1H$three_pt_att_pgp, c(0, 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9))
hist(starters_1H$three_pt_att_pgp)

#add a variable for valid 3pt shooters. are they attempting ~2 3pa per game
starters_1H <- starters_1H %>%
  mutate(valid_three_point_shooter = ifelse(three_pt_att_pgp >= quantile(starters_1H$three_pt_att_pgp, 0.5) ,1, 0), 
         effective_three_point_shooter = ifelse(three_point_clip >= 0.35 & valid_three_point_shooter == 1, 1, 0)
  )


rm(starters_agg)
#aggregate to team level. want shooters who shot 35% & count # of shooters who attempted at least a valid # OF 3'S
#also include foul outs count
#also include % of total minutes played by starters were lost due to injury
starters_agg_1H <- starters_1H %>%
  group_by(season, team_id) %>%
  summarise(starters = n(),
            three_point_shooters = sum(valid_three_point_shooter, na.rm = TRUE),
            threes35 = sum(effective_three_point_shooter, na.rm = TRUE),
            foul_outs = sum(fouled_out_count, na.rm = TRUE),
            healthy_rate = sum(weighted_minutes, na.rm = TRUE)/sum(minutes_, na.rm = TRUE)) %>%
  mutate(three_pt_shooters_at_35pct_pct = threes35 / three_point_shooters,
         starters_shooting_threes_pct = three_point_shooters / starters) %>%
  select(-three_point_shooters, -threes35)


#data check - confirms no mismatch b/w home & away for starters by team
# starters_agg_1H %>%
# group_by(team_id,season) %>%
#   summarise(
#       unique_starters = n_distinct(starters),
#       .groups = "drop"
#     ) %>%
# filter(unique_starters > 1)

#joining player 3p + foul stats to main df
team_season_stats_1H <- team_season_stats_1H %>%
  left_join(starters_agg_1H, by = c('team_id', 'season'))


guard_pct <- starters_1H %>%
  mutate(is_guard = str_detect(str_to_lower(athlete_position_name), "guard")) %>%
  group_by(season, team_id) %>%
  summarise(
    starters = n(),
    guards = sum(is_guard),
    pct_guards = guards / starters,
    .groups = "drop"
  ) %>%
  select(season, team_id, pct_guards)

#joining player position (% guards) to main df
team_season_stats_1H <- team_season_stats_1H %>%
  left_join(guard_pct, by = c('team_id', 'season'))





####
# Teams who go on 10+ point scoring runs (aka Kill Shot)
####

pbp_clean <- pbp %>%
  inner_join(date_ranges, by = "season") %>%
  filter(game_date >= start_date, game_date < mid_date) %>%
  arrange(season, game_id, period_number, row_number()) %>%
  group_by(game_id) %>%
  # Vectorized calculation of points per play
  mutate(
    #points scored on play
    pts = pmax(home_score - lag(home_score, default = 0), 
               away_score - lag(away_score, default = 0)),
    #who scored
    scoring_team_id = case_when(
      home_score > lag(home_score, default = 0) ~ home_team_id,
      away_score > lag(away_score, default = 0) ~ away_team_id,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(scoring_team_id)) %>%
  # logic for 10--0 run
  # rleid creates a unique ID every time the scoring team changes
  mutate(run_id = data.table::rleid(scoring_team_id)) %>%
  group_by(season, game_id, run_id, scoring_team_id) %>%
  summarise(run_pts = sum(pts), 
            .groups = "drop") %>%
  # Using floor division (%/%) handles cases like a 22-0 run (counts as 2 kill shots)
  # fixes glitch of some teams have 600 10-0 runs in one game
  mutate(ten_zero_runs = run_pts %/% 10,
         ten_zero_runs = ifelse(ten_zero_runs >2, 1, ten_zero_runs)) %>%
  filter(ten_zero_runs > 0)

# 4. Final Season Summarization
run_results <- pbp_clean %>%
  group_by(season, team_id = scoring_team_id) %>%
  summarise(kill_shot_count = sum(ten_zero_runs), 
            .groups = "drop")


team_season_stats_1H <- team_season_stats_1H %>%
  left_join(run_results, by = c('season', 'team_id')) %>%
  mutate(kill_shot_count = coalesce(kill_shot_count, 0))


#Final DF for 1st half of season stats
final_df_1H <- team_season_stats_1H %>%
  select(season, team_id, team_name, tourney_team_flag, 
         games, wins,  total_points, total_opp_points,
         fgpct, threeptpct, ftpct, treb, oreb, dreb, ast, stl, blk, to, pers_fouls, 
         ppg, opp_ppg, opp_fgpct, opp_threeptpct, efgpct, mov, pace, unadj_off_eff, unadj_def_eff,
         starters_shooting_threes_pct, three_pt_shooters_at_35pct_pct,
         extraScoreChances, oppTO_teamTO_ratio, ast_TO_ratio, foul_outs, 
         adj_bayes_win_rate_close, close_pct, blowout_pct,
         healthy_rate, pct_guards, kill_shot_count) %>%
  filter(tourney_team_flag == 1) %>%
  distinct()





################################################################################
### --- 8. Finalize dataframes for both 1H and 2H(Home), 2H(Away) ---
################################################################################


final_df_1H #1st half total stats

final_df_2H_home #2nd half home stats
final_df_2H_away #2nd half away stats

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
aa <- final_df_2H_home %>%
   mutate(
     across(
       where(is.numeric) & !c(tourney_team_flag, games, wins, total_points, total_opp_points), 
       ~ .x * weight_2hh
     )
   )

#weight 2nd half away
bb <- final_df_2H_away %>%
  mutate(
    across(
      where(is.numeric) & !c(tourney_team_flag, games, wins, total_points, total_opp_points), 
      ~ .x * weight_2ha
    )
  )
#weight 1st half
cc <- final_df_1H %>%
  mutate(
    across(
      where(is.numeric) & !c(tourney_team_flag, games, wins, total_points, total_opp_points), 
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


#player_exp_lookup, team_smdi, coach_lookup
chocolate_milk <- weighted_df %>%
  left_join(select(coach_lookup, -coach, -school), by = c('team_id' = 'team_id', 'season' = 'year')) %>%
  left_join(player_exp_lookup, by = c("season", "team_id")) %>%
  left_join(team_rotation_metrics, by = c("season", "team_id")) %>%
  #create pythagorean exp & wlPct AFTER weighting since I don't want to weight these
  mutate(wlpct = wins/ games,
         pyth_exp = total_points^11/(total_points^11 + total_opp_points^11),
         luck = pyth_exp - wlpct) %>%
  #drop variables that are not needed for final model
  select(-wins_close, -count_close, -starters, 
         -three_point_shooters, -threes35, -tourney_team_flag,
         -wins, -total_points, -total_opp_points, -games)


####################################################################################
have chatgpt review your code, clean it up if necessary, & then loop 2008:2025 to create full df
>> up to line 690 now (before 2nd H split)
>> you made date_ranges 1 table (start, mid, and end date)

add MOV variable.....surprised this doesnt alr exist
+ wlpct, tourney perf, and womens team in tourney (aka school spirit)

write up documentation for variables


THEN: make a model that is extremely simple. basically copy anthony klemm's project
>> take 3 inputs to decide: your model, simple model, and chalk / coin flip?