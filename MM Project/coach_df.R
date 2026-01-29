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

