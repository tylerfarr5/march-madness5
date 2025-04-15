library(hoopR)
library(dplyr)

#setting up all teams
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



szn_stats <- load_mbb_team_box()
