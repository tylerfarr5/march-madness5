#######
## next steps in EDA:
#######

#injuries
#guard/forward position analysis
#show investigation of any teams with proportionally high home/away games
#decide on which bart torvik stats to inclue (if any)
#home/away matrix



################################################################################
#code below shows a start on analyzing injuries for EDA
# only use injuries for EDA, but you cannot include them as variables

##### Teams with injured players
#Will pull minutes played by each player during regular season
#compare to minutes played during tourney
#If they don't play (or have limited min) in the tourney, flag them as being injured/out/dnp
#show as minutes based metric, points based metric
##############################################################################

#proof that min date in a season is november
for(i in 2008:2025) {
  print(paste(i, min(load_mbb_team_box(seasons = i)$game_date)))
}

#proof that max date in season is april
for(i in 2008:2025) {
  print(paste(i, min(load_mbb_team_box(seasons = i)$game_date)))
}

#date table ranging from start of season to tourney
date_range_pre_tourney <- tibble(
  year = year(tourney_dates),
  start_date = as.Date(paste0(year(tourney_dates)-1, "-10-01")),
  end_date = tourney_dates
)

#date table ranging from tourney to end of tourney
date_range_tourney <- tibble(
  year = year(tourney_dates),
  start_date = tourney_dates+1,
  end_date = as.Date(paste0(year(tourney_dates), "-05-01"))
)

#total minutes & points played in season by a team
#**filter to pre tourney
View(
  players %>%
    group_by(season, team_id) %>%
    summarise(
      season_minutes = sum(minutes, na.rm = TRUE),
      season_points = sum(points, na.rm = TRUE),
      games_played = n_distinct(game_id),
      players = n_distinct(athlete_id),
      .groups = "drop"
    )
)

#join this ^ to the player level table
#**filter to  pre tourney

players %>%
  group_by(season, team_id, athlete_id) %>%
  summarise(
    season_minutes = sum(minutes, na.rm = TRUE),
    season_points = sum(points, na.rm = TRUE),
    games_played = n_distinct(game_id),
    .groups = "drop"
  )

#then compare these results to players during tourney && how the team did


################################################################################
# Guard/Forward position
##############################################################################

#look at tourney performance vs points & minutes of players by position