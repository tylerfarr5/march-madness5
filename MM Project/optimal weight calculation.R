#Function to apply weights
apply_weights <- function(df, weight) {
  exclude_cols <- c("season", "team_id", "tourney_team_flag", "games", "wins", "total_points", "total_opp_points")
  
  df %>%
    mutate(across(
      where(is.numeric) & !any_of(exclude_cols), 
      ~ .x * weight
    ))
}

#weights to decide on: 1h/2hh/2ha
#1. 0.1/0.4/0.5
#2. 0.1/0.3/0.6
#3. 0.1/0.2/0.7
#4. 0.1/0.1/0.8
#5. 0.2/0.4/0.4
#6. 0.2/0.3/0.5
#7. 0.2/0.2/0.6
#8. 0.3/0.3/0.4

#Creates weighted df
weighted_df <- list(
  final_df_1H        %>% apply_weights(weight_1h),
  final_df_2H_home   %>% apply_weights(weight_2hh),
  final_df_2H_away   %>% apply_weights(weight_2ha)
) %>%
  bind_rows() %>%
  # fills in conf perf since it isn't included on 1H dataframe
  group_by(season, team_id) %>% 
  fill(conf_perf, .direction = "downup") %>%
  #now group by correctly
  group_by(season, team_id, team_name, conf_perf) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), 
    .groups = "drop"
  ) 


# Final df for model
chocolate_milk <- weighted_df %>%
  left_join(select(coach_lookup, -coach, -school), by = c('team_id', 'season' = 'year')) %>%
  left_join(player_exp_lookup, by = c("season", "team_id")) %>%
  left_join(team_smdi, by = c("season", "team_id")) %>%
  left_join(womens_teams, by = c("season", "team_id")) %>%
  left_join(mm_perf, by = c("season", "team_id")) %>%
  #create pythagorean exp & wlPct AFTER weighting since I don't want to weight these
  mutate(
    wlpct    = wins / games,
    pyth_exp = total_points^11 / (total_points^11 + total_opp_points^11),
    luck     = pyth_exp - wlpct,
    deep_run = if_else(tourney_summary %in% 
                         c("Final", "F4", "E8", "S16"), 1, 0)
  ) %>%
  # Drop non-model variables
  select(-c(games, wins, tourney_team_flag,
            total_points, total_opp_points))


######
#Determine Optimal Weight
######


#Clean DF for modeling

opt_weight_clean_df <- chocolate_milk %>%
  select(-c(season, team_id, team_name, tourney_summary, tourney_perf))

opt_weight_clean_df$conf_perf <- as.factor(opt_weight_clean_df$conf_perf)
opt_weight_clean_df$womens_team_tourney_flag <- as.factor(opt_weight_clean_df$womens_team_tourney_flag)
opt_weight_clean_df$deep_run <- as.factor(opt_weight_clean_df$deep_run)


####  Fit Logistic Regression for all variables (GLM)
model <- glm(deep_run ~ ., data = opt_weight_clean_df, family = binomial(link = "logit"))


#calculate AUC
probs <- predict(model, type = "response")
roc_obj <- roc(opt_weight_clean_df$deep_run, probs, quiet = TRUE)

auc(roc_obj)


#Build table for evaluating weighting schemes
schemes <- tribble(
  ~id, ~w1h, ~w2hh, ~w2ha, ~auc_score,  ~auc_s16,
  1,  0.1,  0.4,  0.5,   0.8607,   0.8328,
  2,  0.1,  0.3,  0.6,   0.8603,   0.8327,
  3,  0.1,  0.2,  0.7,   0.8600,   0.8316,
  4,  0.1,  0.1,  0.8,   0.8592,   0.8302,
  5,  0.2,  0.4,  0.4,   0.8604,   0.8319,
  6,  0.2,  0.3,  0.5,   0.8604,   0.8318,
  7,  0.2,  0.2,  0.6,   0.8598,   0.8302,
  8,  0.3,  0.3,  0.4,   0.8599,   0.8296
)

#AUC is highest for the 0.1/0.4/0.5 weight scheme for both E8 and S16