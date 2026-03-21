######################
### Code for Motivation on Weighting by Time of Year
#####################
df_1st <- read.csv("df_1st.csv")
df_2nd_away <- read.csv("df_2nd_away.csv")
df_2nd_home <- read.csv("df_2nd_home.csv")

# 1. Create a function to calculate "Predictive Signal" 
# (Point-Biserial Correlation is great for Predictor vs. Binary Target)
get_signal <- function(df, label) {
  # 1. Isolate numeric columns
  numeric_cols <- df %>% 
    dplyr::select(where(is.numeric)) %>% 
    dplyr::select(-any_of(c("deep_run", "team_id", "season", "X")))
  
  # 2. Map the correlation test and "tidy" the results
  # We use purrr::map_df to stack the results into a dataframe automatically
  results <- purrr::map_df(names(numeric_cols), function(col_name) {
    test <- cor.test(df$deep_run, df[[col_name]])
    
    # Return a small tibble for this specific variable
    broom::tidy(test) %>% 
      mutate(Variable = col_name) %>%
      dplyr::select(Variable, Signal = estimate) # 'estimate' is the correlation coefficient
  })
  
  # 3. Add the Period label
  results %>%
    mutate(Period = label,
           Signal = abs(Signal))
}

# 2. Process your 3 dataframes
# Replace 'df_1st', etc., with your actual object names
data_1st  <- get_signal(df_1st, "1st Half")
data_home <- get_signal(df_2nd_home, "2nd Half - Home")
data_away <- get_signal(df_2nd_away, "2nd Half - Away")

# 3. Combine and Format
full_data <- rbind(data_1st, data_home, data_away)
full_data$Period <- factor(full_data$Period, levels = c("1st Half", "2nd Half - Home", "2nd Half - Away"))

# 4. Generate the One-Graph View
ggplot(full_data, aes(x = Period, y = Variable, fill = Signal)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#f7fbff", high = "#084594", name = "Signal Strength") +
  labs(
    title = "Predictive Signal Strength Across Season Segments",
    subtitle = "Higher intensity indicates the 1st Half games provide the most meaningful data",
    x = "Season Timeline",
    y = "Predictor Variables"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 16),
    panel.grid = element_blank()
  )


# #############################################################################
# #############################################################################
# #############################################################################
# 
# 
# 
# #Function to apply weights
# #Function to apply weights
# apply_weights <- function(df, weight) {
#   exclude_cols <- c("season", "team_id", "tourney_team_flag", "games", "wins", "total_points", "total_opp_points")
#   
#   df %>%
#     mutate(across(
#       where(is.numeric) & !any_of(exclude_cols), 
#       ~ .x * weight
#     ))
# }
# 
# #weights to decide on: 1h/2hh/2ha
# #1. 0.1/0.4/0.5
# #2. 0.1/0.3/0.6
# #3. 0.1/0.2/0.7
# #4. 0.1/0.1/0.8
# #5. 0.2/0.4/0.4
# #6. 0.2/0.3/0.5
# #7. 0.2/0.2/0.6
# #8. 0.3/0.3/0.4
# 
# weight_1h <- 0.5
# weight_2hh <- 0.4
# weight_2ha <- 0.1
# 
# #Creates weighted df
# weighted_df <- list(
#   final_df_1H        %>% apply_weights(weight_1h),
#   final_df_2H_home   %>% apply_weights(weight_2hh),
#   final_df_2H_away   %>% apply_weights(weight_2ha)
# ) %>%
#   bind_rows() %>%
#   # fills in conf perf since it isn't included on 1H dataframe
#   group_by(season, team_id) %>% 
#   fill(conf_perf, .direction = "downup") %>%
#   #now group by correctly
#   group_by(season, team_id, team_name, conf_perf) %>%
#   summarise(
#     across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), 
#     .groups = "drop"
#   ) 
# 
# # Final df for model
# chocolate_milk <- weighted_df %>%
#   left_join(select(coach_lookup, -coach, -school), by = c('team_id', 'season' = 'year')) %>%
#   left_join(player_exp_lookup, by = c("season", "team_id")) %>%
#   left_join(team_smdi, by = c("season", "team_id")) %>%
#   left_join(womens_teams, by = c("season", "team_id")) %>%
#   left_join(power_conf, by = c("season", "team_id")) %>%
#   left_join(mm_perf, by = c("season", "team_id")) %>%
#   #create pythagorean exp & wlPct AFTER weighting since I don't want to weight these
#   mutate(
#     wlpct    = wins / games,
#     pyth_exp = total_points^8.6 / (total_points^8.6 + total_opp_points^8.6),
#     luck     = pyth_exp - wlpct,
#     deep_run = if_else(tourney_summary %in% 
#                          c("Final", "F4", "E8"), 1, 0)
#   ) %>%
#   # Drop non-model variables
#   select(-c(games, wins, tourney_team_flag,
#             total_points, total_opp_points))
# 
# 
# 
# ######
# #Determine Optimal Weight
# ######
# 
# 
# #Clean DF for modeling
# 
# opt_weight_clean_df <- chocolate_milk %>%
#   dplyr::select(-c(season, team_id, team_name, tourney_summary, tourney_perf, Conference))
# 
# opt_weight_clean_df$conf_perf <- as.factor(opt_weight_clean_df$conf_perf)
# opt_weight_clean_df$womens_team_tourney_flag <- as.factor(opt_weight_clean_df$womens_team_tourney_flag)
# opt_weight_clean_df$deep_run <- as.factor(opt_weight_clean_df$deep_run)
# opt_weight_clean_df$power_conf <- as.factor(opt_weight_clean_df$power_conf)
# opt_weight_clean_df$power_conf_extra <- as.factor(opt_weight_clean_df$power_conf_extra)
# 
# 
# ####  Fit Logistic Regression for all variables (GLM)
# set.seed(123)
# model <- glm(deep_run ~ ., data = opt_weight_clean_df, family = binomial(link = "logit"))
# 
# 
# #calculate AUC
# probs <- predict(model, type = "response")
# roc_obj <- roc(opt_weight_clean_df$deep_run, probs, quiet = TRUE)
# 
# auc(roc_obj)
# 
# 
# #Build table for evaluating weighting schemes
# schemes <- tribble(
#   ~id, ~w1h, ~w2hh, ~w2ha, ~auc_score,
#   1,  0.1,  0.4,  0.5,   0.875,
#   2,  0.1,  0.3,  0.6,   0.8757,  
#   3,  0.1,  0.2,  0.7,   0.8756,
#   4,  0.1,  0.1,  0.8,   0.875, 
#   5,  0.2,  0.4,  0.4,   0.8754,  
#   6,  0.2,  0.3,  0.5,   0.8758,
#   7,  0.2,  0.2,  0.6,   0.876,
#   8,  0.3,  0.3,  0.4,   0.8772
# )
# 
# 
# #AUC is highest for the 0.3/0.3/0.4 weight scheme for E8
# 
# #Also note: since 1H is surprisingly favored, I tried:  (use 0.5/0.3/0.2)
# #1h/2hh/2ha
# #1. 0.4/0.2/0.4 -- 0.8784
# #2. 0.4/0.3/0.3 -- 0.8786
# #3. 0.5/0.3/0.2 -- 0.8794
# #4. 0.5/0.4/0.1 -- 0.8785