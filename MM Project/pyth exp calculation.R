
pyth_exp_calculation <- weighted_df %>%
  mutate(wlpct = wins / games) %>%
  filter(season %in% 2018:2025) %>% #choosing more recent tournaments
  select(season, team_id, team_name, wlpct, total_points, total_opp_points)


#Logic to determine pythagorean expectation variable - #grid searching all possible values from 2 to 20 by 0.1
num_list <- seq(2,20,0.1)
dict <- data.frame(Exponent = as.numeric(), MAE = as.numeric())

for (i in num_list) {
  
  pythag <- (pyth_exp_calculation$total_points^i)/(pyth_exp_calculation$total_points^i + pyth_exp_calculation$total_opp_points^i)
  
  mae <- mean(abs(pyth_exp_calculation$wlpct - pythag))
  
  dict[nrow(dict)+1,] <- c(i, mae)
}

#MAE is lowest when exponent = 8.6