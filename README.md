# march-madness5
This is a personal project intended to predict the winners of March Madness matchups & my second attempt at trying to get a perfect bracket on the 2026 March Madness tournament. 


**Updates Needed to Make to Dataset:**
-   Update years & tourney dates under Configuration + Helper Data section
-   Check coach_lookup table for NA's in team_id column. Are there any team names you need to recode so the data joins?
-   Check coaches_final for exclusions to add (ie: removing a coach for a scandal, suspension, retiring, etc)
-   Check conf_perf for teams that win any round other than Final. Are the filters you have still working?
-   Update womens teams in tourney for that year (Check against: https://www.sports-reference.com/cbb/seasons/women/2025-school-stats.html)
-   Update power_conf in tourney that year. Also - confirm no teams joined or left Big Ten, SEC, ACC, Big12, Big East
-   Pull Bart Torvik CSV for the new year (should be two CSV's: 1st and 2nd half of season). Also - confirm no team_id's missing from Bart Torvik dataset. 

Update to Python:
https://www.kaggle.com/datasets/jonathanpilafas/2024-march-madness-statistical-analysis/data
