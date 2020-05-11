# NFL Capstone Project

**Introduction to the Topic:** Perception is that games on Thursday are different from non-Thursday games. If this is so…

* How does this manifest on game day? 

* What is different about Thursday games from non-Thursday games? 

We examined the following variables for differences on Thursday and non-Thursday games:

1. Travel Time

2. ELO win probabilities 

3. Expected Points (added)

4. Run/Pass Completion

**Data:** The data for this project is owned and maintained by the NFL. Thus, no data is included in the public repository. We used 2 large data.frames: play-by-play (`pbp`) and games (`games`).  Calls to these data.frames will be seen in our work. 

**File Descriptions:**

* data_dictionary.RMD: a dictionary that includes all variable descriptions 

* dataframes.R: a file that includes all necessary data-wrangling and data.frame management. 

* calculating_distance.Rmd: calculates and graphs distance traveled by teams

* Distance and Accuracy.Rmd: calculate pass completion status and compares to  travel distance 

* expected_points.R: examines expected points added (EPA) on Thursday and non-Thursday games. 

* nfl_538_team_codes.R: includes ELO ratings for teams and replicates calculations

* rankings.R: calculating rankings for teams before games 

* Scores.Rmd: function for calculating final score and margin of victory for all the games of a given team in a given season

* travel_permutation.Rmd: permutation tests for difference in travel times for different variables 

* travel_vs_margin_of_victory.R: compares travel time and margin of victory for teams

* vis_team_strength.Rmd: calculating a strength metric for visiting teams utilizing ELO rankings 

* win_travel_permutation.R: permutation test for difference in travel time and win status








