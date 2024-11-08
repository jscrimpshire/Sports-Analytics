#Load necessary libraries
library(tidyverse)
library(worldfootballR)
library(goalmodel)
library(ggplot2)
library(reshape2)

# function to extract EPL match results data
epl_match_results <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2024, tier = "1st")

#filter matches up to matchweek 10
epl_match_results <- epl_match_results %>%
  filter(Date < as.Date("2023-11-01"))

#set a recency weight for the Dixon-Coles model (you can change as you see fit)
my_weights <- weights_dc(epl_match_results$Date, xi=0.0019)

#plot the weights as a function of date to understand how it will affect the results
plot(epl_match_results$Date, my_weights)

#create the weighted Poisson model (the Dixon-Coles model requires integers, but since we are using xG, we will use the Poisson)
gm_res_w <- goalmodel(goals1 = epl_match_results$Home_xG, goals2 = epl_match_results$Away_xG,
                      team1 = epl_match_results$Home, team2 = epl_match_results$Away,
                      weights = my_weights)

#get a summary of the model
summary(gm_res_w)

# Define a function to simulate match outcomes
simulate_match <- function(team1, team2, model) {
  pred <- predict_expg(model, team1 = team1, team2 = team2, return_df = TRUE)
  goals1 <- rpois(1, pred$expg1)
  goals2 <- rpois(1, pred$expg2)
  return(c(goals1, goals2))
}

# Get all of the future matches (that we want to predict)
future_matches <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2024, tier = "1st") %>%
  filter(Date >= as.Date("2023-11-01"))

#get the list of unique teams
teams <- unique(c(future_matches$Home, future_matches$Away))
#set your number of simulations (I use 10,000 because it's a nice number)
num_simulations <- 10000
points_matrix <- matrix(0, nrow = length(teams), ncol = num_simulations)
rownames(points_matrix) <- teams

#run the Monte Carlo simulations
for (sim in 1:num_simulations) {
  points <- setNames(rep(0, length(teams)), teams)
  for (match in 1:nrow(future_matches)) {
    team1 <- future_matches$Home[match]
    team2 <- future_matches$Away[match]
    result <- simulate_match(team1, team2, gm_res_w)
    if (result[1] > result[2]) {
      points[team1] <- points[team1] + 3
    } else if (result[1] < result[2]) {
      points[team2] <- points[team2] + 3
    } else {
      points[team1] <- points[team1] + 1
      points[team2] <- points[team2] + 1
    }
  }
  points_matrix[, sim] <- points
}

#Summarize and plot the results
points_summary <- apply(points_matrix, 1, mean)

# Convert the matrix to a data frame for plotting
points_df <- as.data.frame(t(points_matrix))
colnames(points_df) <- rownames(points_matrix)
points_df <- points_df %>%
  pivot_longer(cols = everything(), names_to = "Team", values_to = "Points")

# Plot histogram for each team
ggplot(points_df, aes(x = Points, fill = Team)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.7) +
  facet_wrap(~ Team, scales = "free_y") +
  theme_minimal() +
  labs(x = "Points", y = "Frequency", fill = "Team",
       title = "Distribution of Expected Points for Each Team from Monte Carlo Simulations")

# Display the mean points for each team
print(points_summary)

#convert to a dataframe for ease
points_summary_df <- as.data.frame(points_summary)


# Create a named vector for current points at matchweek 10
current_points <- c(
  "Arsenal" = 24, "Aston Villa" = 22, "Bournemouth" = 6, "Brentford" = 13,
  "Brighton" = 17, "Burnley" = 4, "Chelsea" = 12, "Crystal Palace" = 12,
  "Everton" = 10, "Fulham" = 12, "Liverpool" = 23, "Luton Town" = 5,
  "Manchester City" = 24, "Manchester Utd" = 15, "Newcastle Utd" = 17,
  "Nott'ham Forest" = 10, "Sheffield Utd" = 1, "Tottenham" = 26,
  "West Ham" = 14, "Wolves" = 12
)

# Add a column for current points by matching team names
points_summary_df$current_points <- sapply(rownames(points_summary_df), function(team) current_points[team])

# Add a column for expected total points
points_summary_df$expected_total_points <- points_summary_df$points_summary + points_summary_df$current_points

#get the final table from the 23/24 epl season
epl_table <- fb_season_team_stats(country = "ENG", gender = "M", season_end_year = "2024", tier = "1st", stat_type = "league_table_home_away")

epl_table <- epl_table %>%
  mutate(Points = as.numeric(Pts_Home) + as.numeric(Pts_Away))

#rename the column
points_summary_df <- points_summary_df %>%
  tibble::rownames_to_column("Squad")

#join our expected points total with the actual points total
points_summary_df <- points_summary_df %>%
  left_join(epl_table %>% select(Squad, Points), by = "Squad")

# Define the limits for both axes (use the min and max values from your data)
min_limit <- min(points_summary_df$Points, points_summary_df$expected_total_points)
max_limit <- max(points_summary_df$Points, points_summary_df$expected_total_points)

#graph the expected vs actual total points
ggplot(data = points_summary_df, aes(x = expected_total_points, y = Points)) +
  geom_point(color = "#4E79A7", size = 3, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_text_repel(aes(label = Squad), size = 3, max.overlaps = 10, color = "gray20") +
  # Add annotations for model under-performance and over-performance
  annotate("text", x = max_limit * 0.3, y = max_limit * 0.7, label = "Model under-predicts", color = "black", fontface = "bold") +
  annotate("text", x = max_limit * 0.7, y = max_limit * 0.3, label = "Model over-predicts", color = "black", fontface = "bold") +
  # Add arrows for the annotations
  annotate("segment", x = max_limit * 0.3, y = max_limit * 0.7, xend = max_limit * 0.15, yend = max_limit * 0.85, 
           arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  annotate("segment", x = max_limit * 0.7, y = max_limit * 0.3, xend = max_limit * 0.85, yend = max_limit * 0.15, 
           arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  labs(
    title = "Calibration of xPoints Model",
    subtitle = "Premier League 2023/24",
    x = "Expected Points",
    y = "Actual Points",
    caption = "10,000 Monte Carlo Markov Chain simulations were run on a recency-weighted Poisson model based on xG results after Matchweek 10\n The xPoints value for each team is the mean expected points value from all simulations | Created by @jscrimpSTATS"
  ) +
  coord_fixed(ratio = 1) +  # Forces a 1:1 aspect ratio
  xlim(min_limit, max_limit) +  # Set the same limits for x and y
  ylim(min_limit, max_limit) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 8, hjust = 0, color = "gray50"), # Decrease font size for caption
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85")
  )


# Calculate residuals
points_summary_df$residuals <- points_summary_df$Points - points_summary_df$expected_total_points

#graph the residuals
ggplot(data = points_summary_df, aes(x = expected_total_points, y = residuals)) +
  geom_point(color = "#4E79A7", size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") + # Reference line at 0
  geom_text_repel(aes(label = Squad), size = 3, max.overlaps = 10, color = "gray20") +
  labs(
    title = "Residuals of xPts Model",
    subtitle = "Premier League 23/24 | MCMC Dixon-Coles Model run after Matchweek 10",
    x = "Expected Points",
    y = "Residuals (Actual - Expected Points)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85")
  )
