library(nflfastR)
library(tidyverse)
library(ggrepel)
library(ggplot2)

###Load play by play data
seasons<- 2023:24
--pbp<- nflfastR::load_pbp(seasons)
pbp<- read.csv("pbp_history.csv")%>%
  filter(season==seasons)


colr<-teams_colors_logos%>% select(team_abbr,team_color, team_color2,team_logo_espn)

#Background Analysis####
###Analyse QB Stats
fgstats <-pbp%>% 
  filter(
    play_type=="field_goal"
  )%>%
  mutate(kick_distance_group = floor(kick_distance / 5) * 5) %>%  # bin into 5-yard groups
  group_by(kick_distance_group)%>%
  summarise(
    attempts = sum(field_goal_attempt),
    made= sum(success),
    miss=sum(field_goal_attempt-success),
    pct=sum(success)/sum(field_goal_attempt)
  )

###GPT Output for chart
ggplot(fgstats, aes(x = kick_distance_group, y = pct)) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(min(fgstats$kick_distance_group), max(fgstats$kick_distance_group), by = 10),
    name = "Kick Distance (yards)"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),  # 0.05 = 5% increments
    labels = scales::percent_format(accuracy = 1),
    name = "Field Goal Percentage"
  ) +
  labs(
    x = "Kick Distance (yards)",
    y = "Field Goal Percentage",
    title = "Field Goal Percentage by Kick Distance and Season"
  ) +
  theme_minimal()


##GPT Output
fgstats2 <- pbp %>% 
  filter(play_type == "field_goal") %>%
  mutate(kick_distance_group = floor(kick_distance / 5) * 5) %>%  # bin into 5-yard groups
  group_by(kick_distance_group, season) %>%
  summarise(
    attempts = sum(field_goal_attempt, na.rm = TRUE),
    made = sum(success, na.rm = TRUE),
    miss=sum(field_goal_attempt-success),
    pct = made / attempts,
    .groups = "drop"
  )

ggplot(fgstats2, aes(x = kick_distance_group, y = pct, color = factor(season))) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(min(fgstats$kick_distance_group), max(fgstats$kick_distance_group), by = 10),
    name = "Kick Distance (yards)"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),  # 0.05 = 5% increments
    labels = scales::percent_format(accuracy = 1),
    name = "Field Goal Percentage"
  ) +
  labs(color = "Season", title = "Field Goal Percentage by Kick Distance Group and Season") +
  theme_minimal()

##Combined Chart
# Fit regression to average line
model <- lm(pct ~ kick_distance_group, data = fgstats)
slope <- coef(model)[2]  # slope coefficient

# Create label text (convert slope to % change per yard)
slope_label <- paste0("Slope: ", round(slope * 100, 2), "% per yard")

ggplot() +
  # Seasonal dashed lines
  geom_line(data = fgstats2, aes(x = kick_distance_group, y = pct, group = season, color = factor(season)), 
            linetype = "dashed", size = 0.8) +
  # Overall average solid line
  geom_line(data = fgstats, aes(x = kick_distance_group, y = pct), 
            color = "black", size = 1.2) +
  annotate("text", x = max(fgstats$kick_distance_group) - 10, 
           y = max(fgstats$pct) - 0.05, 
           label = slope_label, hjust = 1, size = 4, fontface = "italic") +
  scale_x_continuous(
    breaks = seq(min(fgstats$kick_distance_group), max(fgstats$kick_distance_group), by = 10),
    name = "Kick Distance (yards)"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1),
    name = "Field Goal Percentage"
  ) +
  labs(
    color = "Season",
    title = "Field Goal Percentage by Kick Distance Group",
    subtitle = "Dashed = Individual Seasons | Solid = All-Seasons Average"
  ) +
  theme_minimal()

###Field Goal Attempts by Kick Distance and Season
ggplot(fgstats2, aes(x = factor(kick_distance_group),
                     y = total_attempts,
                     fill = factor(season))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Set2", name = "Season") +
  labs(
    title = "Field Goal Attempts by Kick Distance Group and Season",
    x = "Kick Distance Group (yds)",
    y = "Total Attempts"
  ) +
  theme_minimal(base_size = 14)

###Hopefully last
# Fit a quadratic model to average data
model_poly <- lm(pct ~ poly(kick_distance_group, 2), data = fgstats)

# Expanded coefficients for exact formula
model_expanded <- lm(pct ~ kick_distance_group + I(kick_distance_group^2), data = fgstats)
coef(model_expanded)

# Add fitted values for plotting
fgstats$fit <- predict(model_poly)

# Plot
ggplot() +
  # Seasonal dashed lines
  geom_line(data = fgstats2, aes(x = kick_distance_group, y = pct, group = season, color = factor(season)), 
            linetype = "dashed", size = 0.8) +
  # Polynomial fit line for average
  geom_line(data = fgstats, aes(x = kick_distance_group, y = fit), 
            color = "black", size = 1.2) +
  # Average points
  geom_point(data = fgstats, aes(x = kick_distance_group, y = pct), color = "black", size = 2) +
  scale_x_continuous(
    breaks = seq(min(fgstats$kick_distance_group), max(fgstats$kick_distance_group), by = 10),
    name = "Kick Distance (yards)"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1),
    name = "Field Goal Percentage"
  ) +
  labs(
    color = "Season",
    title = "Field Goal Percentage by Kick Distance Group",
    subtitle = "Dashed = Individual Seasons | Solid = Polynomial Fit to All-Seasons Average"
  ) +
  theme_minimal()

####Model Tuning

library(dplyr)

# ----------  model coefficients (from your output) ----------
b0 <- 0.7926562234
b1 <- 0.0183288648
b2 <- -0.0004198852

# ---------- scoring parameters you can tune ----------
min_made <- 3    # points for easiest made
max_made <- 6    # points for hardest made
min_miss <- -1   # points for easiest miss
max_miss <- -6   # points for hardest miss

# If you want attempt points = expectation of outcomes, use that (recommended).
use_expected_for_attempt <- TRUE
# Otherwise set attempt_base and attempt_scale for a linear attempt reward
attempt_base  <- 0
attempt_scale <- 0

# ---------- helper functions ----------
p_hat <- function(x) {
  # predicted probability for distance x
  # ensure x numeric (yards)
  y <- b0 + b1 * x + b2 * x^2
  # clamp between 0 and 1
  pmax(0, pmin(1, y))
}

# choose domain for normalization: compute min and max d on realistic distances (e.g. 0..70)
x_min <- 0
x_max <- 70
d_vals <- 1 - p_hat(seq(x_min, x_max, by = 1))
min_d <- min(d_vals)
max_d <- max(d_vals)

d_norm <- function(x) {
  d <- 1 - p_hat(x)
  (d - min_d) / (max_d - min_d)  # in [0,1]
}

points_made <- function(x) {
  mn <- d_norm(x)
  min_made + mn * (max_made - min_made)
}

points_miss <- function(x) {
  mn <- d_norm(x)
  min_miss + mn * (max_miss - min_miss)
}

points_attempt <- function(x) {
  if (use_expected_for_attempt) {
    p <- p_hat(x)
    p * points_made(x) + (1 - p) * points_miss(x)
  } else {
    attempt_base + attempt_scale * d_norm(x)
  }
}

# ---------- prepare pbp with grouped distances (same as you had) ----------
pbp2 <- pbp %>%
  filter(play_type == "field_goal") %>%
  mutate(
    kick_distance_group = floor(kick_distance / 5) * 5,   # keep your grouping scheme
    # get per-kick predicted p and points
    p_hat = p_hat(kick_distance_group),
    d_norm = d_norm(kick_distance_group),
    point_if_made = points_made(kick_distance_group),
    point_if_miss = points_miss(kick_distance_group),
    point_if_attempt = points_attempt(kick_distance_group),
    # event-level points actual (depending on success)
    event_points = ifelse(success == 1, point_if_made, point_if_miss)
  )

# ---------- Aggregations for categories you listed ----------
# A helper that creates bin counts you asked for:
agg <- pbp2 %>%
  mutate(
    # Threshold flags for thresholds you listed (>= threshold)
    made_ge_1   = as.integer(success == 1 & kick_distance_group >= 1),
    made_ge_5   = as.integer(success == 1 & kick_distance_group >= 5),
    made_ge_10  = as.integer(success == 1 & kick_distance_group >= 10),
    made_ge_20  = as.integer(success == 1 & kick_distance_group >= 20),
    made_ge_25  = as.integer(success == 1 & kick_distance_group >= 25),
    made_ge_50  = as.integer(success == 1 & kick_distance_group >= 50),
    made_ge_100 = as.integer(success == 1 & kick_distance_group >= 100),
    miss_ge_1   = as.integer(success == 0 & kick_distance_group >= 1),
    miss_ge_5   = as.integer(success == 0 & kick_distance_group >= 5),
    miss_ge_10  = as.integer(success == 0 & kick_distance_group >= 10),
    miss_ge_20  = as.integer(success == 0 & kick_distance_group >= 20),
    miss_ge_25  = as.integer(success == 0 & kick_distance_group >= 25),
    miss_ge_50  = as.integer(success == 0 & kick_distance_group >= 50),
    miss_ge_100 = as.integer(success == 0 & kick_distance_group >= 100),
    # distance buckets you requested
    made_0_39 = as.integer(success == 1 & kick_distance_group >= 0  & kick_distance_group <= 39),
    miss_0_39 = as.integer(success == 0 & kick_distance_group >= 0  & kick_distance_group <= 39),
    att_0_39  = as.integer(kick_distance_group >= 0 & kick_distance_group <= 39),
    made_40_49 = as.integer(success == 1 & kick_distance_group >= 40 & kick_distance_group <= 49),
    miss_40_49 = as.integer(success == 0 & kick_distance_group >= 40 & kick_distance_group <= 49),
    att_40_49  = as.integer(kick_distance_group >= 40 & kick_distance_group <= 49),
    made_50_59 = as.integer(success == 1 & kick_distance_group >= 50 & kick_distance_group <= 59),
    miss_50_59 = as.integer(success == 0 & kick_distance_group >= 50 & kick_distance_group <= 59),
    att_50_59  = as.integer(kick_distance_group >= 50 & kick_distance_group <= 59),
    made_60p = as.integer(success == 1 & kick_distance_group >= 60),
    miss_60p = as.integer(success == 0 & kick_distance_group >= 60),
    att_60p  = as.integer(kick_distance_group >= 60)
  )

# Now summarize per-player (or global) for all categories and sum points
summary_by_player <- agg %>%
  group_by(kicker_player_id) %>%    # replace with player id field you have
  summarise(
    total_made = sum(success == 1),
    total_missed = sum(success == 0),
    total_attempted = n(),
    # thresholds (counts)
    kicks_made_ge_1   = sum(made_ge_1),
    kicks_made_ge_5   = sum(made_ge_5),
    kicks_made_ge_10  = sum(made_ge_10),
    kicks_made_ge_20  = sum(made_ge_20),
    kicks_made_ge_25  = sum(made_ge_25),
    kicks_made_ge_50  = sum(made_ge_50),
    kicks_made_ge_100 = sum(made_ge_100),
    kicks_missed_ge_1   = sum(miss_ge_1),
    kicks_missed_ge_5   = sum(miss_ge_5),
    kicks_missed_ge_10  = sum(miss_ge_10),
    kicks_missed_ge_20  = sum(miss_ge_20),
    kicks_missed_ge_25  = sum(miss_ge_25),
    kicks_missed_ge_50  = sum(miss_ge_50),
    kicks_missed_ge_100 = sum(miss_ge_100),
    # buckets
    made_0_39 = sum(made_0_39),
    miss_0_39 = sum(miss_0_39),
    att_0_39  = sum(att_0_39),
    made_40_49 = sum(made_40_49),
    miss_40_49 = sum(miss_40_49),
    att_40_49  = sum(att_40_49),
    made_50_59 = sum(made_50_59),
    miss_50_59 = sum(miss_50_59),
    att_50_59  = sum(att_50_59),
    made_60p = sum(made_60p),
    miss_60p = sum(miss_60p),
    att_60p  = sum(att_60p),
    # points
    total_event_points = sum(event_points),
    total_attempt_points = sum(point_if_attempt)
  ) %>%
  ungroup()
###Scoring Bins from ESPN
fgstats_scoring <- pbp %>% 
  filter(
    play_type == "field_goal",
    season > 2020
  ) %>%
  mutate(
    yard_bin = case_when(
      kick_distance < 40 ~ "0–39",
      kick_distance >= 40 & kick_distance <= 49 ~ "40–49",
      kick_distance >= 50 & kick_distance <= 59 ~ "50–59",
      kick_distance >= 60 ~ "60+",
      TRUE ~ NA_character_
    ),
    yard_bin = factor(yard_bin, levels = c("0–39", "40–49", "50–59", "60+"))
  ) %>%
  group_by(yard_bin, season) %>%
  summarise(
    attempts = sum(field_goal_attempt, na.rm = TRUE),
    made = sum(success, na.rm = TRUE),
    miss = sum(field_goal_attempt - success, na.rm = TRUE),
    pct = made / attempts,
    .groups = "drop"
  )
### new scoring chart
fgstats_all <- fgstats_scoring %>%
  group_by(yard_bin) %>%
  summarise(
    pct = sum(made) / sum(attempts),
    .groups = "drop"
  )

# Plot
ggplot() +
  # Dashed lines for each season
  geom_line(
    data = fgstats_scoring,
    aes(x = yard_bin, y = pct, group = season, color = factor(season)),
    linetype = "dashed",
    size = 1
  ) +
  # Solid black line for all seasons combined
  geom_line(
    data = fgstats_all,
    aes(x = yard_bin, y = pct, group = 1),
    color = "black",
    size = 1.2
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Kick Distance Bin (yards)",
    y = "Field Goal Percentage",
    color = "Season",
    title = "Field Goal Percentage by Distance Bin"
  ) +
  theme_minimal(base_size = 14)
###Fitted Regression
# Define midpoints for each yard_bin category
midpoints <- c("0–39" = 20, "40–49" = 45, "50–59" = 55, "60+" = 65)

fgstats_all <- fgstats_all %>%
  mutate(mid = midpoints[as.character(yard_bin)])

# Fit linear regression: pct ~ mid
model_linear <- lm(pct ~ mid, data = fgstats_all)

summary(model_linear)
###New Scoring

# Step 1: Calculate total points per kicker per season (using original_points)
season_points <- pbp %>%
  filter(play_type == "field_goal") %>%
  mutate(
    yard_bin = case_when(
      kick_distance < 40 ~ "0–39",
      kick_distance >= 40 & kick_distance <= 49 ~ "40–49",
      kick_distance >= 50 & kick_distance <= 59 ~ "50–59",
      kick_distance >= 60 ~ "60+",
      TRUE ~ NA_character_
    ),
    original_points = case_when(
      success == 1 & yard_bin == "0–39" ~ 3,
      success == 1 & yard_bin == "40–49" ~ 4,
      success == 1 & yard_bin == "50–59" ~ 5,
      success == 1 & yard_bin == "60+" ~ 6,
      success == 0 ~ -1,
      TRUE ~ 0
    )
  ) %>%
  group_by(season, kicker_player_name) %>%
  summarise(
    season_total_points = sum(original_points, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Identify top 12 kickers per season
top_kickers <- season_points %>%
  group_by(season) %>%
  slice_max(order_by = season_total_points, n = 12, with_ties = FALSE) %>%
  ungroup()

# Step 3 & 4: Filter original pbp_scoring dataset to only these kickers per season, add week and season
pbp_top12 <- pbp %>%
  filter(play_type == "field_goal") %>%
  mutate(
    yard_bin = case_when(
      kick_distance < 40 ~ "0–39",
      kick_distance >= 40 & kick_distance <= 49 ~ "40–49",
      kick_distance >= 50 & kick_distance <= 59 ~ "50–59",
      kick_distance >= 60 ~ "60+",
      TRUE ~ NA_character_
    ),
    original_points = case_when(
      success == 1 & yard_bin == "0–39" ~ 3,
      success == 1 & yard_bin == "40–49" ~ 4,
      success == 1 & yard_bin == "50–59" ~ 5,
      success == 1 & yard_bin == "60+" ~ 6,
      success == 0 ~ -1,
      TRUE ~ 0
    ),
    new_points = case_when(
      success == 1 & yard_bin == "0–39" ~ 3,
      success == 1 & yard_bin == "40–49" ~ 3.75,
      success == 1 & yard_bin == "50–59" ~ 4,
      success == 1 & yard_bin == "60+" ~ 5,
      success == 0 & yard_bin == "0–39" ~ -3,
      success == 0 & yard_bin == "40–49" ~ -2.25,
      success == 0 & yard_bin == "50–59" ~ -2,
      success == 0 & yard_bin == "60+" ~ -1,
      TRUE ~ 0
    )
  ) %>%
  inner_join(top_kickers, by = c("season", "kicker_player_name")) %>%
  group_by(game_id, season, week, kicker_player_name) %>%
  summarise(
    total_original_points = sum(original_points, na.rm = TRUE),
    total_new_points = sum(new_points, na.rm = TRUE),
    change_in_points = total_new_points - total_original_points,
    .groups = "drop"
  )

###Final Methodololigy 2024 point chart
pbp_2024_sorted <- pbp_top12 %>%
  filter(season == 2024) %>%
  group_by(kicker_player_name) %>%
  arrange(desc(total_original_points), .by_group = TRUE) %>%
  mutate(instance = row_number()) %>%
  ungroup()

ggplot(pbp_2024_sorted, aes(x = factor(instance))) +
  # draw boxes between original and new points
  geom_rect(aes(
    xmin = as.numeric(factor(instance)) - 0.4,
    xmax = as.numeric(factor(instance)) + 0.4,
    ymin = pmin(total_original_points, total_new_points),
    ymax = pmax(total_original_points, total_new_points)
  ),
  fill = "grey90", color = "black", alpha = 0.6
  ) +
  # original point at top of box
  geom_point(aes(y = total_original_points), color = "black", size = 2) +
  # new point at bottom of box
  geom_point(aes(y = total_new_points), color = "red", size = 2) +
  facet_wrap(~ kicker_player_name, scales = "free_x") +
  labs(
    title = "Original vs New Points per Game by Kicker (2024)",
    x = "Game Instances (Sorted by Original Points per Kicker)",
    y = "Points"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing.x = unit(1, "lines"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

