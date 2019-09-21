library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)

# Load data
pbp_all <- readRDS("nflScrapRAllPlays.rds")
nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
team_wins_18 <- read_csv("2018Wins.csv")

# Clean and define plays
pbp_all_rp <- pbp_all %>%
  filter(!is_na(epa), !is_na(posteam), play_type=="no_play" | play_type=="pass" | play_type=="run") %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0),
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    yards_gained=ifelse(play_type=="no_play",NA,yards_gained),
    play=1
  ) %>%
  filter(pass==1 | rush==1)

pbp_all_rp %>% filter(rush == 1) %>% select(rush, desc) %>% head

# Find teams' early down pass and rush rates
team_ED_RP <- pbp_all_rp %>%
  filter(season == 2018 & down <= 2 & wp >= 0.2 & wp <= 0.8) %>%
  group_by(posteam) %>%
  summarise(
    rushes = sum(rush),
    passes = sum(pass),
    total_plays = rushes + passes,
    rush_rate = rushes / total_plays,
    pass_rate = passes / total_plays
  )

team_ED_RP <- team_ED_RP %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))
team_ED_RP <- team_ED_RP %>% left_join(team_wins_18, by = c("posteam" = "Team"))

team_ED_RP %>%
  ggplot(aes(x = `2018 Wins`, y = rush_rate)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Wins in 2018",
       y = "Early Down rush rate",
       caption = "Data from nflscrapR",
       title = "Early Down Rush Rate by Wins",
       subtitle = "2018") +
  expand_limits(x=c(2,15)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))
ggsave('EDRushRatevsWins.png', dpi=1000)

# Filter to rush plays and add direction counts
rushes <- pbp_all_rp %>%
  filter(rush == 1) %>%
  mutate(
    left_end = if_else(str_detect(desc, "(left end)"), 1, 0),
    left_tackle = if_else(str_detect(desc, "(left tackle)"), 1, 0),
    left_guard = if_else(str_detect(desc, "(left guard)"), 1, 0),
    middle = if_else(str_detect(desc, "(up the middle)"), 1, 0),
    right_guard = if_else(str_detect(desc, "(right guard)"), 1, 0),
    right_tackle = if_else(str_detect(desc, "(right tackle)"), 1, 0),
    right_end = if_else(str_detect(desc, "(right end)"), 1, 0),
    left = if_else((left_end == 1 | left_tackle == 1 | left_guard == 1), 1, 0),
    right = if_else((right_end == 1 | right_tackle == 1 | right_guard == 1), 1, 0)
  )

# 2018 all downs team directional rush success
rush_success_18_all <- rushes %>%
  filter(season == 2018) %>%
  group_by(posteam) %>%
  summarise(
    n_rush = sum(rush),
    rush_epa = sum(epa),
    epa_per_rush = sum(epa) / n_rush,
    rush_success = sum(success) / n_rush,
    left_end_rushes = sum(left_end),
    left_end_epa = sum(epa * left_end) / left_end_rushes,
    left_end_success = sum(success * left_end) / left_end_rushes,
    left_tackle_rushes = sum(left_tackle),
    left_tackle_epa = sum(epa * left_tackle) / left_tackle_rushes,
    left_tackle_success = sum(success * left_tackle) / left_tackle_rushes,
    left_guard_rushes = sum(left_guard),
    left_guard_epa = sum(epa * left_guard) / left_guard_rushes,
    left_guard_success = sum(success * left_guard) / left_guard_rushes,
    middle_rushes = sum(middle),
    middle_epa = sum(epa * middle) / middle_rushes,
    middle_success = sum(success * middle) / middle_rushes,
    right_guard_rushes = sum(right_guard),
    right_guard_epa = sum(epa * right_guard) / right_guard_rushes,
    right_guard_success = sum(success * right_guard) / right_guard_rushes,
    right_tackle_rushes = sum(right_tackle),
    right_tackle_epa = sum(epa * right_tackle) / right_tackle_rushes,
    right_tackle_success = sum(success * right_tackle) / right_tackle_rushes,
    right_end_rushes = sum(right_end),
    right_end_epa = sum(epa * right_end) / right_end_rushes,
    right_end_success = sum(success * right_end) / right_end_rushes,
    left_rushes = sum(left),
    left_epa = sum(epa * left) / left_rushes,
    left_success = sum(success * left) / left_rushes,
    right_rushes = sum(right),
    right_epa = sum(epa * right) / right_rushes,
    right_success = sum(success * right) / right_rushes
  )

rush_success_18_all

# 2018 early downs team directional rush success
rush_success_18_early <- rushes %>%
  filter(season == 2018 & down <= 2 & wp >= 0.2 & wp <= 0.8) %>%
  group_by(posteam) %>%
  summarise(
    n_rush = sum(rush),
    rush_epa = sum(epa),
    epa_per_rush = sum(epa) / n_rush,
    rush_success = sum(success) / n_rush,
    left_end_rushes = sum(left_end),
    left_end_epa = sum(epa * left_end) / left_end_rushes,
    left_end_success = sum(success * left_end) / left_end_rushes,
    left_tackle_rushes = sum(left_tackle),
    left_tackle_epa = sum(epa * left_tackle) / left_tackle_rushes,
    left_tackle_success = sum(success * left_tackle) / left_tackle_rushes,
    left_guard_rushes = sum(left_guard),
    left_guard_epa = sum(epa * left_guard) / left_guard_rushes,
    left_guard_success = sum(success * left_guard) / left_guard_rushes,
    middle_rushes = sum(middle),
    middle_epa = sum(epa * middle) / middle_rushes,
    middle_success = sum(success * middle) / middle_rushes,
    right_guard_rushes = sum(right_guard),
    right_guard_epa = sum(epa * right_guard) / right_guard_rushes,
    right_guard_success = sum(success * right_guard) / right_guard_rushes,
    right_tackle_rushes = sum(right_tackle),
    right_tackle_epa = sum(epa * right_tackle) / right_tackle_rushes,
    right_tackle_success = sum(success * right_tackle) / right_tackle_rushes,
    right_end_rushes = sum(right_end),
    right_end_epa = sum(epa * right_end) / right_end_rushes,
    right_end_success = sum(success * right_end) / right_end_rushes,
    left_rushes = sum(left),
    left_epa = sum(epa * left) / left_rushes,
    left_success = sum(success * left) / left_rushes,
    right_rushes = sum(right),
    right_epa = sum(epa * right) / right_rushes,
    right_success = sum(success * right) / right_rushes
  )

# Combine team rush attempts, wins, logos and graph
all_rush_attempt_chart <- rush_success_18_all %>%
  select(posteam, n_rush)
all_rush_attempt_chart <- all_rush_attempt_chart %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))
all_rush_attempt_chart <- all_rush_attempt_chart %>% left_join(team_wins_18, by = c("posteam" = "Team"))

all_rush_attempt_chart %>%
  ggplot(aes(x = `2018 Wins`, y = n_rush)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Wins in 2018",
       y = "Total rush attempts",
       caption = "Data from nflscrapR",
       title = "Rush Attempts by Wins",
       subtitle = "2018") +
  expand_limits(x=c(2,15)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))
ggsave('RushesvsWins.png', dpi=1000)

# Do the same except for early down rushes
early_rush_attempt_chart <- rush_success_18_early %>%
  select(posteam, n_rush)
early_rush_attempt_chart <- early_rush_attempt_chart %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))
early_rush_attempt_chart <- early_rush_attempt_chart %>% left_join(team_wins_18, by = c("posteam" = "Team"))

early_rush_attempt_chart %>%
  ggplot(aes(x = `2018 Wins`, y = n_rush)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Wins in 2018",
       y = "Early down rush attempts",
       caption = "Data from nflscrapR",
       title = "Early Down Rush Attempts by Wins",
       subtitle = "2018") +
  expand_limits(x=c(2,15)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))
ggsave('EDRushesvsWins.png', dpi=1000)

# Compare difference in rush %
rush_attempt_diff_chart <- all_rush_attempt_chart
rush_attempt_diff_chart$ED_rush <- early_rush_attempt_chart$n_rush
rush_attempt_diff_chart$rush_diff <- rush_attempt_diff_chart$n_rush - rush_attempt_diff_chart$ED_rush
rush_attempt_diff_chart$percent_ED_rush <- rush_attempt_diff_chart$ED_rush / rush_attempt_diff_chart$n_rush * 100

rush_attempt_diff_chart %>%
  ggplot(aes(x = `2018 Wins`, y = percent_ED_rush)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Wins in 2018",
       y = "Percent Early Down rushes",
       caption = "Data from nflscrapR",
       title = "Percentage Early Down Rush Attempts by Wins",
       subtitle = "2018") +
  expand_limits(x=c(2,15)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))
ggsave('EDRushPercentvsWins.png', dpi=1000)


# How many rushes went in each direction
rush_dir_18 <- cbind.data.frame(direction = c("total", "left end", "left tackle", "left guard", "middle", "right guard", "right tackle", "right end"),
                             n_rush = rep(NA, 8),
                             success_rate = rep(NA, 8),
                             epa_rush = rep(NA, 8))

rush_dir_18[1, 2] <- sum(rush_success_18_early[, 2])
rush_dir_18[1, 3] <- sum(rush_success_18_early[, 2] * rush_success_18_early[, 5])
rush_dir_18[1, 4] <- sum(rush_success_18_early[, 3])
for (i in 2:8) {
  rush_dir_18[i, 2] <- sum(rush_success_18_early[, i*3])
  rush_dir_18[i, 3] <- sum(rush_success_18_early[, i*3] * rush_success_18_early[, i*3 + 2])
  rush_dir_18[i, 4] <- sum(rush_success_18_early[, i*3] * rush_success_18_early[, i*3 + 1])
}

# Verify loop worked
rushes %>%
  filter(left_end == 1 & season == 2018 & down <= 2 & wp >= 0.2 & wp <= 0.8) %>%
  summarise(
    successes = sum(success),
    total_epa = sum(epa)
  )

# Make success and EPA totals per rush
rush_dir_18$success_rate <- rush_dir_18$success_rate / rush_dir_18$n_rush
rush_dir_18$epa_rush <- rush_dir_18$epa_rush / rush_dir_18$n_rush

# Plotz
rush_dir_18[c(2:8),] %>%
  ggplot(aes(x = direction, y = n_rush)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  scale_x_discrete(limits=c("left end", "left tackle", "left guard", "middle", "right guard", "right tackle", "right end")) +
  labs(x = "Direction of Rush",
       y = "Number Early Down rushes",
       caption = "Data from nflscrapR",
       title = "Number of Early Down Rush Attempts by Direction",
       subtitle = "2018") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))
ggsave('RushesbyDirection18.png', dpi=1000)

rush_dir_18[c(2:8),] %>%
  ggplot(aes(x = direction, y = epa_rush)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  scale_x_discrete(limits=c("left end", "left tackle", "left guard", "middle", "right guard", "right tackle", "right end")) +
  labs(x = "Direction of Rush",
       y = "EPA per rush",
       caption = "Data from nflscrapR",
       title = "EPA per Early Down Rush Attempts by Direction",
       subtitle = "2018") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))
ggsave('EPAperRushbyDirection18.png', dpi=1000)

rush_dir_18[c(2:8),] %>%
  ggplot(aes(x = direction, y = success_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  scale_x_discrete(limits=c("left end", "left tackle", "left guard", "middle", "right guard", "right tackle", "right end")) +
  expand_limits(y=c(0,1)) +
  labs(x = "Direction of Rush",
       y = "Success Rate",
       caption = "Data from nflscrapR",
       title = "Success Rate on Early Down Rush Attempts by Direction",
       subtitle = "2018") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))
ggsave('SuccRatebyDirection18.png', dpi=1000)

rush_success_18_early$rush_sr_match <- rep(0, 32)
rush_success_18_early$rush_sr_match_LR <- rep(0, 32)

# Investigate how often teams ran in their most successful direction
for (i in 1:32) {
  high_rush <- 0
  high_rush_dir <- NA
  high_sr <- 0
  high_sr_dir <- NA
  for (j in 1:7) {
    if (rush_success_18_early[i, j*3 + 3] > high_rush) {
      high_rush <- rush_success_18_early[i, j*3 + 3]
      high_rush_dir <- j
    }
    if (rush_success_18_early[i, j*3 + 5] > high_sr) {
      high_sr <- rush_success_18_early[i, j*3 + 5]
      high_sr_dir <- j
    }
  }
  rush_success_18_early$high_rush_dir[i] <- high_rush_dir
  rush_success_18_early$high_rush_count[i] <- high_rush
  rush_success_18_early$high_sr_dir[i] <- high_sr_dir
  rush_success_18_early$high_sr[i] <- high_sr
  if (rush_success_18_early$high_rush_dir[i] == rush_success_18_early$high_sr_dir[i]){
    rush_success_18_early$rush_sr_match[i] <- 1
  }
}

rush_success_18_early %>%
  filter(rush_sr_match == 1) %>%
  select(posteam, high_sr_dir)

# Generalize to Left vs Middle vs Right
for (i in 1:32) {
  high_rush <- rush_success_18_early$left_rushes[i]
  high_rush_dir <- 1
  high_sr <- rush_success_18_early$left_success[i]
  high_sr_dir <- 1
  if (rush_success_18_early$middle_rushes[i] > high_rush) {
    if(rush_success_18_early$right_rushes[i] > rush_success_18_early$middle_rushes[i]) {
      high_rush <- rush_success_18_early$right_rushes[i]
      high_rush_dir <- 3
    } else {
      high_rush <- rush_success_18_early$middle_rushes[i]
      high_rush_dir <- 2
    }
  } else if (rush_success_18_early$right_rushes[i] > high_rush) {
      high_rush <- rush_success_18_early$right_rushes[i]
      high_rush_dir <- 3
  }
  if (rush_success_18_early$middle_success[i] > high_sr) {
    if(rush_success_18_early$right_success[i] > rush_success_18_early$middle_success[i]) {
      high_sr <- rush_success_18_early$right_success[i]
      high_sr_dir <- 3
    } else {
      high_sr <- rush_success_18_early$middle_success[i]
      high_sr_dir <- 2
    }
  } else if (rush_success_18_early$right_success[i] > high_sr) {
    high_sr <- rush_success_18_early$right_success[i]
    high_sr_dir <- 3
  }
  rush_success_18_early$high_rush_dir_LR[i] <- high_rush_dir
  rush_success_18_early$high_rush_count_LR[i] <- high_rush
  rush_success_18_early$high_sr_dir_LR[i] <- high_sr_dir
  rush_success_18_early$high_sr_LR[i] <- high_sr
  if (rush_success_18_early$high_rush_dir_LR[i] == rush_success_18_early$high_sr_dir_LR[i]){
    rush_success_18_early$rush_sr_match_LR[i] <- 1
  }
}

rush_success_18_early %>%
  filter(rush_sr_match_LR == 1) %>%
  select(posteam, high_sr_dir_LR)

testdf <- cbind.data.frame(ID = 1:10,
                           pass = rep(NA, 10))
testdf2 <- cbind.data.frame(ID = 2:5,
                            rush = c(9,8,7,6))
testdf <- testdf %>% left_join(testdf2, by = c("ID" = "ID"))
testdf
