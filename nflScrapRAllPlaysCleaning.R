#install.packages("na.tools")
#install.packages("ggimage")

library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)

# Read in 2018 play by play data from CSV on Ron's github
pbp18 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))

# Filter play by play to only run plays, pass plays, or penalties)
pbp18_rp <- pbp18 %>% filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

# Create new variables to identify pass plays, run plays, and successful plays
pbp18_rp <- pbp18_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

# Filter play by play to only run and pass plays (incuding live ball penalties)
pbp18_rp <- pbp18_rp %>% filter(pass==1 | rush==1)

# Confirm that CJA is a generational talent
pbp18_rp %>%
  filter(posteam == "LA", rush == 1, down<=4) %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>40)

# Make fun of the 2018 Seahawks
schotty <- pbp18_rp %>%
  filter(wp>.20 & wp<.80 & down<=2 & qtr<=2 & half_seconds_remaining>120) %>%
  group_by(posteam) %>%
  summarize(mean_pass=mean(pass), plays=n()) %>%
  arrange(mean_pass)
schotty
ggplot(schotty, aes(x=reorder(posteam,-mean_pass), y=mean_pass)) +
  geom_text(aes(label=posteam))

# Success rate vs EPA chart
chart_data <- pbp18_rp %>%
  filter(pass==1) %>%
  group_by(posteam) %>%
  summarise(
    num_db = n(),
    epa_per_db = sum(epa) / num_db,
    success_rate = sum(epa > 0) / num_db
  )

nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
chart <- chart_data %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))

chart %>%
  ggplot(aes(x = success_rate, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Success rate",
       y = "EPA per play",
       caption = "Data from nflscrapR",
       title = "Dropback success rate & EPA/play",
       subtitle = "2018") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))
ggsave('SuccessvsEPA.png', dpi=1000)

# Who is better at rushing or passing
chart_data <- pbp18_rp %>%
  group_by(posteam) %>%
  filter(down<=2) %>%
  summarise(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    epa_per_db = sum(epa * pass) / n_dropbacks,
    epa_per_rush = sum(epa * rush) / n_rush,
    success_per_db = sum(success * pass) / n_dropbacks,
    success_per_rush = sum(success * rush) / n_rush
  )

chart <- chart_data %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))

# EPA
chart %>%
  ggplot(aes(x = epa_per_rush, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Rush EPA/play",
       y = "Pass EPA/play",
       caption = "Data from nflscrapR",
       title = "Early-down rush and pass EPA/play",
       subtitle = "2018") +
  theme_bw() +
  geom_abline(slope=1, intercept=0, alpha=.2) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('TeamRushPassEPA.png', dpi=1000)

#Success Rate
chart %>%
  ggplot(aes(x = success_per_rush, y = success_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Rush success rate",
       y = "Pass success rate",
       caption = "Data from nflscrapR",
       title = "Early-down rush and pass success rate",
       subtitle = "2018") +
  theme_bw() +
  geom_abline(slope=1, intercept=0, alpha=.2) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('TeamPassRushSuccess.png', dpi=1000)

# Identify passer, rusher, and receriver for each play including penalties
pbp_players <- pbp18_rp %>% 
  mutate(
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, 
                                              "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name)
  )

# Find all QBs
qbs <- pbp_players %>% 
  mutate(
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    rusher = rusher_player_name,
    receiver = receiver_player_name,
    play = 1
  ) %>%
  group_by(name, posteam) %>%
  summarize (
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    n_plays = sum(play),
    epa_per_play = sum(epa)/n_plays,
    success_per_play =sum(success)/n_plays
  ) %>%
  filter(n_dropbacks>=100)

# install.packages("ggrepel")
library(ggrepel)

# Look at QB success rate vs EPA
qbs %>%
  ggplot(aes(x = success_per_play, y = epa_per_play)) +
  geom_hline(yintercept = mean(qbs$epa_per_play), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(qbs$success_per_play), color = "red", linetype = "dashed") +
  geom_point(color = ifelse(qbs$posteam == "SF", "red", "black"), cex=qbs$n_plays/60, alpha=1/4) +
  geom_text_repel(aes(label=name),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "Success rate",
       y = "EPA per play",
       caption = "Data from nflscrapR",
       title = "QB success rate and EPA/play",
       subtitle = "2018, min 100 pass attempts, includes all QB's rush and pass plays") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))

ggsave('QBEPAvSuccess.png', dpi=1000)

# Pull in all seasons
first <- 2009 #first season to grab. min available=2009
last <- 2018 # most recent season

datalist = list()
for (yr in first:last) {
  pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
  pbp <- pbp %>% inner_join(games %>% distinct(game_id, week, season)) %>% select(-c(fumble_recovery_2_yards, blocked_player_id, fumble_recovery_2_player_id))
  datalist[[yr]] <- pbp # add it to your list
}

pbp_all <- dplyr::bind_rows(datalist)

# Clean team names
pbp_all <- pbp_all %>% 
  mutate_at(vars(home_team, away_team, posteam, defteam), funs(case_when(
    . %in% "JAX" ~ "JAC",
    . %in% "STL" ~ "LA",
    . %in% "SD" ~ "LAC",
    TRUE ~ .
  )))

# Run Pass cleaning
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

saveRDS(pbp_all, file="nflScrapRAllPlays.rds")
pbp_all <- readRDS("nflScrapRAllPlays.rds")