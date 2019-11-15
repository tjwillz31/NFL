library(devtools)
library(nflscrapR)
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(stringi)
library(ggplot2)
library(ggrepel)

setwd("~/Fun/NFL_Analysis/Kickoff Analysis")

# Pull all plays from 2018 and through 11/10/19
pbp2018 <- scrape_season_play_by_play(2018)
pbp2019 <- scrape_season_play_by_play(2019)
pbp2018$season <- 2018
pbp2019$season <- 2019
pbp <- rbind(pbp2018, pbp2019)

# Filter for kickoffs
kickoffs <- pbp %>% 
  filter(play_type == 'kickoff') %>% 
  select(season, play_id, game_id, home_team, away_team, posteam, defteam, yardline_100, game_seconds_remaining, desc,
         play_type, yards_gained, epa, wp, wpa, touchback, kickoff_in_endzone, kickoff_out_of_bounds, kickoff_downed,
         kickoff_fair_catch, kickoff_attempt, fumble_lost, touchdown, kickoff_returner_player_name, kicker_player_name,
         return_yards)

# Clean data
kickoffs[is.na(kickoffs$wp),14] <- 0.5
kickoffs[is.na(kickoffs$wpa),15] <- 0

# summarizing and plotz
# starting with analysis of teams' kicking
kicking <- kickoffs %>% 
  filter(onside == 0 & !is.na(epa)) %>% 
  group_by(defteam, season) %>% 
  summarise(
    kicks = sum(kickoff_attempt),
    ez_rate = sum(endzone_kick) / kicks,
    total_tb_rate = sum(touchback) / kicks,
    ez_tb_rate = sum(touchback) / sum(endzone_kick),
    epa_per_play = sum(epa) / kicks
  )

# plot total touchback rate by rate of kicks reaching the endzone
kicking %>%
  ggplot(aes(x = ez_rate, y = total_tb_rate)) +
  geom_hline(yintercept = mean(kicking$total_tb_rate), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(kicking$ez_rate), color = "red", linetype = "dashed") +
  geom_point(color = "black", cex=kicking$kicks/10, alpha=1/4) +
  geom_text_repel(aes(label=paste0(kicking$defteam, "-", kicking$season)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "% of Kickoffs into Endzone",
       y = "% of Touchbacks on Kickoffs",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Touchback Rate by Endzone Kick Rate",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('TouchbackvsEndzone.png', dpi=1000)

# plot touchback rate for kicks into endzone by rate of kicks reaching the endzone
kicking %>%
  ggplot(aes(x = ez_rate, y = ez_tb_rate)) +
  geom_hline(yintercept = mean(kicking$ez_tb_rate), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(kicking$ez_rate), color = "red", linetype = "dashed") +
  geom_point(color = "black", cex=kicking$kicks/10, alpha=1/4) +
  geom_text_repel(aes(label=paste0(kicking$defteam, "-", kicking$season)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "% of Kickoffs into Endzone",
       y = "% of Touchbacks on Endzone Kickoffs",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Touchback Rate by Endzone Kick Rate",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('TBonEZvsEZ.png', dpi=1000)

# plot epa per play by endzone rate
kicking %>%
  ggplot(aes(x = ez_rate, y = epa_per_play)) +
  geom_hline(yintercept = mean(kicking$epa_per_play), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(kicking$ez_rate), color = "red", linetype = "dashed") +
  geom_point(color = "black", cex=kicking$kicks/10, alpha=1/4) +
  geom_text_repel(aes(label=paste0(kicking$defteam, "-", kicking$season)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "% of Kickoffs reaching Endzone",
       y = "EPA per Kickoff",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "EPA per play by Endzone Kick Rate",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('EPAvsEZ.png', dpi=1000)

# plot epa per play by touchback rate
kicking %>%
  ggplot(aes(x = total_tb_rate, y = epa_per_play)) +
  geom_hline(yintercept = mean(kicking$epa_per_play), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(kicking$total_tb_rate), color = "red", linetype = "dashed") +
  geom_point(color = "black", cex=kicking$kicks/10, alpha=1/4) +
  geom_text_repel(aes(label=paste0(kicking$defteam, "-", kicking$season)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "% of Touchbacks on Kickoffs",
       y = "EPA per Kickoff",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "EPA per play by Touchback Kickoff Rate",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('EPAvsTB.png', dpi=1000)

# plot epa per play by touchback rate on endzone kicks
kicking %>%
  ggplot(aes(x = ez_tb_rate, y = epa_per_play)) +
  geom_hline(yintercept = mean(kicking$epa_per_play), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(kicking$ez_tb_rate), color = "red", linetype = "dashed") +
  geom_point(color = "black", cex=kicking$kicks/10, alpha=1/4) +
  geom_text_repel(aes(label=paste0(kicking$defteam, "-", kicking$season)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "% of Touchbacks on Kickoffs reaching Endzone",
       y = "EPA per Kickoff",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "EPA per play by Touchback Rate on Endzone Kicks",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('EPAvsTBonEZ.png', dpi=1000)

# Are Endzone Rate, Total Touchback Rate, and Endzone Touchback Rate stable?
kicking18 <- kicking %>% 
  filter(season == 2018)
kicking19 <- kicking %>% 
  filter(season == 2019)
rsq_kick_ez <- cor(kicking18$ez_rate, kicking19$ez_rate)^2
rsq_kick_total_tb <- cor(kicking18$total_tb_rate, kicking19$total_tb_rate)^2
rsq_kick_ez_tb <- cor(kicking18$ez_tb_rate, kicking19$ez_tb_rate)^2
rsq_kick_epa <- cor(kicking18$epa_per_play, kicking19$epa_per_play)^2

kicking_by_year <- cbind.data.frame(team = kicking18$defteam, kicks18 = kicking18$kicks, ez_rate18 = kicking18$ez_rate,
                                    total_tb_rate18 = kicking18$total_tb_rate, ez_tb_rate18 = kicking18$ez_tb_rate, 
                                    epa_per_play18 = kicking18$epa_per_play, kicks19 = kicking19$kicks, 
                                    ez_rate19 = kicking19$ez_rate, total_tb_rate19 = kicking19$total_tb_rate, 
                                    ez_tb_rate19 = kicking19$ez_tb_rate, epa_per_play19 = kicking19$epa_per_play)

# plot 18 vs 19 rate variables
kicking_by_year %>%
  ggplot(aes(x = ez_rate18, y = ez_rate19)) +
  geom_hline(yintercept = mean(kicking_by_year$ez_rate19), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(kicking_by_year$ez_rate18), color = "red", linetype = "dashed") +
  geom_point(color = "black", alpha=1/4) +
  geom_text_repel(aes(label=paste0(team)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "2018 Rate of Kickoffs Reaching Endzone",
       y = "2019 Rate of Kickoffs Reaching Endzone",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Endzone Rate on Kickoffs by Year",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('EZYoY.png', dpi=1000)

kicking_by_year %>%
  ggplot(aes(x = total_tb_rate18, y = total_tb_rate19)) +
  geom_hline(yintercept = mean(kicking_by_year$total_tb_rate19), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(kicking_by_year$total_tb_rate18), color = "red", linetype = "dashed") +
  geom_point(color = "black", alpha=1/4) +
  geom_text_repel(aes(label=paste0(team)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "2018 Rate of Touchbacks",
       y = "2019 Rate of Touchbacks",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Touchback Rate on Kickoffs by Year",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('TBYoY.png', dpi=1000)

kicking_by_year %>%
  ggplot(aes(x = ez_tb_rate18, y = ez_tb_rate19)) +
  geom_hline(yintercept = mean(kicking_by_year$ez_tb_rate19), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(kicking_by_year$ez_tb_rate18), color = "red", linetype = "dashed") +
  geom_point(color = "black", alpha=1/4) +
  geom_text_repel(aes(label=paste0(team)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "2018 Touchback Rate on Kickoffs Reaching Endzone",
       y = "2019 Touchback Rate on Kickoffs Reaching Endzone",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Touchback Rate on Endzone Kickoffs by Year",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('EZTBYoY.png', dpi=1000)

kicking_by_year %>%
  ggplot(aes(x = epa_per_play18, y = epa_per_play19)) +
  geom_hline(yintercept = mean(kicking_by_year$epa_per_play19), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(kicking_by_year$epa_per_play18), color = "red", linetype = "dashed") +
  geom_point(color = "black", alpha=1/4) +
  geom_text_repel(aes(label=paste0(team)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "2018 EPA per Kickoff",
       y = "2019 EPA per Kickoff",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "EPA per Kickoffs by Year",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('EPAYoY.png', dpi=1000)


# Now look at teams' returns
returning <- kickoffs %>% 
  filter(onside == 0 & !is.na(epa)) %>% 
  group_by(posteam, season) %>% 
  summarise(
    kicks = sum(kickoff_attempt),
    ez_rate = sum(endzone_kick) / kicks,
    total_tb_rate = sum(touchback) / kicks,
    ez_tb_rate = sum(touchback) / sum(endzone_kick),
    epa_per_play = sum(epa) / kicks
  )

# plot total touchback rate by rate of kicks reaching the endzone
returning %>%
  ggplot(aes(x = ez_rate, y = 1 - total_tb_rate)) +
  geom_hline(yintercept = 1 - mean(returning$total_tb_rate), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(returning$ez_rate), color = "red", linetype = "dashed") +
  geom_point(color = "black", cex=returning$kicks/10, alpha=1/4) +
  geom_text_repel(aes(label=paste0(returning$posteam, "-", returning$season)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "% of Kickoffs into Endzone",
       y = "% of Kickoffs with Returns",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Return Rate by Endzone Kick Rate",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('RetRatevsEndzone.png', dpi=1000)

# plot touchback rate for kicks into endzone by rate of kicks reaching the endzone
returning %>%
  ggplot(aes(x = ez_rate, y = 1 - ez_tb_rate)) +
  geom_hline(yintercept = 1 - mean(returning$ez_tb_rate), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(returning$ez_rate), color = "red", linetype = "dashed") +
  geom_point(color = "black", cex=returning$kicks/10, alpha=1/4) +
  geom_text_repel(aes(label=paste0(returning$posteam, "-", returning$season)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "% of Kickoffs into Endzone",
       y = "% of Returns on Endzone Kickoffs",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Return Rate on Endzone Kicks by Endzone Kick Rate",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('RetRateEZvsEZ.png', dpi=1000)

# plot epa per play by endzone return rate
returning %>%
  ggplot(aes(x = 1 - ez_tb_rate, y = epa_per_play)) +
  geom_hline(yintercept = mean(returning$epa_per_play), color = "red", linetype = "dashed") +
  geom_vline(xintercept = 1 - mean(returning$ez_rate), color = "red", linetype = "dashed") +
  geom_point(color = "black", cex=returning$kicks/10, alpha=1/4) +
  geom_text_repel(aes(label=paste0(returning$posteam, "-", returning$season)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "Return Rate on Endzone Kickoffs",
       y = "EPA per Kickoff",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "EPA per play by Endzone Return Rate",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('RetEPAvsEZReturn.png', dpi=1000)

# plot epa per play by overall return rate
returning %>%
  ggplot(aes(x = 1 - total_tb_rate, y = epa_per_play)) +
  geom_hline(yintercept = mean(returning$epa_per_play), color = "red", linetype = "dashed") +
  geom_vline(xintercept = 1 - mean(returning$total_tb_rate), color = "red", linetype = "dashed") +
  geom_point(color = "black", cex=returning$kicks/10, alpha=1/4) +
  geom_text_repel(aes(label=paste0(returning$posteam, "-", returning$season)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "Return Rate on All Kickoffs",
       y = "EPA per Kickoff",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "EPA per play by Return Rate",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('RetEPAvsRetRate.png', dpi=1000)

# Are Endzone Rate, Total Touchback Rate, and Endzone Touchback Rate stable?
returning18 <- returning %>% 
  filter(season == 2018)
returning19 <- returning %>% 
  filter(season == 2019)
rsq_ret_ez <- cor(returning18$ez_rate, returning19$ez_rate)^2
rsq_ret_total_tb <- cor(returning18$total_tb_rate, returning19$total_tb_rate)^2
rsq_ret_ez_tb <- cor(returning18$ez_tb_rate, returning19$ez_tb_rate)^2
rsq_ret_epa <- cor(returning18$epa_per_play, returning19$epa_per_play)^2

returning_by_year <- cbind.data.frame(team = returning18$posteam, ret18 = returning18$kicks, ez_rate18 = returning18$ez_rate,
                                    total_tb_rate18 = returning18$total_tb_rate, ez_tb_rate18 = returning18$ez_tb_rate, 
                                    epa_per_play18 = returning18$epa_per_play, kicks19 = returning19$kicks, 
                                    ez_rate19 = returning19$ez_rate, total_tb_rate19 = returning19$total_tb_rate, 
                                    ez_tb_rate19 = returning19$ez_tb_rate, epa_per_play19 = returning19$epa_per_play)

# plot 18 vs 19 rate variables
returning_by_year %>%
  ggplot(aes(x = ez_rate18, y = ez_rate19)) +
  geom_hline(yintercept = mean(returning_by_year$ez_rate19), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(returning_by_year$ez_rate18), color = "red", linetype = "dashed") +
  geom_point(color = "black", alpha=1/4) +
  geom_text_repel(aes(label=paste0(team)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "2018 Rate of Kickoffs Reaching Endzone for Return Team",
       y = "2019 Rate of Kickoffs Reaching Endzone for Return Team",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Endzone Rate on Kickoffs for Return Team by Year",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('RetEZYoY.png', dpi=1000)

returning_by_year %>%
  ggplot(aes(x = 1 - total_tb_rate18, y = 1- total_tb_rate19)) +
  geom_hline(yintercept = 1 - mean(returning_by_year$total_tb_rate19), color = "red", linetype = "dashed") +
  geom_vline(xintercept = 1 - mean(returning_by_year$total_tb_rate18), color = "red", linetype = "dashed") +
  geom_point(color = "black", alpha=1/4) +
  geom_text_repel(aes(label=paste0(team)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "2018 Rate of Returns",
       y = "2019 Rate of Returns",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Return Rate by Year",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('RetRateYoY.png', dpi=1000)

returning_by_year %>%
  ggplot(aes(x = 1 - ez_tb_rate18, y = 1 - ez_tb_rate19)) +
  geom_hline(yintercept = 1 - mean(returning_by_year$ez_tb_rate19), color = "red", linetype = "dashed") +
  geom_vline(xintercept = 1 - mean(returning_by_year$ez_tb_rate18), color = "red", linetype = "dashed") +
  geom_point(color = "black", alpha=1/4) +
  geom_text_repel(aes(label=paste0(team)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "2018 Return Rate on Kickoffs Reaching Endzone",
       y = "2019 Return Rate on Kickoffs Reaching Endzone",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Return Rate on Endzone Kickoffs by Year",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('RetRateEZYoY.png', dpi=1000)

returning_by_year %>%
  ggplot(aes(x = epa_per_play18, y = epa_per_play19)) +
  geom_hline(yintercept = mean(returning_by_year$epa_per_play19), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(returning_by_year$epa_per_play18), color = "red", linetype = "dashed") +
  geom_point(color = "black", alpha=1/4) +
  geom_text_repel(aes(label=paste0(team)),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "2018 Return EPA per Kickoff",
       y = "2019 Return EPA per Kickoff",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Return EPA per Kickoffs by Year",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('RetEPAYoY.png', dpi=1000)


# Does Win Probability influence return rate on endzone kicks
hist(kickoffs$wp)
kickoffs$wp.bin <- round(kickoffs$wp / 0.05) * 0.05

wp.groups <- kickoffs %>% 
  filter(onside == 0 & !is.na(epa)) %>% 
  group_by(wp.bin) %>% 
  summarise(
    kicks = sum(kickoff_attempt),
    ret_rate = sum(taken_out_ez) / kicks,
    epa_per_play = sum(epa) / kicks
  )

wp.groups %>%
  ggplot(aes(x = wp.bin, y = ret_rate)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(wp.groups$ret_rate), color = "red", linetype = "dashed") +
  labs(x = "Win Probability",
       y = "Return Rate on Kicking Reaching Endzone",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "Return Rate by Win Probability",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('RetRatebyWP.png', dpi=1000)

wp.groups %>%
  ggplot(aes(x = wp.bin, y = epa_per_play)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(wp.groups$epa_per_play), color = "red", linetype = "dashed") +
  labs(x = "Win Probability",
       y = "EPA per Play",
       caption = "Data from nflscrapR, @tjwillz31",
       title = "EPA per Play by Win Probability",
       subtitle = "2018-Week 10, 2019") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))
ggsave('RetEPAbyWP.png', dpi=1000)
