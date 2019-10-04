library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(reshape2)

# Load data
pbp_all <- readRDS("nflScrapRAllPlays.rds")

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

# Filter to run and pass plays from the last 3 seasons (2016-2018) by down type
pbp_all_rp_l3 <- pbp_all_rp %>%
  filter(season == 2018 | season == 2017 | season == 2016)

pbp_early_rp_l3 <- pbp_all_rp_l3 %>%
  filter(down < 3)

pbp_late_rp_l3 <- pbp_all_rp_l3 %>%
  filter(down == 3 | down == 4)
  
# Plot pass rates by yards to go for different downs
late_ytg <- pbp_late_rp_l3 %>%
  group_by(ydstogo) %>%
  summarise(
    plays = n(),
    passrate = sum(pass) / plays)

all_ytg <- pbp_all_rp_l3 %>%
  group_by(ydstogo) %>%
  summarise(
    plays = n(),
    passrate = sum(pass) / plays)

early_ytg <- pbp_early_rp_l3 %>%
  group_by(ydstogo) %>%
  summarise(
    plays = n(),
    passrate = sum(pass) / plays)

pr_by_down_ytg_sub <- cbind.data.frame(ytg = all_ytg$ydstogo[2:16], all_down = all_ytg$passrate[2:16], 
                              early_down = early_ytg$passrate[1:15], late_down = late_ytg$passrate[1:15])
pr_by_down_ytg <- melt(pr_by_down_ytg_sub, id = c("ytg"))

ggplot(pr_by_down_ytg) +
  geom_line(aes(x = ytg, y = value, color = variable), size = 1.25) +
  scale_colour_manual(values=c("blue","green","red")) +
  labs(x = "Yards to Go",
       y = "Pass Rate",
       caption = "Data from nflscrapR",
       title = "Pass Rate by Yards to Go and Down Type",
       subtitle = "2016-2018") +
  expand_limits(y=c(0,1)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))
ggsave('PassRatebyDownYTG.png', dpi=1000)

# Filter on all downs for losing big, neutrel, and winning big
pbp_all_lose_rp_l3 <- pbp_all_rp_l3 %>%
  filter(wp < 0.2)

pbp_all_neut_rp_l3 <- pbp_all_rp_l3 %>%
  filter(wp >= 0.2 & wp < 0.8)

pbp_all_win_rp_l3 <- pbp_all_rp_l3 %>%
  filter(wp >= 0.8)

# Plot pass rates by yards to go for different game scripts
losing_ytg <- pbp_all_lose_rp_l3 %>%
  group_by(ydstogo) %>%
  summarise(
    plays = n(),
    passrate = sum(pass) / plays)

neutral_ytg <- pbp_all_neut_rp_l3 %>%
  group_by(ydstogo) %>%
  summarise(
    plays = n(),
    passrate = sum(pass) / plays)

wining_ytg <- pbp_all_win_rp_l3 %>%
  group_by(ydstogo) %>%
  summarise(
    plays = n(),
    passrate = sum(pass) / plays)

pr_by_gs_ytg_sub <- cbind.data.frame(ytg = losing_ytg$ydstogo[2:16], losing_big = losing_ytg$passrate[2:16], 
                                     neutral = neutral_ytg$passrate[2:16], winning_big = wining_ytg$passrate[2:16])
pr_by_gs_ytg <- melt(pr_by_gs_ytg_sub, id = c("ytg"))

ggplot(pr_by_gs_ytg) +
  geom_line(aes(x = ytg, y = value, color = variable), size = 1.25) +
  scale_colour_manual(values=c("red","blue","green")) +
  labs(x = "Yards to Go",
       y = "Pass Rate",
       caption = "Data from nflscrapR",
       title = "Pass Rate by Yards to Go and Game Script",
       subtitle = "2016-2018") +
  expand_limits(y=c(0,1)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))
ggsave('PassRatebyGSYTG.png', dpi=1000)

# Predict pass plays using yards to go for different subsets of plays by down and win probability

# Create a binomial logistic regression for all downs to predict pass/run with only yards to go
alldown <- glm(pass ~ ydstogo, data = pbp_all_rp_l3, family = binomial(link = logit))
summary(alldown)
preds_ad <- alldown$fitted.values
min(preds_ad)
ad_pr <- sum(pbp_all_rp_l3$pass) / nrow(pbp_all_rp_l3)
preds_ad[preds_ad>=ad_pr] <- 1
preds_ad[preds_ad<ad_pr] <- 0
ad_table <- table(pbp_all_rp_l3$pass, preds_ad)
round(prop.table(ad_table,1),2)
ad_accuracy <- (ad_table[1,1] + ad_table[2,2]) / nrow(pbp_all_rp_l3)

# Create a binomial logistic regression for early downs to predict pass/run with only yards to go
earlydown <- glm(pass ~ ydstogo, data = pbp_early_rp_l3, family = binomial(link = logit))
ed_pr <- sum(pbp_early_rp_l3$pass) / nrow(pbp_early_rp_l3)
preds_ed <- earlydown$fitted.values
preds_ed[preds_ed>=ed_pr] <- 1
preds_ed[preds_ed<ed_pr] <- 0
ed_table <- table(pbp_early_rp_l3$pass, preds_ed)
round(prop.table(ed_table,1),2)
ed_accuracy <- (ed_table[1,1] + ed_table[2,2]) / nrow(pbp_early_rp_l3)

# Create a binomial logistic regression for late downs to predict pass/run with only yards to go
latedown <- glm(pass ~ ydstogo, data = pbp_late_rp_l3, family = binomial(link = logit))
ld_pr <- sum(pbp_late_rp_l3$pass) / nrow(pbp_late_rp_l3)
preds_ld <- latedown$fitted.values
preds_ld[preds_ld>=ld_pr] <- 1
preds_ld[preds_ld<ld_pr] <- 0
ld_table <- table(pbp_late_rp_l3$pass, preds_ld)
round(prop.table(ld_table,1),2)
ld_accuracy <- (ld_table[1,1] + ld_table[2,2]) / nrow(pbp_late_rp_l3)

# Incorporate win probability into subset of plays

# Create a binomial logistic regression for all downs and losing big to predict pass/run with only yards to go
ad_lb <- glm(pass ~ ydstogo, data = pbp_all_lose_rp_l3, family = binomial(link = logit))
ad_lb_pr <- sum(pbp_all_lose_rp_l3$pass) / nrow(pbp_all_lose_rp_l3)
preds_ad_lb <- ad_lb$fitted.values
preds_ad_lb[preds_ad_lb>=ad_lb_pr] <- 1
preds_ad_lb[preds_ad_lb<ad_lb_pr] <- 0
ad_lb_table <- table(pbp_all_lose_rp_l3$pass, preds_ad_lb)
round(prop.table(ad_lb_table,1),2)
ad_lb_accuracy <- (ad_lb_table[1,1] + ad_lb_table[2,2]) / nrow(pbp_all_lose_rp_l3)

# Create a binomial logistic regression for all downs and neutral to predict pass/run with only yards to go
ad_neut <- glm(pass ~ ydstogo, data = pbp_all_neut_rp_l3, family = binomial(link = logit))
ad_neut_pr <- sum(pbp_all_neut_rp_l3$pass) / nrow(pbp_all_neut_rp_l3)
preds_ad_neut <- ad_neut$fitted.values
preds_ad_neut[preds_ad_neut>=ad_neut_pr] <- 1
preds_ad_neut[preds_ad_neut<ad_neut_pr] <- 0
ad_neut_table <- table(pbp_all_neut_rp_l3$pass, preds_ad_neut)
round(prop.table(ad_neut_table,1),2)
ad_neut_accuracy <- (ad_neut_table[1,1] + ad_neut_table[2,2]) / nrow(pbp_all_neut_rp_l3)

# Create a binomial logistic regression for all downs and winning big to predict pass/run with only yards to go
ad_wb <- glm(pass ~ ydstogo, data = pbp_all_win_rp_l3, family = binomial(link = logit))
ad_wb_pr <- sum(pbp_all_win_rp_l3$pass) / nrow(pbp_all_win_rp_l3)
preds_ad_wb <- ad_wb$fitted.values
preds_ad_wb[preds_ad_wb>=ad_wb_pr] <- 1
preds_ad_wb[preds_ad_wb<ad_wb_pr] <- 0
ad_wb_table <- table(pbp_all_win_rp_l3$pass, preds_ad_wb)
round(prop.table(ad_wb_table,1),2)
ad_wb_accuracy <- (ad_wb_table[1,1] + ad_wb_table[2,2]) / nrow(pbp_all_win_rp_l3)

# Filter on early downs for losing big, neutrel, and winning big
pbp_early_lose_rp_l3 <- pbp_early_rp_l3 %>%
  filter(wp < 0.2)

pbp_early_neut_rp_l3 <- pbp_early_rp_l3 %>%
  filter(wp >= 0.2 & wp < 0.8)

pbp_early_win_rp_l3 <- pbp_early_rp_l3 %>%
  filter(wp >= 0.8)

# Create a binomial logistic regression for early downs and losing big to predict pass/run with only yards to go
ed_lb <- glm(pass ~ ydstogo, data = pbp_early_lose_rp_l3, family = binomial(link = logit))
ed_lb_pr <- sum(pbp_early_lose_rp_l3$pass) / nrow(pbp_early_lose_rp_l3)
preds_ed_lb <- ed_lb$fitted.values
preds_ed_lb[preds_ed_lb>=ed_lb_pr] <- 1
preds_ed_lb[preds_ed_lb<ed_lb_pr] <- 0
ed_lb_table <- table(pbp_early_lose_rp_l3$pass, preds_ed_lb)
round(prop.table(ed_lb_table,1),2)
ed_lb_accuracy <- (ed_lb_table[1,1] + ed_lb_table[2,2]) / nrow(pbp_early_lose_rp_l3)

# Create a binomial logistic regression for early downs and neutral to predict pass/run with only yards to go
ed_neut <- glm(pass ~ ydstogo, data = pbp_early_neut_rp_l3, family = binomial(link = logit))
ed_neut_pr <- sum(pbp_early_neut_rp_l3$pass) / nrow(pbp_early_neut_rp_l3)
preds_ed_neut <- ed_neut$fitted.values
preds_ed_neut[preds_ed_neut>=ed_neut_pr] <- 1
preds_ed_neut[preds_ed_neut<ed_neut_pr] <- 0
ed_neut_table <- table(pbp_early_neut_rp_l3$pass, preds_ed_neut)
round(prop.table(ed_neut_table,1),2)
ed_neut_accuracy <- (ed_neut_table[1,1] + ed_neut_table[2,2]) / nrow(pbp_early_neut_rp_l3)

# Create a binomial logistic regression for early downs and winning big to predict pass/run with only yards to go
ed_wb <- glm(pass ~ ydstogo, data = pbp_early_win_rp_l3, family = binomial(link = logit))
ed_wb_pr <- sum(pbp_early_win_rp_l3$pass) / nrow(pbp_early_win_rp_l3)
preds_ed_wb <- ed_wb$fitted.values
preds_ed_wb[preds_ed_wb>=ed_wb_pr] <- 1
preds_ed_wb[preds_ed_wb<ed_wb_pr] <- 0
ed_wb_table <- table(pbp_early_win_rp_l3$pass, preds_ed_wb)
round(prop.table(ed_wb_table,1),2)
ed_wb_accuracy <- (ed_wb_table[1,1] + ed_wb_table[2,2]) / nrow(pbp_early_win_rp_l3)

# Filter on late downs for losing big, neutrel, and winning big
pbp_late_lose_rp_l3 <- pbp_late_rp_l3 %>%
  filter(wp < 0.2)

pbp_late_neut_rp_l3 <- pbp_late_rp_l3 %>%
  filter(wp >= 0.2 & wp < 0.8)

pbp_late_win_rp_l3 <- pbp_late_rp_l3 %>%
  filter(wp >= 0.8)

# Create a binomial logistic regression for late downs and losing big to predict pass/run with only yards to go
ld_lb <- glm(pass ~ ydstogo, data = pbp_late_lose_rp_l3, family = binomial(link = logit))
ld_lb_pr <- sum(pbp_late_lose_rp_l3$pass) / nrow(pbp_late_lose_rp_l3)
preds_ld_lb <- ld_lb$fitted.values
preds_ld_lb[preds_ld_lb>=ld_lb_pr] <- 1
preds_ld_lb[preds_ld_lb<ld_lb_pr] <- 0
ld_lb_table <- table(pbp_late_lose_rp_l3$pass, preds_ld_lb)
round(prop.table(ld_lb_table,1),2)
ld_lb_accuracy <- (ld_lb_table[1,1] + ld_lb_table[2,2]) / nrow(pbp_late_lose_rp_l3)

# Create a binomial logistic regression for late downs and neutral to predict pass/run with only yards to go
ld_neut <- glm(pass ~ ydstogo, data = pbp_late_neut_rp_l3, family = binomial(link = logit))
ld_neut_pr <- sum(pbp_late_neut_rp_l3$pass) / nrow(pbp_late_neut_rp_l3)
preds_ld_neut <- ld_neut$fitted.values
preds_ld_neut[preds_ld_neut>=ld_neut_pr] <- 1
preds_ld_neut[preds_ld_neut<ld_neut_pr] <- 0
ld_neut_table <- table(pbp_late_neut_rp_l3$pass, preds_ld_neut)
round(prop.table(ld_neut_table,1),2)
ld_neut_accuracy <- (ld_neut_table[1,1] + ld_neut_table[2,2]) / nrow(pbp_late_neut_rp_l3)

# Create a binomial logistic regression for late downs and winning big to predict pass/run with only yards to go
ld_wb <- glm(pass ~ ydstogo, data = pbp_late_win_rp_l3, family = binomial(link = logit))
ld_wb_pr <- sum(pbp_late_win_rp_l3$pass) / nrow(pbp_late_win_rp_l3)
preds_ld_wb <- ld_wb$fitted.values
preds_ld_wb[preds_ld_wb>=ld_wb_pr] <- 1
preds_ld_wb[preds_ld_wb<ld_wb_pr] <- 0
ld_wb_table <- table(pbp_late_win_rp_l3$pass, preds_ld_wb)
round(prop.table(ld_wb_table,1),2)
ld_wb_accuracy <- (ld_wb_table[1,1] + ld_wb_table[2,2]) / nrow(pbp_late_win_rp_l3)
