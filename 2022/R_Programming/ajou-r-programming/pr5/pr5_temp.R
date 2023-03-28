rm(list = ls())

Sys.setlocale('LC_ALL', 'C')
data = read.csv('업종 카드소비 트렌드.csv', encoding = 'UTF-8')
data[which(is.na(data$agrde_code)), 'agrde_code']
table(data$agrde_code)

data$agrde_code[is.na(data$agrde_code)] = 0
data$agrde_code = factor(
  data$agrde_code,
  levels = c(0, 1, 2, 3, 4, 5, 6, 7),
  labels = c(
    '-',
    '20대 미만',
    "20세~29세",
    '30세~39세',
    '40세~49세',
    '50세~59세',
    '60세~69세',
    '70세 이상'
  ),
  
)
data$agrde_code[is.na(data$agrde_code)] = '-'

korean_food = factor(levels(factor(data$induty_nm)))
# \uD55C\uC2DD 한식
korean_food = data[korean_food == levels(korean_food)[(levels(korean_food) == '\uD55C\uC2DD')],]

head(korean_food[order(korean_food$setle_cascnt, decreasing = T), ], 5)

korean_food[korean_food$agrde_code == levels(data$agrde_code)[3] &
              korean_food$setle_cascnt >= 10000 &
              korean_food$setle_cascnt <= 150000,][1]

#install.packages('devtools')
#library(devtools)
#devtools::install_github('JaseZiv/worldfootballR', ref = 'main')
library(worldfootballR)

match_summary = fb_match_summary(match_url = "https://fbref.com/en/matches/74aed880/Ajax-Napoli-October-4-2022-Champions-League")
match_summary[match_summary$Home_Away == 'Away' &
                match_summary$Event_Type == 'Goal', 'Event_Players']

shooting = fb_match_shooting(
  "https://fbref.com/en/matches/2f44d120/Eintracht-Frankfurt-Totterham-Hotspur-October-4-2022-Champions-League"
)
shooting[shooting$Shooting_Player == 'Son Heung-min' |
           shooting$Shooting_Player == 'Harry Kane', ]

man_city_url = "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats"
man_city_logs = fb_team_match_log_stats(team_urls = man_city_url, stat_type =
                                          "passing")
man_city_logs[man_city_logs$Result == 'W' &
                man_city_logs$PPA > 10, ]
