library(tidyr)
library(dplyr)
library(ggplot2)
# library(lubridate)

# Los archivos deben estar en utf-8!!
v <- read.csv('analytics/datos/vsh.psv', sep='|', header=TRUE, fileEncoding = 'UTF-8')
nv <- read.csv('analytics/datos/nvsh.psv', sep='|', header=TRUE, fileEncoding = 'UTF-8')

dat <- rbind(
  data.frame(rec = TRUE, v),
  data.frame(rec = FALSE, nv)
) %>%
  group_by(rec, date) %>%
  summarise(users = sum(users),
         bounceRate = sum(bounceRate * sessions)/sum(sessions),
         avgSessionDuration= sum(avgSessionDuration * sessions)/sum(sessions)/60,
         goalConversionRateAll = sum(goalConversionRateAll* sessions)/sum(sessions),
         pageviews = sum(pageviews),
         sessions = sum(sessions)) %>%
  mutate(pageviews_per_session = pageviews / sessions) %>%
  filter(!is.na(date)) %>%
  ungroup
dat <- dat[dat$pageviews < max(dat$pageviews), ]

dat$date <- strptime(dat$date, '%Y%m%d') %>%
  as.character %>%
  as.Date

dat2 <- dat %>%
  gather(var, val, sessions, bounceRate, avgSessionDuration, goalConversionRateAll, pageviews_per_session)

qplot(date, bounceRate, data=dat, geom='line', group=rec)
qplot(date, val, data=dat2, geom='line', group=rec, color=rec) +
  geom_smooth() +
  facet_wrap(~ var, scales = 'free_y') +
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date()


dat %>%
  group_by(rec) %>%
  summarise(tot_sessions = sum(sessions),
         bounce_rate = sum(bounceRate * sessions)/sum(sessions),
         avg_session_minutes= sum(avgSessionDuration * sessions)/sum(sessions)/60,
         conversion_rate = sum(goalConversionRateAll* sessions)/sum(sessions)) %>%
  mutate(perc_sessions = 100*tot_sessions/sum(tot_sessions)) %>%
  sapply(function(x){ if(is.numeric(x)) round(x, 2) else x}) %>%
  data.frame
  
