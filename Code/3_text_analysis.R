rm(list=ls())
options(scipen = 10^6, digits=2)

library(tidyverse)
library(jsonlite)
library(lubridate)
library(stringi)
library(stringr)
library(tidytext)
library(spacyr)
spacy_initialize()  # Please follow CRAN instructions: https://cran.r-project.org/web/packages/spacyr/readme/README.html

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project"

setwd(wd)

###################################
### Text Processing ###############
###################################

# Sentiment Analysis of Trump's tweets

## Loading AFINN
sentiment_afinn <-
  read.delim(file.path("Input","AFINN-en-165.txt"), header = F) %>%
  as_tibble() %>%
  rename(word = 1, sentiment = 2)

## Wrangling set of tweets
tweets_df <-
  fromJSON(file.path("Input","tweets_01-08-2021.json")) %>%
  mutate(
    date = ymd_hms(date, tz = "EST"),
    text = stri_replace_all_regex(text, "(\\s?)http(s?)://.*$", ""),      # Removing URL addresses
    text = str_replace_all(text,                                          
                           c(     "&amp(;|,)?" = "and",               # Replacing HTML characters
                                  '@|#|&|%|$|""|--' = "",                  # Removing symbols
                                  "Donald J. Trump" = "Donald J Trump",    # standardizing Middle Name
                                  "\\-+Donald" = "Donald"))           # Formatting Trump Self-Cites
  ) %>%  
  filter(
    isRetweet == "f",                                           # Removing RTs
    !text == "",                                                # Removing empty processed tweets
    date > ymd("2018-01-01")                                    # Keeping tweets starting in 2018
  ) %>%          
  arrange(date) %>%
  mutate(tweet_id = row_number()) %>%
  select(tweet_id, date, text, device, favorites)

## Constructing list of words from tweets
tweets <- tweets_df[,c("tweet_id","text")]
list_tweets <- list()

for (i in 1:nrow(tweets)) {   # ~ Task lasts ~36min
  
  tweet <-
    tweets_df %>%
    filter(text == tweets$text[i]) %>%
    unnest_tokens(sentence, text, token = "sentences", drop = TRUE)
  
  vec_tweet <- tweet$sentence
  
  list_tweets[[i]] <-
    spacy_parse(vec_tweet, additional_attributes = c("is_stop")) %>%
    as_tibble() %>%
    select(-c(doc_id, token_id)) %>%
    mutate(tweet_id = tweets$tweet_id[i])
}

## Developing sentiment analysis from non-stop words
trump_sentiment <-
  do.call("rbind", list_tweets) %>%
  as_tibble() %>%
  left_join(tweets_df, by = "tweet_id") %>%
  filter(is_stop == FALSE, !pos == "PUNCT") %>%
  inner_join(sentiment_afinn, by = c("lemma" = "word")) %>%
  mutate(date = as.Date(date),
         week = floor_date(date, "weeks")) %>%
  group_by(week) %>%
  summarise(value = mean(sentiment, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(subject = "Trump")

# Sentiment of 16 major US newspapers
news_sentiment <-
  readxl::read_excel(file.path("Input","news_sentiment_data.xlsx"), sheet = "Data") %>%
  mutate(week = floor_date(ymd(date), unit = "weeks")) %>%
  rename(sentiment = `News Sentiment`) %>%
  group_by(week) %>%
  summarise(value = mean(sentiment, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(subject = "News")

# Merging both sentiments
sentiment_analysis <-
  rbind(trump_sentiment, news_sentiment) %>%
  filter(between(week, ymd("2018-01-01"), ymd("2020-11-30"))) %>%         # Keeping only last 3 years
  group_by(subject) %>%
  mutate(scaled = scale(value),
         scaled_smoothed = zoo::rollmean(scaled, k = 6,
                                         fill = NA, align = "right")) %>%
  ungroup()

# Creating Sentiment plot: Trump vs. US News
sentiment_analysis_plot <-
  ggplot(sentiment_analysis, aes(x = week, y = scaled_smoothed, color = subject, group = subject)) +
  geom_hline(yintercept = 0, color = "grey70", size = 1) +
  geom_line(size = 1.0) + 
  annotate(geom = "text",                                                                # Trump legend
           x = as.Date("2018-10-07"), y = 1.3, color = "red", fontface = "bold",  
           label = "Trump", hjust = "center", vjust = "right") +
  annotate(geom = "text",
           x = as.Date("2019-05-07"), y = 0.95, color = "blue", fontface = "bold",       # US News legend
           label = "US News", hjust = "center", vjust = "right") +
  annotate(geom = "curve",                                                               # Adding Flags
           x = as.Date("2020-01-30"), y = 0, xend = as.Date("2020-01-01"), yend = -1,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-01-01"), y = -1,
           label = "JAN.30\nThe WHO declared\na global health nemergency", hjust = "center", vjust = "right") +
  annotate(geom = "curve",
           x = as.Date("2020-03-15"), y = 0, xend = as.Date("2020-03-01"), yend = 1.1,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-03-01"), y = 1.1,
           label = "MAR.15\nCDC recommended\nno gatherings of 50+ people", hjust = "center", vjust = "left") +
  annotate(geom = "curve",
           x = as.Date("2020-03-26"), y = 0, xend = as.Date("2020-04-28"), yend = -0.5,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-04-28"), y = -0.5,
           label = "MAR.26\nUSA led the world\nin confirmed\ncases: 81k", hjust = "center", vjust = "right") +
  annotate(geom = "curve",
           x = as.Date("2020-05-27"), y = 0, xend = as.Date("2020-05-30"), yend = 1.5,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-05-30"), y = 1.5,
           label = "MAY.27\nCOVID-19 deaths\nin the US\npassed 100k", hjust = "center", vjust = "left") +
  annotate(geom = "curve",
           x = as.Date("2020-06-01"), y = 0, xend = as.Date("2020-08-30"), yend = 1.5,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-08-30"), y = 1.5,
           label = "MAY/JUN.\nBlack Lives\nMatters protests", hjust = "center", vjust = "left") +
  annotate(geom = "curve",
           x = as.Date("2020-07-13"), y = 0, xend = as.Date("2020-07-07"), yend = -0.6,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-07-07"), y = -0.6,
           label = "JUL.13\n5M+ Americans\nlost health insurance\ndue to job losses", hjust = "center", vjust = "right") +
  annotate(geom = "curve",
           x = as.Date("2020-08-01"), y = 0, xend = as.Date("2020-09-15"), yend = -0.2,
           curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-09-15"), y = -0.2,
           label = "AUG.1\nOnly in July\nthe USA recorded\n1.9M new\ncases", hjust = "center", vjust = "right") +
  annotate(geom = "curve",
           x = as.Date("2020-11-03"), y = 0, xend = as.Date("2020-10-30"), yend = 1.5,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-10-30"), y = 1.5,
           label = "NOV.3\nPresidential\nElections", hjust = "center", vjust = "left") +
  scale_color_manual(values = c("blue", "red")) +
  scale_y_continuous(limits = c(-2.5, 2.0), breaks = seq(-2.5, 2.0, 0.5)) +
  scale_x_date(date_breaks = "2 months",
               date_labels = format("%b-%Y"),
               limits = as.Date(c("2018-01-01","2020-11-30")),
               expand = c(0,0)) +
  labs(x = "", color = "", y = "Sentiment (scaled and smoothed)*",
       title    = "Trump sentiment got disconneted from News' sentiment during COVID-19",
       subtitle = "Sentiment Analysis: Trump's tweets vs. 16 Major US News, Jan 2018 - Nov 2020",
       caption  = paste0("Source: Trump's tweets copiled by TheTrumpArchive.com. Sentiment calculated by Authors.",
                         "News' Sentiment calculated by Federal Reserve Bank of San Francisco.\n",
                         "Timeline flags from New York Times: A timeline of the Coronavirus Pandemic.\n",
                         "*Scale to mean = 0. Smoothness through rolling average 6 weeks.")) +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_text(size = 10, face="bold", colour = "black"),
        axis.title = element_text(size = 13, face="bold", colour = "black"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5))

## -- News flags from The New York Times
## -- More details: https://www.nytimes.com/article/coronavirus-timeline.html

# Saving plot
ggsave(file.path("Output","sentiment_analysis_plot.png"), sentiment_analysis_plot, width = 20, height = 8.7)
