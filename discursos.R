#loading packages

library(knitr)
library(magrittr)
library(dplyr)
library(wordcloud)
library(lexiconPT)
library(ggplot2)
library(tm)
library("SnowballC")
library(tidytext)
library(wordcloud2)
library(rvest)
library(purrr)
library(tidytext)
library(leaflet)
library(RColorBrewer)
library(colorspace)
library(htmlwidgets)
library(webshot)

webshot::install_phantomjs()

#loading Haddad's speech

text <- readLines("Disc_Haddad.txt")

#create df from Haddad'speech

dh_df <- tbl_df(map_df(text, as.data.frame))

#tidying df

dh_words <- dh_df %>% unnest_tokens(word, .x[[i]])

#cleaning df

dh_words$word <- dh_words$word %>% 
  tolower %>% 
  removePunctuation %>%
  removeNumbers %>%
  removeWords(., stopwords('pt')) %>%
  trimws("l")

# removing blank spaces and summarzing by word count

dh_check <- dh_words %>%
  select(word) %>%
  group_by(word) %>%
  summarize(total = n()) %>%
  filter(total < 10) %>%
  arrange(desc(total)) 

#creating simple wordcloud

set.seed(1234)
wordcloud(dh_check$word, freq = dh_check$total, min.freq = 1, scale = c(3, 0.1), 
          max.words=200, random.order=FALSE, rot.per=0, 
          colors=brewer.pal(9, "Reds"))

#creating new wordcloud with manual palette

wordcloud2(dh_check, size=.4, fontWeight = "600", 
           color = rev(brewer.pal(9, "Reds")), shape = 'circle')

hwc <- wordcloud2(dh_check, size=.4, fontWeight = "600", 
           color = c("DarkRed", "FireBrick", "Crimson", "Crimson", "Crimson", "Crimson",
                     "Red", "Red", "Red", "Red",
                     "OrangeRed", "OrangeRed", "OrangeRed", "OrangeRed",
                     "IndianRed", "IndianRed", "IndianRed", "IndianRed", "IndianRed", "IndianRed", "IndianRed", "IndianRed",
                     "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral"), shape = 'circle')

#using webshot to creat an image for the RMD file

saveWidget(hwc,"wordcloud.html",selfcontained = F)
webshot::webshot("wordcloud.html","wordcloud.png",vwidth = 800, vheight = 800, delay = 60)

#doing the same with Bolsonaro's speech

#loading the data
text_b <- readLines("Disc_B.txt")

#creating df
b_df <- tbl_df(map_df(text_b, as.data.frame))

#tidying df
b_words <- b_df %>% unnest_tokens(word, .x[[i]])

#cleaning df
b_words$word <- b_words$word %>% 
  tolower %>% 
  removePunctuation %>%
  removeNumbers %>%
  removeWords(., stopwords('pt')) %>%
  trimws("l")

#summarzing df by wordcount
b_check <- b_words %>%
  select(word) %>%
  group_by(word) %>%
  summarize(total = n()) %>%
  filter(total < 10) %>%
  arrange(desc(total)) 

#creating simple wordcloud
set.seed(1234)
wordcloud(b_check$word, freq = b_check$total, min.freq = 1, scale = c(3, 0.1), 
          max.words=70, random.order=FALSE, rot.per=0, 
          colors=brewer.pal(10, "BrBG"))

#creating wordcloud2
wordcloud2(b_check, size=.4, fontWeight = "600", 
           color = c("DarkGreen", "DarkOliveGreen", "OliveDrab", "OliveDrab",
                     "Green", "Green",
                     "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen",
                     "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen"),
                     shape = 'circle')
