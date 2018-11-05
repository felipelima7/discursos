setwd("C:/Users/Felipe/Downloads/Curso/Discursos")
install.packages("wordcloud2")
install.packages("colorspace")
install.packages("knitr")

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

?tidytext

text <- readLines("Disc_Haddad.txt")
docs <- Corpus(VectorSource(text))


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords('portuguese'))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

inspect(docs)

#backup

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = enc2native(names(v)),freq=v)

d$word <- as.character(d$word)

set.seed(1234)
wordcloud(enc2native(d$word), freq = d$freq, min.freq = 2, scale = c(3, 0.1), 
          max.words=200, random.order=FALSE, rot.per=0, 
          colors=brewer.pal(9, "Reds"))


?wordcloud

#stopwords

#salvando rm
rm(toSpace)
rm(docs)
rm(d)
rm(dtm)
rm(m)
rm(v)

#tentativa com rvest

url <- 'https://www.poder360.com.br/eleicoes/leia-a-integra-do-discurso-de-derrota-fernando-haddad/'
webpage <- read_html(url)

#using CSS selectors to scrap the ranking section
dh_html <- html_nodes(webpage, ".visible-xs~ p em , p:nth-child(13) em , p:nth-child(12) em , p:nth-child(11) em")
dh <- html_text(dh_html)

#backup dh
dh2 <- dh

#continuando

text <- readLines("Disc_Haddad.txt")

dh_df <- tbl_df(map_df(text, as.data.frame))

dh_words <- dh_df %>% unnest_tokens(word, .x[[i]])

#até aqui deu certo

#backup de dh_words

dh_words2 <- dh_words

dh_words$word <- dh_words$word %>% 
  tolower %>% 
  removePunctuation %>%
  removeNumbers %>%
  removeWords(., stopwords('pt')) %>%
  trimws("l")

dh_check <- dh_words %>%
  select(word) %>%
  group_by(word) %>%
  summarize(total = n()) %>%
  filter(total < 10) %>%
  arrange(desc(total)) 

#ok até aqui

set.seed(1234)
wordcloud(dh_check$word, freq = dh_check$total, min.freq = 1, scale = c(3, 0.1), 
          max.words=200, random.order=FALSE, rot.per=0, 
          colors=brewer.pal(9, "Reds"))

#ok

#falta criar uma escala manual de cores com ao menos 18 passos

wordcloud2(dh_check, size=.4, fontWeight = "600", 
           color = rev(brewer.pal(9, "Reds")), shape = 'circle')

wordcloud2(dh_check, size=.4, fontWeight = "600", 
           color = c("DarkRed", "FireBrick", "Crimson", "Crimson", "Crimson", "Crimson",
                     "Red", "Red", "Red", "Red",
                     "OrangeRed", "OrangeRed", "OrangeRed", "OrangeRed",
                     "IndianRed", "IndianRed", "IndianRed", "IndianRed", "IndianRed", "IndianRed", "IndianRed", "IndianRed",
                     "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral", "LightCoral"), shape = 'circle')

#agora com b

text_b <- readLines("Disc_B.txt")

b_df <- tbl_df(map_df(text_b, as.data.frame))

b_words <- b_df %>% unnest_tokens(word, .x[[i]])

b_words$word <- b_words$word %>% 
  tolower %>% 
  removePunctuation %>%
  removeNumbers %>%
  removeWords(., stopwords('pt')) %>%
  trimws("l")

b_check <- b_words %>%
  select(word) %>%
  group_by(word) %>%
  summarize(total = n()) %>%
  filter(total < 10) %>%
  arrange(desc(total)) 

set.seed(1234)
wordcloud(b_check$word, freq = b_check$total, min.freq = 1, scale = c(3, 0.1), 
          max.words=70, random.order=FALSE, rot.per=0, 
          colors=brewer.pal(10, "BrBG"))

wordcloud2(b_check, size=.4, fontWeight = "600", 
           color = c("DarkGreen", "DarkOliveGreen", "OliveDrab", "OliveDrab",
                     "Green", "Green",
                     "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen", "SeaGreen",
                     "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen", "LightSeaGreen"),
                     shape = 'circle')

