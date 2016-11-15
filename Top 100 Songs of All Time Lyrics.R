R Script for Text Analysis
#set up workspace
setwd("C:/Users/jriley215/Desktop/makeovermonday")
library(tidyverse)
library(tm)
library(SnowballC)
library(stringr)

#bring in data
stops <- stopwords()

makeTable <- 12

corpus <-
  read.csv("Top 100 Songs of All Time Lyrics.csv", stringsAsFactors = F) %>%
  mutate(Word2 = trimws(tolower(Word)),
         Stem = wordStem(Word2, language = "english"),
         Stopwords = ifelse((Word2 %in% stops), "Y", "N")) %>%
  group_by(Song.Rank, Word2) %>%
  mutate(WordTally = n()) %>%
  group_by(Song.Rank) %>%
  mutate(Location = ifelse(Order == 1, "First",
                    ifelse(Order == max(Order), "Last", NA))) %>%
  group_by(Song.Rank, Stopwords) %>%
  mutate(faveWord = ifelse(WordTally == max(WordTally), "Y", "N")) %>%
  ungroup() %>%
  mutate(Distinct = ifelse(WordTally == 1, "N", "Y"),
         RowFloor = floor(Order/makeTable)+1,
         ColFloor = (Order %% makeTable),
         FaveWordNoStop = ifelse(Stopwords == "Y", "N",
                          ifelse(faveWord == "Y", "Y", "N")))

#create csv
write.csv(corpus, "Song corpus.csv", row.names = F)


faveWords <-
  corpus %>%
  filter(faveWord == "Y") %>%
  distinct(Song.Rank, Word2, Stopwords, faveWord, WordTally) %>%
  group_by(Song.Rank, Stopwords) %>%
  summarise(Words = paste(Word2, collapse = ", ")) %>%
  ungroup()

write.csv(faveWords, "Song faveWords.csv", row.names = F)

wait4It <-
  corpus %>%
  arrange(Order) %>%
  group_by(Song.Rank, Song.Title, Artist) %>%
  summarise(Words = paste(Word2, collapse = " ")) %>%
  ungroup() %>%
  mutate(Title2 = gsub("(|\\.|)", "", Song.Title) %>% tolower(.),
         Title2 = gsub("loving feeling", "lovin' feelin'", Title2),
         Title2 = gsub("in the streets", "in the street", Title2),
         Title2 = gsub("change is gonna come", "change gonna come", Title2),
         Title2 = gsub("river deep,", "river's deep", Title2),
         Title2 = gsub("anarchy in the u k", "anarchy for the uk", Title2),
         Title2 = gsub("(this bird has flown)", "", Title2),
         Title2 = gsub("whole lotta shak", "whole lot of shak", Title2),
         Title2 = gsub(", pt1", "", Title2),
         nChar = nchar(Words))

wait4It$FindTitle <- 0

for(i in 1:nrow(wait4It)){
  wait4It$FindTitle[i] <- regexpr(wait4It$Title2[i], wait4It$Words[i])[1]
}

wait4It <-
  wait4It %>%
  mutate(tilTitle = ifelse(FindTitle == -1, NA, round(FindTitle/wait4It$nChar, 2)))


#artist
wordRatio <-
  corpus %>%
  group_by(Artist, Song.Title, Song.Rank) %>%
  summarise(nWords = n(),
            dWords = n_distinct(Stem),
            ratio = (nWords/dWords) %>% round(., 2)) %>%
  ungroup() %>%
  left_join(select(wait4It, Song.Rank, tilTitle))

write.csv(wordRatio, "Song wordRatio.csv", row.names = F)



wordRatio



ggplot(wordRatio, aes(nWords, dWords, color = Song.Rank)) +
  geom_point()

ggplot(wordRatio, aes(Song.Rank, nWords)) +
  geom_bar(stat = "identity")

ggplot(wordRatio, aes(Song.Rank, dWords)) +
  geom_bar(stat = "identity")


plot(wordRatio$nWords, wordRatio$dWords)

