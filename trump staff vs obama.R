library(tidyverse)
library(stringr)

setwd("C:\\Users\\foxtr\\Desktop")
data_raw <-
  read_csv("presidential administration positions.csv")

split_words <- "( TO THE )|( FOR )|( OFFICE OF )|( TO THE PRESIDENT )|( TO THE PRESIDENT AND )|( OF )"

find_split <-
  function(string){
    str_replace(string, split_words, ":")
  }

positions <-
  data_raw %>% 
  select(Position.Title) %>% 
  distinct() %>% 
  mutate(president = str_detect(Position.Title, " PRESIDENT "),
         word_break = find_split(Position.Title)) %>% 
  separate(col = word_break, 
           into = c("title", "area"), sep = ":", remove = F, fill = "right")

str_replace(positions$Position.Title[11], split_words, "|")

count(positions, title, sort = T) %>% head()
count(positions, area, sort = T) %>% head()

positions_break <-
  positions %>% head() %>% 
  rowwise() %>% 
  mutate(to = str_locate(Position.Title, " TO ")[2],
         of = str_locate(Position.Title, " OF ")[2],
         for_ = str_locate(Position.Title, " FOR ")[2],
         office_of = str_locate(Position.Title, " OFFICE OF ")[2]) %>%
  ungroup() %>% 
  replace_na(replace = list(to=0, of=0, for_=0, office_of=0)) %>% 
  rowwise() %>% 
  #mutate(which = min(c(to,office_of), na.rm = T))
  gather(key = word, value = location, c(to:for_)) %>% 
  filter(!is.na(location)) %>% 
  group_by(Position.Title) %>% 
  mutate(min = min(location),
         max = max(location))
  
