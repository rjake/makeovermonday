setwd("J:/makeovermonday")
library(tidyverse)
library(stringi)

#http://www.makeovermonday.co.uk/data/

pTrans <- read.csv("public transport.csv", stringsAsFactors = F) %>%
  mutate(Year = as.integer(stri_sub(Survey.Date, -4)))

NPS <-
  pTrans %>%
  select(Year, City, Country, seq(4, 12, 2)) %>%
  mutate(Promoters = Very.satisfied + Rather.satisfied,
         Detractors = Rather.unsatisfied + Not.at.all.satisfied,
         N = Promoters + Detractors + Don.t.know,
         NPS = ((Promoters-Detractors)/N)*100) %>%
  select(Year:Country, NPS) %>%
  spread(key = Year, value = NPS) %>%
  mutate(NPSChange = `2012`-`2015`)

write.csv(NPS, "public transport_NPS.csv", row.names = F)
