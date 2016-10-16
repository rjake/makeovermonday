setwd("C:/Users/jriley215/Desktop/makeovermonday")
library(tidyverse)
library(stringi)

pTrans <- read.csv("public transport.csv", stringsAsFactors = F) %>%
  mutate(Year = as.integer(stri_sub(Survey.Date, -4)))

pTransFinal <-
  pTrans %>%
  select(Year, City, Country, seq(4, 12, 2)) %>%
  gather(key = Response, value = N, -c(Year:Country)) %>%
  mutate(Response = gsub("n.t", "n't", Response),
         Response = gsub("\\.", " ", Response)) %>%
  group_by(Year, City, Country) %>%
  mutate(Total = sum(N),
         Pct = round(N/Total, 3)*100) %>%
  group_by(City, Country, Response) %>%
  ungroup()

pTransChange <-
  pTransFinal %>%
  select(-N, -Total) %>%
  spread(key = Year, value = Pct) %>%
  mutate(Change = `2012`-`2015`)



write.csv(pTransFinal, "public transport_edited.csv", row.names = F)

write.csv(pTransChange, "public transport_change.csv", row.names = F)

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
