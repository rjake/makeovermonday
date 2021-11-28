library(tidyverse)
library(readxl)
options(scipen = 999)

dataRaw <- 
  read_excel("2016_48_US_Income_Inequality.xlsx") %>% 
  rename_all(str_remove_all, "%$") %>%
  rename_all(str_replace_all, " ", ".")

data_long <- 
  dataRaw %>% 
  filter(Year > 1916) %>% 
  select(Year, Top.10:Top.1) %>%
  mutate_each(funs(gsub("%", "", .) %>% as.numeric(.)/100), -Year) %>%
  mutate(Bottom.90 = 1 - Top.10,
         Bottom.95 = 1 - Top.5,
         Bottom.99 = 1 - Top.1) %>% 
  gather(key = Group, value = Pct, -Year) %>% 
  mutate(Side = gsub("(.*)\\.(.*)", "\\1", Group),
         Amount = gsub("(.*)\\.(.*)", "\\2", Group) %>% as.integer(.),
         Pct = gsub("%", "", Pct) %>% as.numeric(.),
         GroupName = ifelse(Amount %in% c(10, 90), "90-10",
                     ifelse(Amount %in% c(1, 99), "99-1", "95-5"))) %>% 
  mutate(PctMil = 10000000 * Pct,
         perPerson = PctMil/Amount) %>% 
  group_by(Year, GroupName) %>% 
  arrange(desc(Side)) %>% 
  mutate(Ratio = ifelse(Side == "Top", perPerson/lead(perPerson), perPerson/lag(perPerson))) %>% 
  ungroup()
  
  
write.csv(data_long, "2016_48_US_Income_Inequality-long.csv", row.names = F)


data_wide <- 
  dataRaw %>% 
  filter(Year > 1916) %>% 
  select(Year, Top.10:Top.1) %>%
  mutate_each(funs(gsub("%", "", .) %>% as.numeric(.)/100), -Year) %>%
  mutate(Bottom.90 = 1 - Top.10,
         Top.10 = Top.10 - Top.5,
         Top.5 = Top.5 - Top.1) %>% 
  gather(key = Group, value = Pct, -Year) %>% 
  mutate(Side = gsub("(.*)\\.(.*)", "\\1", Group),
         Amount = gsub("(.*)\\.(.*)", "\\2", Group) %>% as.integer(.),
         Pct = gsub("%", "", Pct) %>% as.numeric(.)) %>% 
  mutate(PctMil = 10000000 * Pct,
         perPerson = PctMil/Amount) %>% 
  group_by(Year) %>% 
  arrange(desc(Side)) %>% 
  mutate(Value90 = min(perPerson)) %>% 
  group_by(Amount) %>%
  arrange(Year) %>% 
  mutate(PctPtChg = Pct - lag(Pct),
         PctChg = round(PctPtChg/lag(Pct), 3)) %>% 
  ungroup() %>% 
  mutate(Ratio = ifelse(Amount == 90, NA, perPerson/Value90),
         PctPtChgCat = cut(PctPtChg, 
                           breaks = c(-Inf, -0.02, -0.01, -0.005, 0.005, 0.01, .02, Inf),
                           include.lowest = T))



write.csv(data_wide, "2016_48_US_Income_Inequality-wide.csv", row.names = F)
