library(tidyverse)
library(rvest)
library(lubridate)
library(qicharts2)
library(scales)

url <- "https://docs.google.com/spreadsheets/d/1b9o6uDO18sLxBqPwl_Gh9bnhW-ev_dABH83M5Vb5L8o/htmlview?sle=true"

html_result <- read_html(url)

raw_data <- 
  html_result %>% 
  html_node("table") %>% 
  html_table()

cols <- which(raw_data[1,] == "location")
raw_data[1,][cols] <- c("location", "location_type")

prep_data <-
  raw_data[-(1:2),] %>% 
  setNames(raw_data[1,]) %>% 
  select(-`1`) %>% 
  mutate_at(vars(matches("fatal|injur|total|age_|year")), as.integer) %>% 
  mutate_at(vars(matches("tude")), as.numeric) %>% 
  mutate(
    date = mdy(date),
    n = 1,
    automatic_ind = str_detect(weapon_type, "automatic")
    
  ) %>% 
  select_if(function(x) max(nchar(x)) < 50)


rare_events <-
  prep_data %>% 
  arrange(date) %>% 
  mutate(x = row_number()) %>% 
  filter(automatic_ind) %>% 
  mutate(
    days_between = date - lag(date, default = first(date))
  ) %>% 
  slice(-1)


rare_events %>% 
  group_by(year > 2004) %>% 
  summarise(
    n = n(),
    fatal = sum(fatalities),
    injured = sum(injured)
  )

stat_labels <- c(
  "1982-2004\n24 shootings\n198 dead, 218 injured\navg. 261 days", 
  "2005-2019\n50 shootings\n511 dead, 1015 injured\navg. 68 days"
)


qic(
  days_between,
  data = rare_events,
  chart = "t",
  title = "Mass shootings with an automatic or semi-automatic weapon happen \nfour times more frequently now than before 2004",
  subtitle = "There have been 74 mass shootings of this kind between 1984 - August 4th, 2019.",
  caption = "Source:\nMother Jones’ Investigation\nUS Mass Shootings 1982-2019",
  part = 24,
  part.labels = stat_labels,
  xlab = "Shootings in order",
  ylab = "Days between", 
  show.labels = FALSE
)



by_year <-
  prep_data %>% 
  group_by(x = year) %>% #floor_date(date, "year")) %>% 
  filter(automatic_ind) %>% 
  summarise(
    n = n(),
    auto = sum(automatic_ind),
    fatalities = sum(fatalities)
  ) %>% 
  ungroup()


by_year %>% 
  arrange(x) %>% 
  mutate(
    sum = cumsum(fatalities)
  ) %>% 
  ggplot(aes(x)) +
  geom_area(aes(y = sum)) +
  # geom_step(aes(y = sum)) +
  # geom_smooth(aes(y = sum)) +
  geom_col(aes(y = fatalities), fill = "red")



qic(
  x = date,
  y = fatalities,
  point.size = 3,
  part = 15,
  data = df,
  chart = "c",
  x.period = "year"
)


