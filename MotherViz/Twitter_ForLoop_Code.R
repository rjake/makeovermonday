
library(dplyr)

getwd()
setwd("/Users/mitchell/Downloads")
Jake_MetaData <- read.csv("metadata_withLinks.csv",stringsAsFactors = F)

Jake_MetaData$Original.Link[[15]]


# This is the for loop to run the code...
for(i in 1:nrow(Jake_MetaData1)){
 #i = 2
a <-as.data.frame(Jake_MetaData1$Original.Link[[i]])
a <- a %>% filter(grepl("mobile.twitter", a[,1]))
names(a)[1]<-"Lines"
a <- which(grepl("mobile.twitter", a$Lines))
if (length(a) == 1) 
{
  error.URLs.C <- Jake_MetaData1[i,]
  error.URLs.C$Error_Type <-"Twitter Mobile type URL" 
  #NoTwitter.URLs$Error_Type <- NA
  NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.C) %>% distinct() 
  a  <- data.frame(Retweet_Num = NA)
  b  <- data.frame(Likes_Num = NA)
  c  <- data.frame(Name = NA)
  d  <- data.frame(Time_Stamp = NA)
  e  <- data.frame(Handle = NA)
  f <- data.frame(ID = Jake_MetaData1$ID[[i]])
  g <- data.frame(Error_Type = "Twitter Mobile type URL")
  h <- data.frame(Original_Link = Jake_MetaData1$Original.Link[[i]])
  j <- bind_cols(a,b,c,d,e,f,g,h)
  TweetData <- rbind(TweetData,j) %>% distinct()
  
  print("Twitter Mobile type") 
  
  next()
}

a <-as.data.frame(Jake_MetaData1$Original.Link[[i]])
names(a)[1]<-"Lines"
a$Lines <- as.character(a$Lines)
a <- a %>% filter(grepl("twitter.com", a[,1])) 
Tester.a <- which(grepl("twitter.com", a$Lines))

b <-as.data.frame(Jake_MetaData1$Original.Link[[i]])
names(b)[1]<-"Lines"
b$Lines <- as.character(b$Lines)
b <- b %>% filter(grepl("tweetdeck", b[,1])) 
Tester.b <- which(grepl("tweetdeck", b$Lines))

if ((length(Tester.a)) + (length(Tester.b)) == 1)
  {
 # i = 2
  
 # urldata <- scan(Jake_MetaData1$Original.Link[[i]], what="", sep="\n") 
urldata <- tryCatch(scan(Jake_MetaData1$Original.Link[[i]], what="", sep="\n"), error=function(err) "Error 404") 
 if (urldata == "Error 404") {
error.URLs.B <- Jake_MetaData1[i,]
error.URLs.B$Error_Type <-"Error 404" 
#NoTwitter.URLs$Error_Type <- NA
NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.B) %>% distinct() 
a  <- data.frame(Retweet_Num = NA)
b  <- data.frame(Likes_Num = NA)
c  <- data.frame(Name = NA)
d  <- data.frame(Time_Stamp = NA)
e  <- data.frame(Handle = NA)
f <- data.frame(ID = Jake_MetaData1$ID[[i]])
g <- data.frame(Error_Type = "Error 404")
h <- data.frame(Original_Link = Jake_MetaData1$Original.Link[[i]])
j <- bind_cols(a,b,c,d,e,f,g,h)
TweetData <- rbind(TweetData,j) %>% distinct()

print("Error 404") 

next()
} 
  ## Code for 'Retweets_Num'...
  a <- data.frame(Lines = urldata)
  a <- a %>% filter(grepl("strong", a[,1]))
  a <- a %>% filter(grepl("Retweet", a[,1]))

  Tester.a <- which(grepl("Retweet", a$Lines))
  if (length(Tester.a) == 0) # No 'Retweet'
    {
   a  <- data.frame(Retweet_Num = NA)
  } else 
    {
    spl <<-strsplit(as.character(a$Lines), ">")
    a <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                    , a, stringsAsFactors = F) 
    spl <<-strsplit(as.character(a$Temp.2), "<")
    a <- data.frame(Retweet_Num = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                    , a, stringsAsFactors = F) %>% select(1)    
  }
  # Code for 'Likes_Num'
  b <- data.frame(Lines = urldata)
  b <- b %>% filter(grepl("strong", b[,1]))
  b <- b %>% filter(grepl("Like", b[,1]))
  
  Tester.b <- which(grepl("Like", b$Lines))
  if (length(Tester.b) == 0) # No 'Likes'
  {
    b  <- data.frame(Likes_Num = NA)
  } else 
  {
  spl <<-strsplit(as.character(b$Lines), ">")
  b <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                  , b, stringsAsFactors = F) 
  spl <<-strsplit(as.character(b$Temp.2), "<")
  b <- data.frame(Likes_Num = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                  , b, stringsAsFactors = F) %>% select(1)
  }
  ## Code for 'Name'
  c <- data.frame(Lines = urldata)
  c <- c %>% filter(grepl("strong", c[,1]))
  c <- c %>% filter(grepl("fullname", c[,1])) 
  c$Lines <- c$Lines[[1]]  
  c <- c %>% distinct()
  
  Tester.c <- which(grepl("fullname", c$Lines))
  if (length(Tester.c) == 0) # No 'Name'
  {
    c  <- data.frame(Name = NA)
  } else 
  {
  spl <<-strsplit(as.character(c$Lines), ">")
  c <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                  , c, stringsAsFactors = F) 
  spl <<-strsplit(as.character(c$Temp.2), "<")
  c <- data.frame(Name = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                  , c, stringsAsFactors = F) %>% select(1)
  }
  ## Code for 'Time_Stamp'...
  d <- data.frame(Lines = urldata)
  d <- d %>% filter(grepl("/a", d[,1]))
  d <- d %>% filter(grepl("tweet-timestamp", d[,1])) %>% distinct() 
  d$Lines <- d$Lines[[1]]
  d <- d %>% distinct()
  
  Tester.d <- which(grepl("tweet-timestamp", d$Lines))
  if (length(Tester.d) == 0) # No 'Time_Stamp'
  {
    d  <- data.frame(Time_Stamp = NA)
  } else 
  {  
  spl <<-strsplit(as.character(d$Lines), "title=")
  d <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                  , d, stringsAsFactors = F) 
  spl <<-strsplit(as.character(d$Temp.2), ">")
  d <- data.frame(Time_Stamp = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                  , d, stringsAsFactors = F) %>% select(1) 
  d$Time_Stamp <- (gsub("\"", "", d$Time_Stamp))
  }
  # Code for 'Handle'
  e <- data.frame(Lines = urldata)
  e <- e %>% filter(grepl("/a", e[,1]))
  e <- e %>% filter(grepl("tweet-timestamp", e[,1])) %>% distinct() 
  e$Lines <- e$Lines[[1]]
  e <- e %>% distinct()
  
  Tester.e <- which(grepl("tweet-timestamp", e$Lines))
  if (length(Tester.e) == 0) # No 'Handle'
  {
    e  <- data.frame(Handle = NA)
  } else 
  { 
  spl <<-strsplit(as.character(e$Lines), "/")
  e <- data.frame(Temp.1 = sapply(spl, "[", 2)
                  , e, stringsAsFactors = F) 
  spl <<-strsplit(as.character(e$Temp.1), "/")
  e <- data.frame(Handle = sapply(spl, "[", 1)
                  , e, stringsAsFactors = F) %>% select(1) 
  e$atStamp <- "@"
  e <- e %>% mutate(Handle = paste0(atStamp,Handle)) %>% select(1)
  }
  # Joined
f <- data.frame(ID = Jake_MetaData1$ID[[i]])
g <- data.frame(Error_Type = "No Error")
h <- data.frame(Original_Link = Jake_MetaData1$Original.Link[[i]])
j <- bind_cols(a,b,c,d,e,f,g,h)
TweetData <- rbind(TweetData,j) %>% distinct()
} else 
  {    
error.URLs.A <- Jake_MetaData1[i,]
error.URLs.A$Error_Type <-"Not Twitter URL/Variety Error Type" 
# NoTwitter.URLs <- rbind(error.URLs.A,error.URLs.A) %>% distinct()
NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.A) %>% distinct() 

a  <- data.frame(Retweet_Num = NA)
b  <- data.frame(Likes_Num = NA)
c  <- data.frame(Name = NA)
d  <- data.frame(Time_Stamp = NA)
e  <- data.frame(Handle = NA)
f <- data.frame(ID = Jake_MetaData1$ID[[i]])
g <- data.frame(Error_Type = "Not Twitter URL/Variety Error Type")
h <- data.frame(Original_Link = Jake_MetaData1$Original.Link[[i]])
j <- bind_cols(a,b,c,d,e,f,g,h)
TweetData <- rbind(TweetData,j) %>% distinct()
# TweetData <- rbind(i,i) %>% distinct()

print("Not Twitter URL/Variety Error Type") 
next()
}

print(i)

}

remove(NoTwitter.URLs)
#remove(TweetData)
#TweetData <- TweetData[1,] # This is used for recreating the first row of data to join to...
#TweetData1 <- TweetData
#TweetData <- TweetData1    # This is used for recreating the first row of data to join to...

# Code for slicing, reading in, and writing out data...
{
  Jake_MetaData1 <- Jake_MetaData %>% slice(1:775)
  Jake_MetaData2 <- Jake_MetaData %>% slice(776:1550)
  Jake_MetaData3 <- Jake_MetaData %>% slice(1551:2325)
  Jake_MetaData4 <- Jake_MetaData %>% slice(2326:3100)
write.csv(TweetData, "TweetData_A.csv", row.names = F)
TweetData_A <- read.csv("TweetData_A.csv", stringsAsFactors = F)
setwd("/Users/mitchell")
TweetData_B <- read.csv("TweetData_B.csv", stringsAsFactors = F)
TweetData_C <- read.csv("TweetData_C.csv", stringsAsFactors = F)
TweetData_D <- read.csv("TweetData_D.csv", stringsAsFactors = F)
setwd("/Users/mitchell/Downloads/Jake_Mother_Proj")

TweetData_All <- bind_rows(TweetData_A,TweetData_B,TweetData_C,TweetData_D) %>% distinct()
write.csv(TweetData_All,"TweetData_All.csv",row.names = F)
}
# Complete script for Jake_MetaData2...(for running the script in seperate Rstudio).
{
library(dplyr)

getwd()
setwd("/Users/mitchell/Downloads")
Jake_MetaData <- read.csv("metadata_withLinks.csv",stringsAsFactors = F)

# This line of code is used for checking faulty URLs...
Jake_MetaData2$Original.Link[[495]]

# This is the second portion of the sliced data...
Jake_MetaData2 <- Jake_MetaData %>% slice(776:1550)

for(i in 1:nrow(Jake_MetaData2)){
  #i = 495
  a <-as.data.frame(Jake_MetaData2$Original.Link[[i]])
  a <- a %>% filter(grepl("mobile.twitter", a[,1]))
  names(a)[1]<-"Lines"
  a <- which(grepl("mobile.twitter", a$Lines))
  
  b <-as.data.frame(Jake_MetaData2$Original.Link[[i]])
  b <- b %>% filter(grepl("tilnamrata|notifications", b[,1]))
  names(b)[1]<-"Lines"
  b <- which(grepl("tilnamrata|notifications", b$Lines))
  
  if ((length(a)) + (length(b)) >= 1)
  {
    error.URLs.C <- Jake_MetaData2[i,]
    error.URLs.C$Error_Type <-"Twitter Mobile type URL" 
    #NoTwitter.URLs$Error_Type <- NA
    NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.C) %>% distinct() 
    a  <- data.frame(Retweet_Num = NA)
    b  <- data.frame(Likes_Num = NA)
    c  <- data.frame(Name = NA)
    d  <- data.frame(Time_Stamp = NA)
    e  <- data.frame(Handle = NA)
    f <- data.frame(ID = Jake_MetaData2$ID[[i]])
    g <- data.frame(Error_Type = "Twitter Mobile type URL")
    h <- data.frame(Original_Link = Jake_MetaData2$Original.Link[[i]])
    j <- bind_cols(a,b,c,d,e,f,g,h)
    TweetData <- rbind(TweetData,j) %>% distinct()
    
    print("Twitter Mobile type") 
    
    next()
  }
  
  a <-as.data.frame(Jake_MetaData2$Original.Link[[i]])
  names(a)[1]<-"Lines"
  a$Lines <- as.character(a$Lines)
  a <- a %>% filter(grepl("twitter.com", a[,1])) 
  Tester.a <- which(grepl("twitter.com", a$Lines))
  
  b <-as.data.frame(Jake_MetaData2$Original.Link[[i]])
  names(b)[1]<-"Lines"
  b$Lines <- as.character(b$Lines)
  b <- b %>% filter(grepl("tweetdeck", b[,1])) 
  Tester.b <- which(grepl("tweetdeck", b$Lines))
  
  if ((length(Tester.a)) + (length(Tester.b)) == 1)
  {
    # i = 2
    
    # urldata <- scan(Jake_MetaData2$Original.Link[[i]], what="", sep="\n") 
    urldata <- tryCatch(scan(Jake_MetaData2$Original.Link[[i]], what="", sep="\n"), error=function(err) "Error 404") 
    if (urldata == "Error 404") {
      error.URLs.B <- Jake_MetaData2[i,]
      error.URLs.B$Error_Type <-"Error 404" 
      #NoTwitter.URLs$Error_Type <- NA
      NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.B) %>% distinct() 
      a  <- data.frame(Retweet_Num = NA)
      b  <- data.frame(Likes_Num = NA)
      c  <- data.frame(Name = NA)
      d  <- data.frame(Time_Stamp = NA)
      e  <- data.frame(Handle = NA)
      f <- data.frame(ID = Jake_MetaData2$ID[[i]])
      g <- data.frame(Error_Type = "Error 404")
      h <- data.frame(Original_Link = Jake_MetaData2$Original.Link[[i]])
      j <- bind_cols(a,b,c,d,e,f,g,h)
      TweetData <- rbind(TweetData,j) %>% distinct()
      
      print("Error 404") 
      
      next()
    } 
    ## Code for 'Retweets_Num'...
    a <- data.frame(Lines = urldata)
    a <- a %>% filter(grepl("strong", a[,1]))
    a <- a %>% filter(grepl("Retweet", a[,1]))
    
    Tester.a <- which(grepl("Retweet", a$Lines))
    if (length(Tester.a) == 0) # No 'Retweet'
    {
      a  <- data.frame(Retweet_Num = NA)
    } else 
    {
      spl <<-strsplit(as.character(a$Lines), ">")
      a <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                      , a, stringsAsFactors = F) 
      spl <<-strsplit(as.character(a$Temp.2), "<")
      a <- data.frame(Retweet_Num = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                      , a, stringsAsFactors = F) %>% select(1)    
    }
    # Code for 'Likes_Num'
    b <- data.frame(Lines = urldata)
    b <- b %>% filter(grepl("strong", b[,1]))
    b <- b %>% filter(grepl("Like", b[,1]))
    
    Tester.b <- which(grepl("Like", b$Lines))
    if (length(Tester.b) == 0) # No 'Likes'
    {
      b  <- data.frame(Likes_Num = NA)
    } else 
    {
      spl <<-strsplit(as.character(b$Lines), ">")
      b <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                      , b, stringsAsFactors = F) 
      spl <<-strsplit(as.character(b$Temp.2), "<")
      b <- data.frame(Likes_Num = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                      , b, stringsAsFactors = F) %>% select(1)
    }
    ## Code for 'Name'
    c <- data.frame(Lines = urldata)
    c <- c %>% filter(grepl("strong", c[,1]))
    c <- c %>% filter(grepl("fullname", c[,1])) 
    c$Lines <- c$Lines[[1]]  
    c <- c %>% distinct()
    
    Tester.c <- which(grepl("fullname", c$Lines))
    if (length(Tester.c) == 0) # No 'Name'
    {
      c  <- data.frame(Name = NA)
    } else 
    {
      spl <<-strsplit(as.character(c$Lines), ">")
      c <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                      , c, stringsAsFactors = F) 
      spl <<-strsplit(as.character(c$Temp.2), "<")
      c <- data.frame(Name = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                      , c, stringsAsFactors = F) %>% select(1)
    }
    ## Code for 'Time_Stamp'...
    d <- data.frame(Lines = urldata)
    d <- d %>% filter(grepl("/a", d[,1]))
    d <- d %>% filter(grepl("tweet-timestamp", d[,1])) %>% distinct() 
    d$Lines <- d$Lines[[1]]
    d <- d %>% distinct()
    
    Tester.d <- which(grepl("tweet-timestamp", d$Lines))
    if (length(Tester.d) == 0) # No 'Time_Stamp'
    {
      d  <- data.frame(Time_Stamp = NA)
    } else 
    {  
      spl <<-strsplit(as.character(d$Lines), "title=")
      d <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                      , d, stringsAsFactors = F) 
      spl <<-strsplit(as.character(d$Temp.2), ">")
      d <- data.frame(Time_Stamp = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                      , d, stringsAsFactors = F) %>% select(1) 
      d$Time_Stamp <- (gsub("\"", "", d$Time_Stamp))
    }
    # Code for 'Handle'
    e <- data.frame(Lines = urldata)
    e <- e %>% filter(grepl("/a", e[,1]))
    e <- e %>% filter(grepl("tweet-timestamp", e[,1])) %>% distinct() 
    e$Lines <- e$Lines[[1]]
    e <- e %>% distinct()
    
    Tester.e <- which(grepl("tweet-timestamp", e$Lines))
    if (length(Tester.e) == 0) # No 'Handle'
    {
      e  <- data.frame(Handle = NA)
    } else 
    { 
      spl <<-strsplit(as.character(e$Lines), "/")
      e <- data.frame(Temp.1 = sapply(spl, "[", 2)
                      , e, stringsAsFactors = F) 
      spl <<-strsplit(as.character(e$Temp.1), "/")
      e <- data.frame(Handle = sapply(spl, "[", 1)
                      , e, stringsAsFactors = F) %>% select(1) 
      e$atStamp <- "@"
      e <- e %>% mutate(Handle = paste0(atStamp,Handle)) %>% select(1)
    }
    # Joined
    f <- data.frame(ID = Jake_MetaData2$ID[[i]])
    g <- data.frame(Error_Type = "No Error")
    h <- data.frame(Original_Link = Jake_MetaData2$Original.Link[[i]])
    j <- bind_cols(a,b,c,d,e,f,g,h)
    TweetData <- rbind(TweetData,j) %>% distinct()
  } else 
  {    
    error.URLs.A <- Jake_MetaData2[i,]
    error.URLs.A$Error_Type <-"Not Twitter URL/Variety Error Type" 
    # NoTwitter.URLs <- rbind(error.URLs.A,error.URLs.A) %>% distinct()
    NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.A) %>% distinct() 
    
    a  <- data.frame(Retweet_Num = NA)
    b  <- data.frame(Likes_Num = NA)
    c  <- data.frame(Name = NA)
    d  <- data.frame(Time_Stamp = NA)
    e  <- data.frame(Handle = NA)
    f <- data.frame(ID = Jake_MetaData2$ID[[i]])
    g <- data.frame(Error_Type = "Not Twitter URL/Variety Error Type")
    h <- data.frame(Original_Link = Jake_MetaData2$Original.Link[[i]])
    j <- bind_cols(a,b,c,d,e,f,g,h)
    TweetData <- rbind(TweetData,j) %>% distinct()
    # TweetData <- rbind(i,i) %>% distinct()
    
    print("Not Twitter URL/Variety Error Type") 
    next()
  }
  
  print(i)
  
}

remove(NoTwitter.URLs)
#remove(TweetData)
#TweetData <- TweetData[1,]
#TweetData1 <- TweetData
#TweetData <- TweetData1
write.csv(TweetData, "TweetData_B.csv", row.names = F)
getwd()
} 
# Complete script for Jake_MetaData3...(for running the script in seperate Rstudio).
{
  library(dplyr)
  
  getwd()
  setwd("/Users/mitchell/Downloads")
  Jake_MetaData <- read.csv("metadata_withLinks.csv",stringsAsFactors = F)
  
  Jake_MetaData3$Original.Link[[699]]
  
  Jake_MetaData3 <- Jake_MetaData %>% slice(1551:2325)

  
  for(i in 699:nrow(Jake_MetaData3)){
    #i = 330
    a <-as.data.frame(Jake_MetaData3$Original.Link[[i]])
    a <- a %>% filter(grepl("mobile.twitter", a[,1]))
    names(a)[1]<-"Lines"
    a <- which(grepl("mobile.twitter", a$Lines))
    
    b <-as.data.frame(Jake_MetaData3$Original.Link[[i]])
    b <- b %>% filter(grepl("tilnamrata|notifications", b[,1]))
    names(b)[1]<-"Lines"
    b <- which(grepl("tilnamrata|notifications", b$Lines))
    
    if ((length(a)) + (length(b)) >= 1)
    {
      error.URLs.C <- Jake_MetaData3[i,]
      error.URLs.C$Error_Type <-"Twitter Mobile type URL" 
      #NoTwitter.URLs$Error_Type <- NA
      NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.C) %>% distinct() 
      a  <- data.frame(Retweet_Num = NA)
      b  <- data.frame(Likes_Num = NA)
      c  <- data.frame(Name = NA)
      d  <- data.frame(Time_Stamp = NA)
      e  <- data.frame(Handle = NA)
      f <- data.frame(ID = Jake_MetaData3$ID[[i]])
      g <- data.frame(Error_Type = "Twitter Mobile type URL")
      h <- data.frame(Original_Link = Jake_MetaData3$Original.Link[[i]])
      j <- bind_cols(a,b,c,d,e,f,g,h)
      TweetData <- rbind(TweetData,j) %>% distinct()
      
      print("Twitter Mobile type") 
      
      next()
    }
    
    a <-as.data.frame(Jake_MetaData3$Original.Link[[i]])
    names(a)[1]<-"Lines"
    a$Lines <- as.character(a$Lines)
    a <- a %>% filter(grepl("twitter.com", a[,1])) 
    Tester.a <- which(grepl("twitter.com", a$Lines))
    
    b <-as.data.frame(Jake_MetaData3$Original.Link[[i]])
    names(b)[1]<-"Lines"
    b$Lines <- as.character(b$Lines)
    b <- b %>% filter(grepl("tweetdeck", b[,1])) 
    Tester.b <- which(grepl("tweetdeck", b$Lines))
    
    if ((length(Tester.a)) + (length(Tester.b)) == 1)
    {
      # i = 2
      
      # urldata <- scan(Jake_MetaData3$Original.Link[[i]], what="", sep="\n") 
      urldata <- tryCatch(scan(Jake_MetaData3$Original.Link[[i]], what="", sep="\n"), error=function(err) "Error 404") 
      if (urldata == "Error 404") {
        error.URLs.B <- Jake_MetaData3[i,]
        error.URLs.B$Error_Type <-"Error 404" 
        #NoTwitter.URLs$Error_Type <- NA
        NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.B) %>% distinct() 
        a  <- data.frame(Retweet_Num = NA)
        b  <- data.frame(Likes_Num = NA)
        c  <- data.frame(Name = NA)
        d  <- data.frame(Time_Stamp = NA)
        e  <- data.frame(Handle = NA)
        f <- data.frame(ID = Jake_MetaData3$ID[[i]])
        g <- data.frame(Error_Type = "Error 404")
        h <- data.frame(Original_Link = Jake_MetaData3$Original.Link[[i]])
        j <- bind_cols(a,b,c,d,e,f,g,h)
        TweetData <- rbind(TweetData,j) %>% distinct()
        
        print("Error 404") 
        
        next()
      } 
      ## Code for 'Retweets_Num'...
      a <- data.frame(Lines = urldata)
      a <- a %>% filter(grepl("strong", a[,1]))
      a <- a %>% filter(grepl("Retweet", a[,1]))
      
      Tester.a <- which(grepl("Retweet", a$Lines))
      if (length(Tester.a) == 0) # No 'Retweet'
      {
        a  <- data.frame(Retweet_Num = NA)
      } else 
      {
        spl <<-strsplit(as.character(a$Lines), ">")
        a <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                        , a, stringsAsFactors = F) 
        spl <<-strsplit(as.character(a$Temp.2), "<")
        a <- data.frame(Retweet_Num = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                        , a, stringsAsFactors = F) %>% select(1)    
      }
      # Code for 'Likes_Num'
      b <- data.frame(Lines = urldata)
      b <- b %>% filter(grepl("strong", b[,1]))
      b <- b %>% filter(grepl("Like", b[,1]))
      
      Tester.b <- which(grepl("Like", b$Lines))
      if (length(Tester.b) == 0) # No 'Likes'
      {
        b  <- data.frame(Likes_Num = NA)
      } else 
      {
        spl <<-strsplit(as.character(b$Lines), ">")
        b <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                        , b, stringsAsFactors = F) 
        spl <<-strsplit(as.character(b$Temp.2), "<")
        b <- data.frame(Likes_Num = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                        , b, stringsAsFactors = F) %>% select(1)
      }
      ## Code for 'Name'
      c <- data.frame(Lines = urldata)
      c <- c %>% filter(grepl("strong", c[,1]))
      c <- c %>% filter(grepl("fullname", c[,1])) 
      c$Lines <- c$Lines[[1]]  
      c <- c %>% distinct()
      
      Tester.c <- which(grepl("fullname", c$Lines))
      if (length(Tester.c) == 0) # No 'Name'
      {
        c  <- data.frame(Name = NA)
      } else 
      {
        spl <<-strsplit(as.character(c$Lines), ">")
        c <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                        , c, stringsAsFactors = F) 
        spl <<-strsplit(as.character(c$Temp.2), "<")
        c <- data.frame(Name = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                        , c, stringsAsFactors = F) %>% select(1)
      }
      ## Code for 'Time_Stamp'...
      d <- data.frame(Lines = urldata)
      d <- d %>% filter(grepl("/a", d[,1]))
      d <- d %>% filter(grepl("tweet-timestamp", d[,1])) %>% distinct() 
      d$Lines <- d$Lines[[1]]
      d <- d %>% distinct()
      
      Tester.d <- which(grepl("tweet-timestamp", d$Lines))
      if (length(Tester.d) == 0) # No 'Time_Stamp'
      {
        d  <- data.frame(Time_Stamp = NA)
      } else 
      {  
        spl <<-strsplit(as.character(d$Lines), "title=")
        d <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                        , d, stringsAsFactors = F) 
        spl <<-strsplit(as.character(d$Temp.2), ">")
        d <- data.frame(Time_Stamp = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                        , d, stringsAsFactors = F) %>% select(1) 
        d$Time_Stamp <- (gsub("\"", "", d$Time_Stamp))
      }
      # Code for 'Handle'
      e <- data.frame(Lines = urldata)
      e <- e %>% filter(grepl("/a", e[,1]))
      e <- e %>% filter(grepl("tweet-timestamp", e[,1])) %>% distinct() 
      e$Lines <- e$Lines[[1]]
      e <- e %>% distinct()
      
      Tester.e <- which(grepl("tweet-timestamp", e$Lines))
      if (length(Tester.e) == 0) # No 'Handle'
      {
        e  <- data.frame(Handle = NA)
      } else 
      { 
        spl <<-strsplit(as.character(e$Lines), "/")
        e <- data.frame(Temp.1 = sapply(spl, "[", 2)
                        , e, stringsAsFactors = F) 
        spl <<-strsplit(as.character(e$Temp.1), "/")
        e <- data.frame(Handle = sapply(spl, "[", 1)
                        , e, stringsAsFactors = F) %>% select(1) 
        e$atStamp <- "@"
        e <- e %>% mutate(Handle = paste0(atStamp,Handle)) %>% select(1)
      }
      # Joined
      f <- data.frame(ID = Jake_MetaData3$ID[[i]])
      g <- data.frame(Error_Type = "No Error")
      h <- data.frame(Original_Link = Jake_MetaData3$Original.Link[[i]])
      j <- bind_cols(a,b,c,d,e,f,g,h)
      TweetData <- rbind(TweetData,j) %>% distinct()
    } else 
    {    
      error.URLs.A <- Jake_MetaData3[i,]
      error.URLs.A$Error_Type <-"Not Twitter URL/Variety Error Type" 
      # NoTwitter.URLs <- rbind(error.URLs.A,error.URLs.A) %>% distinct()
      NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.A) %>% distinct() 
      
      a  <- data.frame(Retweet_Num = NA)
      b  <- data.frame(Likes_Num = NA)
      c  <- data.frame(Name = NA)
      d  <- data.frame(Time_Stamp = NA)
      e  <- data.frame(Handle = NA)
      f <- data.frame(ID = Jake_MetaData3$ID[[i]])
      g <- data.frame(Error_Type = "Not Twitter URL/Variety Error Type")
      h <- data.frame(Original_Link = Jake_MetaData3$Original.Link[[i]])
      j <- bind_cols(a,b,c,d,e,f,g,h)
      TweetData <- rbind(TweetData,j) %>% distinct()
      # TweetData <- rbind(i,i) %>% distinct()
      
      print("Not Twitter URL/Variety Error Type") 
      next()
    }
    
    print(i)
    
  }
  
  remove(NoTwitter.URLs)
  #remove(TweetData)
  #TweetData <- TweetData[1,]
  #TweetData1 <- TweetData
  #TweetData <- TweetData1
  write.csv(TweetData, "TweetData_C.csv", row.names = F)
} 
# Complete script for Jake_MetaData4...(for running the script in seperate Rstudio).
{
  library(dplyr)
  
  getwd()
  setwd("/Users/mitchell/Downloads")
  Jake_MetaData <- read.csv("metadata_withLinks.csv",stringsAsFactors = F)
  
  Jake_MetaData$Original.Link[[495]]

  Jake_MetaData4 <- Jake_MetaData %>% slice(2326:3100)
  
  for(i in 1:nrow(Jake_MetaData4)){
    #i = 2
    a <-as.data.frame(Jake_MetaData4$Original.Link[[i]])
    a <- a %>% filter(grepl("mobile.twitter", a[,1]))
    names(a)[1]<-"Lines"
    a <- which(grepl("mobile.twitter", a$Lines))
    if (length(a) == 1) 
    {
      error.URLs.C <- Jake_MetaData4[i,]
      error.URLs.C$Error_Type <-"Twitter Mobile type URL" 
      #NoTwitter.URLs$Error_Type <- NA
      NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.C) %>% distinct() 
      a  <- data.frame(Retweet_Num = NA)
      b  <- data.frame(Likes_Num = NA)
      c  <- data.frame(Name = NA)
      d  <- data.frame(Time_Stamp = NA)
      e  <- data.frame(Handle = NA)
      f <- data.frame(ID = Jake_MetaData4$ID[[i]])
      g <- data.frame(Error_Type = "Twitter Mobile type URL")
      h <- data.frame(Original_Link = Jake_MetaData4$Original.Link[[i]])
      j <- bind_cols(a,b,c,d,e,f,g,h)
      TweetData <- rbind(TweetData,j) %>% distinct()
      
      print("Twitter Mobile type") 
      
      next()
    }
    
    a <-as.data.frame(Jake_MetaData4$Original.Link[[i]])
    names(a)[1]<-"Lines"
    a$Lines <- as.character(a$Lines)
    a <- a %>% filter(grepl("twitter.com", a[,1])) 
    Tester.a <- which(grepl("twitter.com", a$Lines))
    
    b <-as.data.frame(Jake_MetaData4$Original.Link[[i]])
    names(b)[1]<-"Lines"
    b$Lines <- as.character(b$Lines)
    b <- b %>% filter(grepl("tweetdeck", b[,1])) 
    Tester.b <- which(grepl("tweetdeck", b$Lines))
    
    if ((length(Tester.a)) + (length(Tester.b)) == 1)
    {
      # i = 2
      
      # urldata <- scan(Jake_MetaData4$Original.Link[[i]], what="", sep="\n") 
      urldata <- tryCatch(scan(Jake_MetaData4$Original.Link[[i]], what="", sep="\n"), error=function(err) "Error 404") 
      if (urldata == "Error 404") {
        error.URLs.B <- Jake_MetaData4[i,]
        error.URLs.B$Error_Type <-"Error 404" 
        #NoTwitter.URLs$Error_Type <- NA
        NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.B) %>% distinct() 
        a  <- data.frame(Retweet_Num = NA)
        b  <- data.frame(Likes_Num = NA)
        c  <- data.frame(Name = NA)
        d  <- data.frame(Time_Stamp = NA)
        e  <- data.frame(Handle = NA)
        f <- data.frame(ID = Jake_MetaData4$ID[[i]])
        g <- data.frame(Error_Type = "Error 404")
        h <- data.frame(Original_Link = Jake_MetaData4$Original.Link[[i]])
        j <- bind_cols(a,b,c,d,e,f,g,h)
        TweetData <- rbind(TweetData,j) %>% distinct()
        
        print("Error 404") 
        
        next()
      } 
      ## Code for 'Retweets_Num'...
      a <- data.frame(Lines = urldata)
      a <- a %>% filter(grepl("strong", a[,1]))
      a <- a %>% filter(grepl("Retweet", a[,1]))
      
      Tester.a <- which(grepl("Retweet", a$Lines))
      if (length(Tester.a) == 0) # No 'Retweet'
      {
        a  <- data.frame(Retweet_Num = NA)
      } else 
      {
        spl <<-strsplit(as.character(a$Lines), ">")
        a <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                        , a, stringsAsFactors = F) 
        spl <<-strsplit(as.character(a$Temp.2), "<")
        a <- data.frame(Retweet_Num = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                        , a, stringsAsFactors = F) %>% select(1)    
      }
      # Code for 'Likes_Num'
      b <- data.frame(Lines = urldata)
      b <- b %>% filter(grepl("strong", b[,1]))
      b <- b %>% filter(grepl("Like", b[,1]))
      
      Tester.b <- which(grepl("Like", b$Lines))
      if (length(Tester.b) == 0) # No 'Likes'
      {
        b  <- data.frame(Likes_Num = NA)
      } else 
      {
        spl <<-strsplit(as.character(b$Lines), ">")
        b <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                        , b, stringsAsFactors = F) 
        spl <<-strsplit(as.character(b$Temp.2), "<")
        b <- data.frame(Likes_Num = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                        , b, stringsAsFactors = F) %>% select(1)
      }
      ## Code for 'Name'
      c <- data.frame(Lines = urldata)
      c <- c %>% filter(grepl("strong", c[,1]))
      c <- c %>% filter(grepl("fullname", c[,1])) 
      c$Lines <- c$Lines[[1]]  
      c <- c %>% distinct()
      
      Tester.c <- which(grepl("fullname", c$Lines))
      if (length(Tester.c) == 0) # No 'Name'
      {
        c  <- data.frame(Name = NA)
      } else 
      {
        spl <<-strsplit(as.character(c$Lines), ">")
        c <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                        , c, stringsAsFactors = F) 
        spl <<-strsplit(as.character(c$Temp.2), "<")
        c <- data.frame(Name = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                        , c, stringsAsFactors = F) %>% select(1)
      }
      ## Code for 'Time_Stamp'...
      d <- data.frame(Lines = urldata)
      d <- d %>% filter(grepl("/a", d[,1]))
      d <- d %>% filter(grepl("tweet-timestamp", d[,1])) %>% distinct() 
      d$Lines <- d$Lines[[1]]
      d <- d %>% distinct()
      
      Tester.d <- which(grepl("tweet-timestamp", d$Lines))
      if (length(Tester.d) == 0) # No 'Time_Stamp'
      {
        d  <- data.frame(Time_Stamp = NA)
      } else 
      {  
        spl <<-strsplit(as.character(d$Lines), "title=")
        d <- data.frame(Temp.1 = sapply(spl, "[", 1), Temp.2 = sapply(spl, "[", 2)
                        , d, stringsAsFactors = F) 
        spl <<-strsplit(as.character(d$Temp.2), ">")
        d <- data.frame(Time_Stamp = sapply(spl, "[", 1), Temp.3 = sapply(spl, "[", 2)
                        , d, stringsAsFactors = F) %>% select(1) 
        d$Time_Stamp <- (gsub("\"", "", d$Time_Stamp))
      }
      # Code for 'Handle'
      e <- data.frame(Lines = urldata)
      e <- e %>% filter(grepl("/a", e[,1]))
      e <- e %>% filter(grepl("tweet-timestamp", e[,1])) %>% distinct() 
      e$Lines <- e$Lines[[1]]
      e <- e %>% distinct()
      
      Tester.e <- which(grepl("tweet-timestamp", e$Lines))
      if (length(Tester.e) == 0) # No 'Handle'
      {
        e  <- data.frame(Handle = NA)
      } else 
      { 
        spl <<-strsplit(as.character(e$Lines), "/")
        e <- data.frame(Temp.1 = sapply(spl, "[", 2)
                        , e, stringsAsFactors = F) 
        spl <<-strsplit(as.character(e$Temp.1), "/")
        e <- data.frame(Handle = sapply(spl, "[", 1)
                        , e, stringsAsFactors = F) %>% select(1) 
        e$atStamp <- "@"
        e <- e %>% mutate(Handle = paste0(atStamp,Handle)) %>% select(1)
      }
      # Joined
      f <- data.frame(ID = Jake_MetaData4$ID[[i]])
      g <- data.frame(Error_Type = "No Error")
      h <- data.frame(Original_Link = Jake_MetaData4$Original.Link[[i]])
      j <- bind_cols(a,b,c,d,e,f,g,h)
      TweetData <- rbind(TweetData,j) %>% distinct()
    } else 
    {    
      error.URLs.A <- Jake_MetaData4[i,]
      error.URLs.A$Error_Type <-"Not Twitter URL/Variety Error Type" 
      # NoTwitter.URLs <- rbind(error.URLs.A,error.URLs.A) %>% distinct()
      NoTwitter.URLs <- rbind(NoTwitter.URLs,error.URLs.A) %>% distinct() 
      
      a  <- data.frame(Retweet_Num = NA)
      b  <- data.frame(Likes_Num = NA)
      c  <- data.frame(Name = NA)
      d  <- data.frame(Time_Stamp = NA)
      e  <- data.frame(Handle = NA)
      f <- data.frame(ID = Jake_MetaData4$ID[[i]])
      g <- data.frame(Error_Type = "Not Twitter URL/Variety Error Type")
      h <- data.frame(Original_Link = Jake_MetaData4$Original.Link[[i]])
      j <- bind_cols(a,b,c,d,e,f,g,h)
      TweetData <- rbind(TweetData,j) %>% distinct()
      # TweetData <- rbind(i,i) %>% distinct()
      
      print("Not Twitter URL/Variety Error Type") 
      next()
    }
    
    print(i)
    
  }
  
  remove(NoTwitter.URLs)
  #remove(TweetData)
  #TweetData <- TweetData[1,]
  #TweetData1 <- TweetData
  #TweetData <- TweetData1
  write.csv(TweetData, "TweetData_D.csv", row.names = F)
} 

