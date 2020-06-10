library(tweetbotornot2)
library(httr)
library(xml2)
library(RJSONIO)
library(rtweet)
library(ROAuth)
library(readr)
library(tidyverse)
library(glue)


api_key <- "ph1qD0fK1RctmqVjdfSevUz7c"
api_secret_key <- "QhxOwDMTWlKtukK1D1oVZz7tLWpd2eSiFZOSWTJcjyk1UFCsBQ"
access_token <- "1232369870139949063-ugXZpGqIujBNGr7850sPHL0vZfhmNP"
access_token_secret <- "EZXrhNJEZCcDXCeIYJFChFwe8Ph4lgoMzmNsgoE6UuE8X"

## authenticate via web browser
token <- create_token(
  app = "rstatsjournalismresearch",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

consumer_key = api_key
consumer_secret = api_secret_key
access_token = access_token
access_secret = access_token_secret

Mashape_key = "3894f0aad4msh55bd164e9c79cc3p1e9a19jsn73b086d9e4d4"

myapp = oauth_app("twitter", key=consumer_key, secret=consumer_secret)
sig = sign_oauth1.0(myapp, token=access_token, token_secret=access_secret)

devtools::install_github("mkearney/tweetbotornot2")

newdata <- read.csv("/Users/vaishnaviiparamashivam/Downloads/Consolidated_MedicalCannabisTweets.csv")

#str(newdata1)


names(newdata)[names(newdata)=="Username"] <- "screen_name"

newdata1 <- newdata[1:5,]

#str(newdata1)

users <- as.vector(newdata$screen_name)
users


n=100

get_timeline_unlimited <- function(users, n){
  
  if (length(users) ==0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline")
  
  if (length(users) <= rl$remaining){
    print(glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + 0.1
    print(glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}

tmls=get_timeline_unlimited(users,100)
dat=tmls
data <- predict_bot(tmls)

#tweetbotornot returns screenname and the probability of it being a bot
#data <- tweetbotornot(users)
data=as.data.frame(data)

write.csv(data,"/Users/vaishnaviiparamashivam/Documents/594/proj/Extracted/initialbotnot.csv", row.names = FALSE)
str(data)
#head(data)

#perform outerjoin as dfs are diff in lengths

full_join_df=full_join(newdata, data, by = c("screen_name"))

Outer_join_df1 =merge(x = newdata, y = data,by=c("screen_name"), all = TRUE)


Outer_join_df1$Bot_or_Not= ifelse(Outer_join_df1$prob_bot > 0.5 ,"Bot","Not a Bot")

full_join_df$Bot_or_Not= ifelse(full_join_df$prob_bot > 0.5 ,"Bot","Not a Bot")


head(full_join_df)

head(Outer_join_df1)

write.csv(Outer_join_df1,"/Users/vaishnaviiparamashivam/Documents/594/proj/Extracted/BotOrNot1.csv", row.names = FALSE)

write.csv(Outer_join_df1,"/Users/vaishnaviiparamashivam/Documents/594/proj/Extracted/BotOrNot2.csv", row.names = FALSE)



