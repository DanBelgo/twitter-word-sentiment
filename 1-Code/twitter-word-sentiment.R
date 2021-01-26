#####Loading Packages#####
#
library(rtweet)
library(tidyverse)
library(tidytext)
library(data.table)
library(magrittr)
library(syuzhet)
library(lubridate)
library(waffle)
library(here)

#####Setting up credentials#####

appname = "appname"
key = "key"
secret = "secret_key"
access_token = "access_token"
access_secret = "access_secret"


twitter_token = create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

#####Extracting the tweets

keyword = "rstats" 

tweets = search_tweets(q = keyword,
                       n = 10000,
                       include_rts = F,
                       retryonratelimit = T) 

####Cleaning the data

clean_urls = function(x) {  #Deletes URLs and hashtags from tweets
  x = gsub("http.*","", x)
  x =  gsub("https.*","", x) 
  x = gsub("#.*","", x)
  return(x) 
}

tweets$text = clean_urls(tweets$text) 
tweets$text <- str_replace_all(tweets$text, "@[a-z,A-Z]*","") #deletes handles from tweets

#Adding time as a variable
tweets$created_at = as_datetime(tweets$created_at)
tweets$created_at = ymd_hms(tweets$created_at)
tweets$created_day = floor_date(tweets$created_at, unit = "day") #day where the tweet has been posted


tweets = tweets %>% #we delete duplicated tweets
  distinct(text, .keep_all =T)

####Sentiment analysis
word.df = as.vector(tweets$text)
emotion.df = get_nrc_sentiment(word.df)

eps = 0.00000000001 #this will come handy when variables equal 0

emotion.df2 = cbind(tweets$created_day, emotion.df) %>%  #joins the date+hour column into the dataframe
  mutate(date = tweets$created_day, ratio = (positive+eps)/(negative+eps)) %>%  #makes the positive/negative ratio
  select(-("tweets$created_day")) 

###Data wrangling

eps = 0.00000000001 #epsilon

filtered.df = emotion.df2 %>% 
  
    filter(!(anger == 0 & #Filter out observations that hold no information
           anticipation == 0 & 
           disgust == 0 &
           fear == 0 &
           joy == 0 &
           sadness == 0 &
           surprise == 0 &
           trust == 0 &
           negative == 0 &
           positive == 0)) %>% 
  
  mutate(veredict = ifelse(ratio>1, "Positive", #classify observations based on the positive/negative ratio
                           ifelse(ratio == 1, "Neutral", "Negative")
                           )
         )

balance = filtered.df %>% count(date, veredict) #counts how many tweets are positive, negative or neutral

#Split and join
positive = balance %>% filter(veredict == "Positive")
negative = balance %>% filter(veredict == "Negative")
neutral = balance %>% filter(veredict == "Neutral")


df.final = positive %>% 
  left_join(negative, by = "date") %>% #we join the splitted datasets into one by date
  left_join(neutral, by = "date") %>% 
  
  drop_na() %>% #If a day has 0 +/-/= tweets in any category, it creates NAs when we join the dfs. 
                #Next version: Add the missing categories to days which lack them, with n = 0
  mutate(positive_n = n.x, #changing names
         negative_n = n.y, 
         neutral_n = n) %>% 
  
  mutate(total_n = positive_n + negative_n + n) %>% #count total number of tweets per day
  select(date, positive_n, negative_n, neutral_n, total_n) %>% 
  
  mutate(positive_ratio = ((positive_n+eps)/total_n)*100, #create ratio of positive/negative/neutral tweets over total
         negative_ratio = ((negative_n+eps)/total_n)*100,
         neutral_ratio = ((neutral_n+eps)/total_n)*100) %>% 
  
  select(date, positive_ratio, neutral_ratio, negative_ratio) 

df.final = slice_tail(df.final, n=1) #gets the last row only. Eliminate this line to get a time series

##Visualizing the ratios

#To automatize the titles and legend of the waffle chart, we set them as variables

positive_string = as.character(paste("Positive Tweets =", as.character(signif(df.final$positive_ratio,4))) %>% 
  paste("%", sep = ""))

negative_string = paste("Negative Tweets =", as.character(signif(df.final$negative_ratio,4))) %>% 
  paste("%", sep = "")

neutral_string = paste("Neutral Tweets =", as.character(signif(df.final$neutral_ratio,4))) %>% 
  paste("%", sep = "")

keyword_string = paste("Word:", keyword, "|", df.final$date)

parts = c("Positive Tweets = " = signif(df.final$positive_ratio,4),
          "Neutral Tweets = " = signif(df.final$neutral_ratio,4),
          "Negative Tweets = " = signif(df.final$negative_ratio,4))

names(parts) = paste(names(parts), parts, "%", sep = "")

###We'll create a jpg file with the output waffle chart

jpeg(here("2-Graphic Outputs", "waffleexample.jpg"), width = 700, height = 350, quality = 150)

waffle(parts,
       rows = 10, 
       colors = c("green", "lightgrey", "red"), 
       title = keyword_string,
       flip = T, 
       reverse = T)

dev.off()

