# Libraries
library(tidyr) 
library(dplyr) 
library(tibble) 
library(forcats)
library(stringi)
library(stringr) 
library(lexicon)


# Data loading
google  <- read.csv("../Scrapers/google_pull.csv", encoding = "UTF-8")
twitter <- read.csv("../Scrapers/twitter_pull.csv", encoding = "UTF-8")
reddit  <- read.csv("../Scrapers/reddit_pull.csv", encoding = "UTF-8")
data(emojis_sentiment)
emojis_sentiment
################################################################################
# Google
################################################################################

# Update date
google <- google %>% mutate(date = as.Date(date))

# Extract scores as vector
google_vals <- google[[2]]

# Score metrics
google_median_all <- google_vals %>% median()
google_mean_all   <- google_vals %>% mean()

google_median_last_12 <- google_vals %>% tail(12) %>% median()
google_mean_last_12   <- google_vals %>% tail(12) %>% mean()

google_scores <- c(google_median_all, google_mean_last_12,
                   google_median_last_12, google_mean_last_12) %>% round(2)

################################################################################
# Twitter
################################################################################

# Clean tweet bodies PT 1
twitter <-
    twitter %>% mutate(text = iconv(text, "UTF-8", "ASCII", "byte"),           # convert emoji's
                       text = gsub("@\\w+ *","", text),                        # removes @'s
                       text = gsub("#\\w+ *","", text),                        # remove hashtag mentions
                       text = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+","", text), # remove links 1
                       text = gsub("/\\w+ *","", text),                        # remove links 2 
                       text_emoji = str_extract_all(text, "(<\\w\\d>|<\\d\\w>|<\\w\\w>|<\\d\\d>){4}"), # Extract emoji's
                       text = gsub("(<\\w\\d>|<\\d\\w>|<\\w\\w>|<\\d\\d>){1,4}","", text),             # remove emoji's from tweet 
                       text_emoji = ifelse(lengths(text_emoji) == 0, NA, text_emoji), # NA conversion        
                       text = gsub("[^[:alnum:][:space:]']", " ", text), # remove special characters
                       text = gsub(" at | gt ", " ", text), 
                       text = gsub(" amp ", " and ", text)) %>% view()

# Emoji conversion
emoji_bytes <- twitter[10]  %>% as.data.frame()
emoji_bytes <- lapply(twitter[10] , unlist)
emoji_bytes <- emojis %>% as.data.frame() %>% drop_na() 
names(emoji_bytes) <- c("bytes")

# Emoji data frame with sentiment scores
emoji_sentiment_scores <- 
    emoji_bytes %>% 
    left_join(emojis_sentiment, by = c("bytes" = "byte")) %>% 
    drop_na() %>% 
    select(bytes, sentiment)

emoji_sentiment_scores %>% view()
twitter %>% view()
emoji_sentiment_scores[1]

emoji_conversion <- function(emoji_sentiment_scores, df){
    for(i in 1:500){
        number_of_emojis <- df[[i,11]]
        for(j in number_of_emojis){
            if(j != 0){
                if(df[[i,10]]  %in% emoji_sentiment_scores[[1]]){
                    print("yes") ############### Continue from here
                }
            }
        }
    }
}

emoji_conversion(emoji_sentiment_scores, twitter)

twitter <- 
    twitter %>% 
    mutate(number_of_emojis  = ifelse(is.na(text_emoji), 0, lengths(text_emoji)))#,
           # emoji_score = lapply(text_emoji, function(emoji_sentiment_scores){
           #     if(text_emoji %in% emoji_sentiment_scores[[1]]){
           #         return("yes")
           #     }
           # }))


################################################################################
# Reddit
################################################################################

    


