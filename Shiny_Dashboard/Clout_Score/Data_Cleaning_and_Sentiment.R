
# Libraries
library(tidyr) 
library(dplyr) 
library(furrr)
library(tibble)
library(forcats)
library(stringi)
library(stringr) 
library(syuzhet)



# Data loading
google  <- read.csv("google_pull.csv", encoding = "UTF-8")
twitter <- read.csv("twitter_pull.csv", encoding = "UTF-8")
reddit  <- read.csv("reddit_pull.csv", encoding = "UTF-8")

# Emoji Data
json_data <-
    rjson::fromJSON(
        paste0(
            readLines("data-by-emoji.json", warn=FALSE), collapse = ""))
emoji_to_text <- json_data %>% unlist()
emoji_to_text <- emoji_to_text %>% as.data.frame()
emoji_to_text$emoji <- rownames(emoji_to_text)
rownames(emoji_to_text) <- NULL
emoji_to_text <- emoji_to_text %>% 
    mutate(emoji = ifelse(str_detect(emoji, ".name"), emoji, NA),
           emoji = str_replace(emoji,"\\.name", "")) %>% 
    drop_na()
names(emoji_to_text) <- c("text", "emoji")

################################################################################
# Functions
################################################################################

# Emoji function
emoji_conversion <- function(e){
    e <- e %>% unlist()
    len <- length(e)
    result <- rep("",len)
    counter <- 1
    for(i in e){
        if(i %in% emoji_to_text$emoji){
            index <- which(emoji_to_text$emoji == i)
            result[counter] <- emoji_to_text[index, 1]
        }
        counter = counter + 1
    }
    return(gsub(",","", toString(result)))
}

# Sentiment Calculation
get_sentiment <- function(comment){
    sent_score <- get_nrc_sentiment(comment)
    num_score <- (sent_score$positive - sent_score$negative) / (sent_score$positive + sent_score$negative)
    if(is.nan(num_score)){
        return(0)
    }
    return(num_score)
}

################################################################################
# Google
################################################################################

# Update date
google <- google %>% mutate(date = as.Date(date))
names(google) <- c("date", "score", "is_partial")

# Extract scores as vector
google_vals <- google[[2]]

# Score metrics
google_median_all <- google_vals %>% median()
google_mean_all   <- google_vals %>% mean()

google_median_last_12 <- google_vals %>% tail(12) %>% median()
google_mean_last_12   <- google_vals %>% tail(12) %>% mean()

google_scores <- c(google_mean_all, google_mean_last_12,
                   google_median_all, google_median_last_12) %>% round(2)

################################################################################
# Twitter
################################################################################

# Data Cleaning & Sentiment Scoring
twitter <- 
    twitter %>% mutate(text = gsub("@\\w+ *","", text),                        # removes @'s
                       text = gsub("#\\w+ *","", text),                        # remove hashtag mentions
                       text = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+","", text), # remove links 1
                       text = gsub("/\\w+ *","", text),                        # remove links 2
                       emojis = stri_extract_all_charclass(text, '\\p{EMOJI}'),# emoji extraction 
                       emojis = ifelse(lengths(emojis) == 0, NA, emojis),      # NA conversion
                       emoji_text = lapply(emojis, emoji_conversion),          # emoji to text conversion
                       text = gsub("[^[:alnum:][:space:]']", " ", text),       # remove special characters
                       text = gsub(" at | gt ", " ", text),                    # cleaning 1
                       text = gsub(" amp ", " and ", text),                    # cleaning 2
                       text = gsub("[\r\n]", "", text),                        # remove \n characters
                       full_text = paste0(text, " ", emoji_text) %>% tolower(),# concatenate results
                       sentiment = future_map(full_text, get_sentiment) %>% unlist) # get sentiment

################################################################################
# Reddit
################################################################################

# Data Cleaning & Sentiment Scoring
reddit <- 
    reddit %>% 
        mutate(comment = gsub("@\\w+ *","", comment),                        # removes @'s
               comment = gsub("#\\w+ *","", comment),                        # remove hashtag mentions
               comment = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+","", comment), # remove links 1
               comment = gsub("/\\w+ *","", comment),                        # remove links 2
               emojis = stri_extract_all_charclass(comment, '\\p{EMOJI}'),   # emoji extraction 
               emojis = ifelse(lengths(emojis) == 0, NA, emojis),            # NA conversion
               emoji_comment = lapply(emojis, emoji_conversion),             # emoji to text conversion
               comment = gsub("[^[:alnum:][:space:]']", " ", comment),       # remove special characters
               comment = gsub(" at | gt ", " ", comment),                    # cleaning 1
               comment = gsub(" amp ", " and ", comment),                    # cleaning 2
               comment = gsub("[\r\n]", "", comment),                        # remove \n characters
               full_text = paste0(comment, " ", emoji_comment) %>% tolower(),# concatenate results
               sentiment = future_map(full_text, get_sentiment) %>% unlist)  # get sentiment

# reddit <- 
    reddit %>% 
    mutate(comment = gsub("@\\w+ *","", comment),                        # removes @'s
           comment = gsub("#\\w+ *","", comment),                        # remove hashtag mentions
           comment = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+","", comment), # remove links 1
           comment = gsub("/\\w+ *","", comment),                        # remove links 2
           emojis = stri_extract_all_charclass(comment, '\\p{EMOJI}'),   # emoji extraction 
           emojis = ifelse(lengths(emojis) == 0, NA, emojis),            # NA conversion
           emoji_comment = lapply(emojis, emoji_conversion),             # emoji to text conversion
           comment = gsub("[^[:alnum:][:space:]']", " ", comment),       # remove special characters
           comment = gsub(" at | gt ", " ", comment),                    # cleaning 1
           comment = gsub(" amp ", " and ", comment),                    # cleaning 2
           comment = gsub("[\r\n]", "", comment),                        # remove \n characters
           full_text = paste0(comment, " ", emoji_comment) %>% tolower()) %>%  # concatenate results
    filter(grepl(tolower("pam"), full_text))  %>%                 # filter based on subtopic
    mutate(full_text = full_text %>% tolower()) %>% 
    mutate(sentiment = future_map(full_text, get_sentiment) %>% unlist)     # get sentiment
    
