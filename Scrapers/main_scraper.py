from Reddit_scraper import cred_r
from Twitter_scraper import cred_t
from Reddit_scraper import reddit_csv as reddit
from Twitter_scraper import twitter_csv as twitter
from Google_scraper import google_csv as google

def main():
    # Topic of interest
    print("")
    topic = input("Enter topic of interest:").lower()
    
    # Hashtag format for Twitter
    topic_is_hashtag = input("Is the topic also the hashtag? (y/n):") 
    if topic_is_hashtag.lower() == "y" or topic_is_hashtag.lower() == "yes": 
        hashtag = "#" + topic.replace(" ", "").lower()
    else:
        hashtag = input("Enter hashtag:").lower()

    # Subreddit
    subreddit = input("Enter subreddit:").lower()
    subreddit = [subreddit.replace(" ", "")]
    # print((topic, hashtag, subreddit))
    # print(subreddit)
    reddit(subreddit)
    twitter(hashtag)
    google(topic)

    

if __name__ == "__main__":
    main()