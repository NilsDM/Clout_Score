import cred_t
import tweepy
from csv import writer


def twitter_csv(hash):
    print("Twitter topic {}".format(hash))
    # Load credentials
    auth = tweepy.OAuthHandler(cred_t.ct[0], cred_t.ct[1])
    auth.set_access_token(cred_t.ct[3], cred_t.ct[4])

    # API connection
    twitter = tweepy.API(auth)

    column_names = ['username',
                    'description',
                    'location',
                    'following',
                    'followers',
                    'totaltweets',
                    'retweetcount',
                    'text',
                    'hashtags']

    # Construct CSV output
    with open('twitter_pull.csv', 'w', encoding='utf-8', newline='') as f:
        
        csv_writer = writer(f) 
        csv_writer.writerow(column_names)
        # twitter.search_30_day("Clout_Score", hashes)

        # Adapted from: https://www.geeksforgeeks.org/extracting-tweets-containing-a-particular-hashtag-using-python/
        tweets = tweepy.Cursor(twitter.search_tweets,
                               hash, lang="en",
                               since_id="2022-06-27",
                               tweet_mode='extended').items(500)
        
        list_tweets = [tweet for tweet in tweets]
        for tweet in list_tweets:
            username = tweet.user.screen_name
            description = tweet.user.description
            location = tweet.user.location
            following = tweet.user.friends_count
            followers = tweet.user.followers_count
            totaltweets = tweet.user.statuses_count
            retweetcount = tweet.retweet_count
            hashtags = tweet.entities['hashtags']

            # Retweets can be distinguished by
            # a retweeted_status attribute,
            # in case it is an invalid reference,
            # except block will be executed
            try:
                    text = tweet.retweeted_status.full_text
            except AttributeError:
                    text = tweet.full_text
            hashtext = list()
            for j in range(0, len(hashtags)):
                    hashtext.append(hashtags[j]['text'])

            # Here we are appending all the
            # extracted information in the DataFrame
            ith_tweet = [username, description,
                            location, following,
                            followers, totaltweets,
                            retweetcount, text, hashtext]
            csv_writer.writerow(ith_tweet)
    print("Twitter successful")

# def main():
#     pass
#     twitter_csv("#theoffice")
#     print("okay")


# if __name__ == "__main__":
#     main()


    