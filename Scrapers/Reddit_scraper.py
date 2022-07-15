#
# Scrapes reddit for metadata and outputs CSV file
#

import praw
import cred_r
from csv import writer

def reddit_csv(s):
    print("reddit topic {}".format(s))
    # API Connection
    reddit = praw.Reddit(client_id=cred_r.c[0],
                         client_secret=cred_r.c[1],
                         user_agent=cred_r.c[2])

    num_of_posts = 10

    column_names = ["subreddit",
                    "post_name",
                    "author_name",
                    "time_created",
                    "num_comments",
                    "karma",
                    "karma_ratio",
                    "comment_author",
                    "comment",
                    "comment_time",
                    "comment_karma"]

    # Construct CSV output
    with open('reddit_pull.csv', 'w', encoding='utf-8', newline='') as f:
        
        csv_writer = writer(f) 
        csv_writer.writerow(column_names)

        posts = reddit.subreddit(s).hot(limit=num_of_posts) # grab top n posts
        
        for p in posts:
            # Skip over stickied posts
            if p.stickied == False:
                submission = reddit.submission(id=p.id)  # get comments
                submission.comments.replace_more(limit=0)  # removes all more comments/Continue buttons
                for c in submission.comments.list():  # for every comment in post 
                    try: # Exception for deleted comments
                        current_row = [s,
                                submission.title,
                                submission.author.name,
                                submission.created_utc,
                                submission.num_comments,
                                submission.score,
                                submission.upvote_ratio,
                                c.author,
                                c.body,
                                c.created_utc,
                                c.score]
                        csv_writer.writerow(current_row)
                    except AttributeError:  
                        continue
    print("Reddit Successful")
    # return csv_writer


# main() for testing
# def main():
#     result = reddit_csv(["DunderMifflin"])
#     print("okay") # Completion test


# if __name__ == "__main__":
#     main()