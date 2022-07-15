import pytrends
from pytrends.request import TrendReq

def google_csv(topic, end):
    print("Google topic {}".format(topic))
    kw_list = [topic]
    pytrends = TrendReq(hl='en-US', tz=360)
    pytrends.build_payload(kw_list, cat=0, timeframe='all', geo='', gprop='')
    trend_df = pytrends.interest_over_time() 
    csv_name = "google_pull_" + end + ".csv"
    trend_df.to_csv(csv_name)
    # print("Google Successful")


# def main():
#     google_csv("The Office")



# if __name__ == "__main__":
#     main()