import pytrends
from pytrends.request import TrendReq

def google_csv(topic):
    kw_list = [topic]
    pytrends = TrendReq(hl='en-US', tz=360)
    pytrends.build_payload(kw_list, cat=0, timeframe='all', geo='', gprop='')
    trend_df = pytrends.interest_over_time() 
    trend_df.to_csv("google_pull.csv")


def main():
    google_csv("The Office")



if __name__ == "__main__":
    main()