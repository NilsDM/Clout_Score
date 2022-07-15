# Set up
library(reticulate)

################################################################################
# use_python("C:/ProgramData/Anaconda3/python.exe")
# Install dependencies
# py_install("pytrends")
# py_install("praw")
# py_install("tweepy")
# py_install("urllib3")

################################################################################
# Load dependencies
import("pytrends")
import("praw")
import("tweepy")

# API keys
py_run_string("import cred_t")
py_run_string("import cred_r")

# Scrapers (Download data)
source_python("../Scrapers/Google_scraper.py")
source_python("../Scrapers/Reddit_scraper.py")
source_python("../Scrapers/Twitter_scraper.py")

# Construct data frames, clean data, calculate sentiment
source("Data_Cleaning_and_Sentiment.R")

# Data visualization
source("Data_Viz.R")


