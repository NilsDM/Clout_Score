# Libraries
library(tidyr)
library(dplyr)  
library(tibble)


data = read.csv("reddit_sublist_master.csv", encoding = "UTF-8")

data_out <- 
    data %>% 
    select(real_name, subs) %>%
    arrange(desc(subs)) %>% 
    head(10000)

data_out %>% view()
write.csv(data_out, "reddit_sublist_top10k.csv")

data2 = read.csv("reddit_sublist_top10k.csv", encoding = "UTF-8")
data2[[2]] %>% unlist()


data_out2 <- 
    data2 %>% 
    select(real_name, subs) %>%
    arrange(desc(subs)) %>% 
    head(500)

data_out2 %>% view()
write.csv(data_out2, "reddit_sublist_top500.csv")
