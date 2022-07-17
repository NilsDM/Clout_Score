# Libraries
library(DT)
library(bslib)
library(shiny)
library(tidyr) 
library(dplyr) 
library(furrr)
library(tibble) 
library(plotly)
library(waiter)
library(forcats)
library(stringi)
library(stringr) 
library(syuzhet)
library(ggplot2)
library(forcats)
library(cowplot)
library(showtext)
library(gridExtra)
library(reticulate)
library(shinydashboard) 

################################################################################
# Functions
################################################################################

# Theme
load_theme <- function(){
    # Add fonts from Google.
    font_add_google("Roboto Mono", "Roboto Mono")
    font_add_google("Open Sans", "Open Sans")
    font_add_google("Special Elite", "Special Elite")
    
    # Set ggplot theme
    theme_set(theme_minimal(base_family = "Roboto Mono"))
    theme_update(
        plot.background = element_rect(fill = "#fafaf5", color = "#fafaf5"),
        panel.background = element_rect(fill = NA, color = NA),
        panel.border = element_rect(fill = NA, color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 13, margin = margin(r = 10)),
        legend.title = element_text(size = 9),
        plot.caption = element_text(
            family = "Special Elite",
            size = 10,
            color = "grey70",
            face = "bold",
            hjust = .5,
            margin = margin(5, 0, 20, 0)
        ),
        plot.margin = margin(10, 25, 10, 25)
    )
    
    my_theme <<- theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                                  hjust=1, size = 16, face = "bold"),
                       axis.title.x  = element_text(size = 14, face = "bold", vjust = 3),
                       axis.title.y  = element_text(size = 14, face = "bold"),
                       plot.subtitle = element_text(size = 16),
                       title = element_text(size = 16),
                       text  = element_text(size = 16),
                       legend.title  = element_text(size = 14),
                       legend.text   = element_text(size = 14), 
                       plot.caption = element_text(size = 16)) 
    
    # Turn on showtext
    showtext_auto()
}


################################################################################
# Shiny App
################################################################################

ui <- dashboardPage(
    
    # Application title
    header <- dashboardHeader(title = "Clout Score"), 
    
    # Sidebar
    dashboardSidebar(disable = TRUE),
    
    
    # Text input
    dashboardBody(
        box(
            # Status Bar
            textOutput("status bar"),
            verbatimTextOutput("status"),
            
            textInput("google_search", "Google Search", "", 
                      placeholder = "Enter Google Search Here E.g. The Office"),
            verbatimTextOutput("g_search"),
            
            textInput("twitter_hash", "Twitter Hashtag", "", 
                      placeholder = "Enter Twitter hashtag E.g. TheOffice"),
            verbatimTextOutput("t_hash"),
            
            textInput("reddit_sub", "Reddit Subreddit", "", 
                      placeholder = "Enter subreddit name E.g. DunderMifflin"),
            verbatimTextOutput("r_sub"), 
            
            # Spinners package
            waiter::use_waiter(),
            
            # Download data button
            actionButton("input_action", "Download Data"),
            
            # Subtopic entry
            textInput("sub_topic", "Sub Topic", "", 
                      placeholder = "Enter related subtopic E.g. Pam"),
            verbatimTextOutput("s_topic"),
            
            # Analyse subtopic button     
            actionButton("input_action3", "Show/Update Results"),
            
        # Text input width
        width = 2),
        
        
        # Boxplots
        box(plotOutput("Box"), width = 4),
        
        # Bar plot metrics
        box(plotOutput("Scores"), width = 4),
        
        # Data Table
        box(dataTableOutput("gtrendstable"), width = 2),
        
        # Google trends
        box(plotOutput("Trend1"), width = 5),
        box(plotOutput("Trend2"), width = 5),
        
    )
)

################################################################################

server <- function(input, output) {
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
        num_score <- (sent_score$positive - sent_score$negative) / 
            (sent_score$positive + sent_score$negative)
        if(is.nan(num_score)){
            return(0)
        }
        return(num_score)
    }
    
    output$status <- renderText("Status: Ready")
    # Python import statements
    import("praw")
    import("tweepy")
    import("pytrends")
    
    # API keys
    py_run_string("import cred_r")
    py_run_string("import cred_t")
    
    # Scraper scripts
    source_python("../../Scrapers/Reddit_scraper.py")
    source_python("../../Scrapers/Twitter_scraper.py")
    source_python("../../Scrapers/Google_scraper.py")
    
    # Reactive text entry
    gs <- reactive(paste0(input$google_search))
    th <- reactive(paste0("#",input$twitter_hash))
    rs <- reactive(paste0("r/",input$reddit_sub))
    st <- reactive(input$sub_topic)
    
    # Display text entries
    output$g_search <- renderText(gs())
    output$t_hash <- renderText(th())
    output$r_sub <- renderText(rs())
    output$s_topic <- renderText(st())
    
    # Plot Theme
    load_theme()
    
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
    
    observeEvent(input$input_action,{
        output$status <- renderText("Status: Downloading")
    })
    
    
    # Download Data Action
    observeEvent(input$input_action,{
        
        # Spinner
        waiter <- waiter::Waiter$new(id = "status", html = spin_chasing_dots())
        waiter$show()
        on.exit(waiter$hide())
        
        # Reddit
        message("Downloading Reddit Data…")
        reddit_csv(input$reddit_sub)
        message("complete")
        
        # Twitter
        message("Downloading Twitter Data…")
        twitter_csv(input$twitter_hash)
        message("complete")
        
        # Google
        message("Downloading Google Data…")
        google_csv(input$google_search, "main")
        message("complete")
        
        output$status <- renderText("Status: Download Complete")
    })
    
    reddit_fc <- reactive({
        reddit <- read.csv("reddit_pull.csv", encoding = "UTF-8")
        reddit_master_length <<- (reddit %>% lengths())[[1]]
        
        if(is.null(input$sub_topic)){
            filter_search <- input$google_search.
        } else {
            filter_search <- input$sub_topic
        }
        
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
                   full_text = paste0(comment, " ", emoji_comment) %>% tolower()) %>%  # concatenate results
            filter(grepl(tolower(filter_search), full_text))  %>%                 # filter based on subtopic
            mutate(full_text = full_text %>% tolower()) %>% 
            mutate(sentiment = future_map(full_text, get_sentiment) %>% unlist)     # get sentiment
        
    })
    
    twitter_fc <- reactive({
        twitter <- read.csv("twitter_pull.csv", encoding = "UTF-8")
        twitter_master_length <<- 500
        if(is.null(input$sub_topic)){
            filter_search <- input$google_search.
        } else {
            filter_search <- input$sub_topic
        }

        
        twitter <-
            twitter %>% 
            mutate(text = gsub("@\\w+ *","", text),                        # removes @'s
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
                   full_text = paste0(text, " ", emoji_text) %>% tolower()) %>% # concatenate results
            filter(grepl(tolower(filter_search), full_text))  %>%             # filter based on subtopic
            mutate(full_text = full_text %>% tolower()) %>% 
            mutate(sentiment = future_map(full_text, get_sentiment) %>% unlist)  # get sentiment
    })

    
    out_g <- eventReactive(input$input_action3,{
        # Spinner
        waiter0 <- waiter::Waiter$new(id = "Trend1", html = spin_gauge())
        waiter0$show()
        waiter1 <- waiter::Waiter$new(id = "Trend2", html = spin_pong())
        waiter1$show()
        waiter2 <- waiter::Waiter$new(id = "Box", html = spin_3circles())
        waiter2$show()
        waiter3 <- waiter::Waiter$new(id = "Scores", html = spin_inner_circles())
        waiter3$show()
        waiter4 <- waiter::Waiter$new(id = "gtrendstable", html = spin_wobblebar())
        waiter4$show()
        on.exit(waiter0$hide())
        on.exit(waiter1$hide())
        on.exit(waiter2$hide())
        on.exit(waiter3$hide())
        on.exit(waiter4$hide())
        
        google  <- read.csv("google_pull_main.csv", encoding = "UTF-8")
        google  <- google %>% mutate(date = as.Date(date))
        names(google) <- c("date", "score", "is_partial")
        g <- google %>% 
            ggplot(aes(x = date, y = score)) + 
            geom_line(size = 1.25,  linetype = 1) + 
            geom_point(size = 2, colour = "red", fill = "black", alpha = 0.9) + 
            my_theme + 
            labs(
                title    = paste0(input$google_search," ","Trend Line"),
                x        = "",
                y        = "Score")
        g
        
    })
    
    out_g2 <- eventReactive(input$input_action3,{
        
        google_csv(paste0(input$google_search," ",input$sub_topic), "sub")
        message("complete")
        
        google  <- read.csv("google_pull_sub.csv", encoding = "UTF-8")
        google <- google %>% mutate(date = as.Date(date))
        names(google) <- c("date", "score", "is_partial")
        
        g <- google %>% 
            ggplot(aes(x = date, y = score)) + 
            geom_line(size = 1.25,  linetype = 1) + 
            geom_point(size = 2, colour = "red", fill = "black", alpha = 0.9) + 
            my_theme + 
            labs(
                title    = paste0(input$google_search," ",input$sub_topic," ","Trend Line"),
                x        = "",
                y        = "Score")
        g

        
    })
    
    out_box <- eventReactive(input$input_action3,{
        
        # Load filtered data
        reddit <- reddit_fc()
        twitter <- twitter_fc()
        
        # Boxplots
        t <- twitter %>% 
            ggplot(aes(y = sentiment)) + 
            geom_boxplot(fill = "Skyblue3", alpha = 0.6)  + 
            labs(
                x        = "Twitter",
                title = "Sentiment"
            ) + 
            my_theme + 
            theme(axis.text.x = element_blank()) + 
            ylim(-1,1)
        
        
        r <- reddit %>% 
            ggplot(aes(y = sentiment)) + 
            geom_boxplot(fill = "coral2", alpha = 0.6) + 
            labs(
                x    = "Reddit",
                y        = "",
                title = "Sentiment"
            ) + 
            my_theme + 
            theme(axis.text.x = element_blank()) + 
            ylim(-1,1)
        
        grid.arrange(r,t, ncol = 2)
    })
    
    out_scores <- eventReactive(input$input_action3,{

        # Data loading
        google1  <- read.csv("google_pull_main.csv", encoding = "UTF-8")
        
        if(is.null(input$sub_topic)){
            google2 <- read.csv("google_pull_main.csv", encoding = "UTF-8")
        } else {
            google2  <- read.csv("google_pull_sub.csv", encoding = "UTF-8")
        }
        
        twitter <- read.csv("twitter_pull.csv", encoding = "UTF-8")
        reddit  <- read.csv("reddit_pull.csv", encoding = "UTF-8")
        
        # Update date
        google1 <- google1 %>% mutate(date = as.Date(date))
        names(google1) <- c("date", "score", "is_partial")
        google2 <- google2 %>% mutate(date = as.Date(date))
        names(google2) <- c("date", "score", "is_partial")
        
        # Extract scores as vector
        google_vals1 <- google1[[2]]
        google_vals2 <- google2[[2]]
        
        # Score metrics
        google_mean_all1   <- google_vals1 %>% mean()
        google_mean_all2   <- google_vals2 %>% mean()
        
        google_mean_last_121   <- google_vals1 %>% tail(12) %>% mean()
        google_mean_last_122   <- google_vals2 %>% tail(12) %>% mean()
        
        google_scores <- c(google_mean_all1, google_mean_all2,
                           google_mean_last_121, google_mean_last_122) %>% round(2)
        
        # Load filtered data
        reddit <- reddit_fc()
        twitter <- twitter_fc()
        
        r_mean <- mean(reddit$sentiment)
        r_mentions <- length(reddit$sentiment)
        t_mean <- mean(twitter$sentiment)
        t_mentions <- length(twitter$sentiment)
        current_google <- tail(google2,1)[[2]]
        
        # Internal Metric
        reddit_twitter_total <- r_mentions + t_mentions
        clout <- current_google + (r_mentions * r_mean) + (t_mentions * t_mean)
        
        subtopic_scores <- data.frame(source  = c("Reddit", "Twitter"),
                                      scores = c((r_mentions * r_mean) / reddit_twitter_total,
                                                 (t_mentions * t_mean) / reddit_twitter_total))
        
        ratio_scores <- data.frame(source = c("Reddit", "Twitter"), 
                                   scores = c(r_mentions / reddit_twitter_total,
                                              t_mentions / reddit_twitter_total))
        
        clout_df <- data.frame(scores = clout) %>% round(1) 
        
        # Subtopic Plots
        st_1 <- ratio_scores %>% 
            ggplot(aes(fill = source, x = 0, y = scores)) + 
            geom_bar(position="stack", stat="identity", show.legend = TRUE, 
                     colour = "black") + 
            my_theme + 
            theme(axis.text.x = element_blank(),
                  legend.position = "bottom",
                  legend.title = element_blank()) + 
            ylim(0,1) + 
            labs(
                title    = "Source Ratio",
                subtitle = "",
                caption  = "",
                x        = "",
                y        = "Scores",
                col      = ""
            )
        
        st_2 <- clout_df %>% 
            ggplot(aes(x = 0, y = scores)) + 
            geom_col(colour= "black", fill = "Aquamarine3",alpha = .9) + 
            geom_label(aes(label = round(clout_df, 2)),
                       fill = "white", colour = "Skyblue4", vjust = 3) +
            my_theme + 
            theme(axis.text.x = element_blank()) + 
            labs(
                title    = paste0("Clout: ", clout %>% round(2)),
                subtitle = "",
                caption  = "",
                x        = "",
                y        = "",
                col      = ""
            )
        
        grid.arrange(st_1, st_2, ncol = 2)
    })
    
    out_dt <- eventReactive(input$input_action3,{
        
        # Data loading
        google1  <- read.csv("google_pull_main.csv", encoding = "UTF-8")
        
        if(is.null(input$sub_topic)){
            google2 <- read.csv("google_pull_main.csv", encoding = "UTF-8")
        } else {
            google2  <- read.csv("google_pull_sub.csv", encoding = "UTF-8")
        }
        
        twitter <- read.csv("twitter_pull.csv", encoding = "UTF-8")
        reddit  <- read.csv("reddit_pull.csv", encoding = "UTF-8")
        
        # Update date
        google1 <- google1 %>% mutate(date = as.Date(date))
        names(google1) <- c("date", "score", "is_partial")
        google2 <- google2 %>% mutate(date = as.Date(date))
        names(google2) <- c("date", "score", "is_partial")
        
        # Extract scores as vector
        google_vals1 <- google1[[2]]
        google_vals2 <- google2[[2]]
        
        # Score metrics
        google_mean_all1   <- google_vals1 %>% mean()
        google_mean_all2   <- google_vals2 %>% mean()
        
        google_mean_last_121   <- google_vals1 %>% tail(12) %>% mean()
        google_mean_last_122   <- google_vals2 %>% tail(12) %>% mean()
        
        google_scores <- c(google_mean_all1, google_mean_all2,
                           google_mean_last_121, google_mean_last_122) %>% round(2)
        
        reddit <- reddit_fc()
        twitter <- twitter_fc()
        
        r_mean <- mean(reddit$sentiment)
        r_mentions <- length(reddit$sentiment)
        t_mean <- mean(twitter$sentiment)
        t_mentions <- length(twitter$sentiment)

        # Data Table
        table_data <- c(google_scores,  r_mentions, (r_mentions / reddit_master_length), 
                        r_mean, t_mentions, (t_mentions / twitter_master_length), t_mean) %>% round(2)
        gtable <- data.frame("Scores" = table_data)
        rownames(gtable) <- c("Main Mean Trend All Time", "Sub Mean Trend All Time",
                              "Main Mean Trend Last Year", "Sub Mean Trend Last Year", 
                              "Reddit Mention Count", "Reddit Mention Ratio",
                              "Reddit Mean Sentiment", "Twitter Mentions Count", 
                              "Twitter Mention Ratio", "Twitter Mean Sentiment")
        datatable(gtable, options = list(dom = 't')) 
    })
    
    output$Trend1 <- renderPlot(out_g()) 
    output$Trend2 <- renderPlot(out_g2()) 
    output$Box    <- renderPlot(out_box())
    output$Scores <- renderPlot({out_scores()})
    output$gtrendstable <- renderDataTable(out_dt())

}

# Run the application 
shinyApp(ui = ui, server = server)
