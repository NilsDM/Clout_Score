# Load cleaned data sets
source("Data_Cleaning_and_Sentiment.R")

# Libraries
library(DT)
library(ggplot2)
library(forcats)
library(cowplot)
library(showtext)
library(gridExtra)

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

# Load theme config
load_theme()

# Google Visualization
g <- google %>% 
    ggplot(aes(x = date, y = score)) + 
    geom_line(size = 1.25,  linetype = 1) + 
    geom_point(size = 2, colour = "red", fill = "black", alpha = 0.9) + 
    my_theme + 
    labs(
        title    = "Google Trend Line",
        y        = "Score")

# Boxplots
t <- twitter %>% 
    ggplot(aes(y = sentiment)) + 
    geom_boxplot(fill = "Skyblue3", alpha = 0.6)  + 
    labs(
        x        = "Twitter",
        title = "Sentiment Distribution"
    )


r <- reddit %>% 
    ggplot(aes(y = sentiment)) + 
    geom_boxplot(fill = "coral2", alpha = 0.6) + 
    labs(
        x    = "Reddit",
        y        = "",
        title = "Sentiment Distribution"
    )

### Subtopic

# Reddit
r_sub_topic <- reddit %>% 
    filter(grepl("Pam", comment))
r_mean <- mean(r_sub_topic$sentiment)
r_mentions <- length(r_sub_topic$sentiment)

# Twitter
t_sub_topic <- twitter %>% 
    filter(grepl("Pam", text))
t_mean <- mean(t_sub_topic$sentiment)
t_mentions <- length(t_sub_topic$sentiment)

# Google
current_google <- tail(google,1)[[2]]

# Internal Metric
reddit_twitter_total <- (r_mentions * r_mean) + (t_mentions * t_mean)
trend_aggregation <- (current_google / 100) + reddit_twitter_total

subtopic_scores <- data.frame(source  = c("Reddit", "Twitter"),
                              scores = c((r_mentions * r_mean) / reddit_twitter_total,
                                         (t_mentions * t_mean) / reddit_twitter_total))

trend_agg_df <- data.frame(scores = trend_aggregation) 

# Subtopic Plots
st_1 <- subtopic_scores %>% 
    ggplot(aes(fill = source, x = 0, y = scores)) + 
    geom_bar(position="stack", stat="identity", show.legend = FALSE, 
             colour = "black") + 
    my_theme + 
    theme(axis.text.x = element_blank()) + 
    ylim(0,2) + 
    geom_label(aes(label = source), fill = "white", colour = "Skyblue4",
               position = position_dodge(width = 2), vjust = 1.2) +
    labs(
        title    = "Clout Sources Ratio",
        subtitle = "",
        caption  = "",
        x        = "",
        y        = "Scores",
        col      = ""
    )

st_2 <- trend_agg_df %>% 
    ggplot(aes(x = 0, y = scores)) + 
        geom_col(colour= "black", fill = "Aquamarine3",alpha = .9) + 
        geom_label(aes(label = round(trend_agg_df,5)), 
                   fill = "white", colour = "Skyblue4",
               position = position_dodge(width = 2), vjust = 3) + 
        my_theme + 
        theme(axis.text.x = element_blank()) + 
        ylim(0,(trend_aggregation + 10)) + 
        labs(
            title    = "Clout Score",
            subtitle = "",
            caption  = "",
            x        = "",
            y        = "",
            col      = ""
        )

# Data Table
table_data <- c(google_scores,  c(r_mentions, r_mean, t_mentions, t_mean)) %>% round(2)
gtable <- data.frame("scores" = table_data)
rownames(gtable) <- c("Mean Trend All Time", "Mean Trend Last Year",
                      "Median Trend All Time", "Median Trend Last Year", 
                      "Reddit Mention Count", "Mean Reddit Sentiment",
                      "Twitter Mentions Count", "Mean Twitter Sentiment")
dt_gtable <- datatable(gtable) 
    
